# Reprocess already-scored rubric-3 AI reviews that the (updated)
# dbCompExtractionCheckConflicts() now flags for a rule-2 violation the old
# checker missed (a quote that is a verbatim substring of another
# competency's quote, with the nested copy left unlocated).
#
# For each selected review: statusCode 5 -> 6, then resolve (batch) ->
# re-score (batch). Extraction is NOT re-run - the extracted competencies and
# evidence text are kept, only the overlap is reassigned/trimmed and the
# review is re-scored against the corrected extraction set.
#
# Usage:  Rscript dev/reprocess_rule2_conflicts.R [N]        (default N = 100)
#
# Resumable: progress is checkpointed to STATE_FILE after every batch step.
# Delete STATE_FILE to start over. PushOver fires after every step.

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# ── Arguments ────────────────────────────────────────────────────────────────
db_path     <- "local/narrate.db"
reviewer_id <- 1L
rubric_id   <- 3L
.args       <- as.integer(commandArgs(trailingOnly = TRUE))
N           <- if (length(.args) >= 1 && !is.na(.args[1])) .args[1] else 100L

STATE_FILE   <- file.path(
  "/tmp/claude-1000/-home-pj-LocalGit-CFME/7051cbef-b081-4ef0-bec7-09b9063df493/scratchpad",
  sprintf("reprocess_rule2_state_%d.json", N)
)
POLL_SEC     <- 60
MAX_WAIT_SEC <- 6 * 3600

Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn(db_path)

log <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
save_state <- function(s) jsonlite::write_json(s, STATE_FILE, auto_unbox = TRUE, pretty = TRUE)

.push_auth <- jsonlite::fromJSON(keyring::key_get("PUSHOVER_API", "default"))
push <- function(msg) {
  log("PUSHOVER:", msg)
  try(
    httr2::request(.push_auth$url) |>
      httr2::req_body_form(
        token = .push_auth$key, user = .push_auth$user,
        title = "rule-2 reprocess", message = msg
      ) |>
      httr2::req_perform(),
    silent = TRUE
  )
  invisible(NULL)
}

status_summary <- function(ids) {
  tbl(conn, "review_assignment") |>
    filter(id %in% local(ids)) |>
    count(statusCode) |> collect() |> arrange(statusCode)
}
fmt_summary <- function(ids) {
  s <- status_summary(ids)
  paste(sprintf("%s:%d", s$statusCode, s$n), collapse = " ")
}

wait_for_batch <- function(batch_id) {
  t0 <- Sys.time()
  repeat {
    st <- llm_batch_status(batch_id, conn)$statusCode
    log(sprintf("  batch %s statusCode = %s", batch_id, st))
    if (st == 3) return(TRUE)
    if (st < 0) { push(sprintf("batch %s FAILED (statusCode %s) - aborting", batch_id, st)); return(FALSE) }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > MAX_WAIT_SEC) {
      push(sprintf("batch %s local wait timed out - aborting", batch_id)); return(FALSE)
    }
    Sys.sleep(POLL_SEC)
  }
}

# ── State: fresh vs resume ───────────────────────────────────────────────────
STAGES <- c("selected", "resolve_submitted", "resolve_loop",
            "score_submitted", "done")

if (file.exists(STATE_FILE)) {
  state <- jsonlite::read_json(STATE_FILE, simplifyVector = TRUE)
  log("Resuming at stage:", state$stage, "| reviews:", length(state$review_ids))
} else {
  candidates <- tbl(conn, "review_assignment") |>
    filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id),
           statusCode == 5L) |>
    pull(id) |> sort()
  log("Scanning", length(candidates), "scored rubric-3 AI reviews for conflicts...")

  flagged <- Filter(
    function(rid) isTRUE(dbCompExtractionCheckConflicts(conn, rid)$has_conflicts),
    candidates
  )
  log(length(flagged), "flagged;   taking first", N)
  ids <- head(flagged, N)

  # Reset to 'extraction conflict pending' and clear the prior review scores
  DBI::dbExecute(conn, sprintf(
    "UPDATE review_assignment
        SET statusCode = 6, note = NULL,
            utility_score_value = NULL, utility_score_id = NULL,
            sentiment_score_value = NULL, sentiment_score_id = NULL,
            modified = datetime('now')
      WHERE id IN (%s)", paste(ids, collapse = ",")))
  log("reset", length(ids), "reviews to statusCode 6")

  state <- list(db_path = db_path, review_ids = ids, stage = "selected",
                resolve_round = 0L)
  save_state(state)
  push(sprintf("selected & reset %d reviews (ids %d-%d). Starting resolve.",
               length(ids), min(ids), max(ids)))
}
rids <- state$review_ids
set_stage <- function(s) { state$stage <<- s; save_state(state) }

# ── STEP 1b — resolve rule-2 conflicts (<= 2 rounds, then -4) ────────────────
if (state$stage == "resolve_submitted") {
  if (!wait_for_batch(state$resolve_batch_id)) quit(save = "no", status = 1)
  batch_resolve_process(state$resolve_batch_id, conn)
  push(sprintf("resolve round %d processed. status now { %s }",
               state$resolve_round, fmt_summary(rids)))
  set_stage("resolve_loop")
}

if (state$stage %in% c("selected", "resolve_loop")) {
  repeat {
    conflict_ids <- tbl(conn, "review_assignment") |>
      filter(id %in% local(rids), statusCode == 6L) |> pull(id)
    if (length(conflict_ids) == 0) { log("no reviews left at statusCode 6"); break }

    state$resolve_round <- state$resolve_round + 1L
    log(sprintf("resolve round %d: submitting %d reviews",
                state$resolve_round, length(conflict_ids)))
    b <- llm_comp_resolve_batch_submit(conn, conflict_ids, verbose = TRUE)
    state$resolve_batch_id <- b$id
    set_stage("resolve_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    push(sprintf("resolve round %d batch %s submitted (%d reviews)",
                 state$resolve_round, b$id, length(conflict_ids)))

    if (!wait_for_batch(b$id)) quit(save = "no", status = 1)
    batch_resolve_process(b$id, conn)
    push(sprintf("resolve round %d processed. status now { %s }",
                 state$resolve_round, fmt_summary(rids)))
    set_stage("resolve_loop")
  }
  set_stage("resolve_done")
}

# ── STEP 2 — re-score ───────────────────────────────────────────────────────
if (state$stage == "score_submitted") {
  if (!wait_for_batch(state$score_batch_id)) quit(save = "no", status = 1)
  batch_score_process(state$score_batch_id, conn)
  set_stage("done")
}

if (!identical(state$stage, "done")) {
  ready <- tbl(conn, "review_assignment") |>
    filter(id %in% local(rids), statusCode == 3L) |> pull(id)
  if (length(ready) > 0) {
    log("scoring: submitting", length(ready), "reviews")
    b <- llm_comp_score_batch_submit(conn, ready, verbose = TRUE)
    state$score_batch_id <- b$id
    set_stage("score_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    push(sprintf("score batch %s submitted (%d reviews)", b$id, length(ready)))
    if (!wait_for_batch(b$id)) quit(save = "no", status = 1)
    batch_score_process(b$id, conn)
    set_stage("done")
  } else {
    log("nothing at statusCode 3 to score")
    set_stage("done")
  }
}

# ── Summary ─────────────────────────────────────────────────────────────────
final <- status_summary(rids)
log("REPROCESS COMPLETE - final statusCode breakdown:")
print(as.data.frame(final))
stuck <- tbl(conn, "review_assignment") |>
  filter(id %in% local(rids), statusCode != 5L) |>
  select(id, statusCode) |> collect()
n_done <- sum(final$n[final$statusCode == 5L])
still_conf <- sum(vapply(rids, function(r)
  isTRUE(dbCompExtractionCheckConflicts(conn, r)$has_conflicts), logical(1)))

push(sprintf(
  "DONE. %d/%d at statusCode 5. %d not complete. %d still have a conflict. { %s }",
  n_done, length(rids), nrow(stuck), still_conf, fmt_summary(rids)
))
log("stuck (not statusCode 5):"); print(as.data.frame(stuck))

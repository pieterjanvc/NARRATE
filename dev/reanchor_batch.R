# Phase 2-4 of dev/paraphrase_fix_plan.md - re-anchor every paraphrased /
# unplaced AI competency quote in the rubric-3 review set, then resolve any
# rule-2 conflict a re-anchor created and re-score every touched review.
#
# Pipeline per review (only reviews actually touched enter the later stages):
#   statusCode 5  --reanchor batch-->  3 (clean) | 6 (conflict) | 5 (unchanged)
#   statusCode 6  --resolve batch (<=2 rounds)-->  3 | -4
#   statusCode 3  --score batch-->  5
#
# Mirrors dev/reprocess_rule2_conflicts.R: resumable via STATE_FILE, PushOver
# after every step, safe to re-run.
#
# Usage:  Rscript dev/reanchor_batch.R [N]        (default N = all)

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# ── Arguments ────────────────────────────────────────────────────────────────
db_path     <- Sys.getenv("REANCHOR_DB", "local/narrate.db")
reviewer_id <- 1L
rubric_id   <- 3L
.args       <- as.integer(commandArgs(trailingOnly = TRUE))
N           <- if (length(.args) >= 1 && !is.na(.args[1])) .args[1] else Inf

STATE_FILE <- file.path(
  Sys.getenv("CLAUDE_SCRATCH", tempdir()),
  "reanchor_batch_state.json"
)
POLL_SEC     <- 60
MAX_WAIT_SEC <- 6 * 3600

Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn(db_path)

log        <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
save_state <- function(s) jsonlite::write_json(s, STATE_FILE, auto_unbox = TRUE, pretty = TRUE)

.push_auth <- tryCatch(jsonlite::fromJSON(keyring::key_get("PUSHOVER_API", "default")), error = function(e) NULL)
push <- function(msg) {
  log("PUSHOVER:", msg)
  if (is.null(.push_auth)) return(invisible(NULL))
  try(
    httr2::request(.push_auth$url) |>
      httr2::req_body_form(
        token = .push_auth$key, user = .push_auth$user,
        title = "reanchor batch", message = msg
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

na_row_reviews <- function(ids) {
  tbl(conn, "competency_text") |>
    filter(is.na(start)) |>
    inner_join(
      tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
      by = "competency_score_id"
    ) |>
    filter(review_assignment_id %in% local(ids)) |>
    distinct(review_assignment_id) |>
    pull(review_assignment_id)
}

# ── State: fresh vs resume ───────────────────────────────────────────────────
if (file.exists(STATE_FILE)) {
  state <- jsonlite::read_json(STATE_FILE, simplifyVector = TRUE)
  log("Resuming at stage:", state$stage, "| reviews:", length(state$review_ids))
} else {
  candidates <- tbl(conn, "review_assignment") |>
    filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id), statusCode == 5L) |>
    pull(id) |> sort()
  ids <- na_row_reviews(candidates) |> sort()
  ids <- head(ids, N)
  log(length(ids), "rubric-3 AI reviews with >=1 unplaced competency quote")

  bak <- file.path("local/backup", sprintf("narrate_pre-reanchor_%s.db", format(Sys.time(), "%Y%m%d-%H%M%S")))
  stopifnot(file.copy(db_path, bak))
  log("backup ->", bak)

  b <- llm_comp_reanchor_batch_submit(conn, ids, verbose = TRUE)
  state <- list(db_path = db_path, review_ids = ids, backup = bak,
                stage = "reanchor_submitted", reanchor_batch_id = b$id, resolve_round = 0L)
  save_state(state)
  batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
  push(sprintf("reanchor batch %s submitted (%d reviews)", b$id, length(ids)))
}
rids <- state$review_ids
set_stage <- function(s) { state$stage <<- s; save_state(state) }

# ── STEP 1 — re-anchor ──────────────────────────────────────────────────────
if (state$stage == "reanchor_submitted") {
  if (!wait_for_batch(state$reanchor_batch_id)) quit(save = "no", status = 1)
  batch_reanchor_process(state$reanchor_batch_id, conn)
  push(sprintf("reanchor processed. status now { %s }", fmt_summary(rids)))
  set_stage("resolve_loop")
}

# ── STEP 2 — resolve rule-2 conflicts a re-anchor created (<= 2 rounds) ──────
if (state$stage == "resolve_submitted") {
  if (!wait_for_batch(state$resolve_batch_id)) quit(save = "no", status = 1)
  batch_resolve_process(state$resolve_batch_id, conn)
  push(sprintf("resolve round %d processed. status now { %s }", state$resolve_round, fmt_summary(rids)))
  set_stage("resolve_loop")
}

if (state$stage == "resolve_loop") {
  repeat {
    conflict_ids <- tbl(conn, "review_assignment") |>
      filter(id %in% local(rids), statusCode == 6L) |> pull(id)
    if (length(conflict_ids) == 0) { log("no reviews left at statusCode 6"); break }

    state$resolve_round <- state$resolve_round + 1L
    log(sprintf("resolve round %d: submitting %d reviews", state$resolve_round, length(conflict_ids)))
    b <- llm_comp_resolve_batch_submit(conn, conflict_ids, verbose = TRUE)
    state$resolve_batch_id <- b$id
    set_stage("resolve_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    push(sprintf("resolve round %d batch %s submitted (%d reviews)", state$resolve_round, b$id, length(conflict_ids)))

    if (!wait_for_batch(b$id)) quit(save = "no", status = 1)
    batch_resolve_process(b$id, conn)
    push(sprintf("resolve round %d processed. status now { %s }", state$resolve_round, fmt_summary(rids)))
  }
  set_stage("score_pending")
}

# ── STEP 3 — re-score every touched review ──────────────────────────────────
if (state$stage == "score_submitted") {
  if (!wait_for_batch(state$score_batch_id)) quit(save = "no", status = 1)
  batch_score_process(state$score_batch_id, conn)
  set_stage("done")
}

if (state$stage == "score_pending") {
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
log("REANCHOR PIPELINE COMPLETE - final statusCode breakdown:")
print(as.data.frame(final))

flagged <- tbl(conn, "competency_text") |>
  filter(locate_status == "unlocated") |>
  inner_join(
    tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  filter(review_assignment_id %in% local(rids)) |>
  tally() |> pull(n)
still_na <- tbl(conn, "competency_text") |>
  filter(is.na(start), is.na(locate_status)) |>
  inner_join(
    tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  filter(review_assignment_id %in% local(rids)) |>
  tally() |> pull(n)

push(sprintf(
  "DONE. %d reviews. %d quotes flagged 'unlocated' for human review, %d still unplaced+unflagged. { %s }",
  length(rids), flagged, still_na, fmt_summary(rids)
))
log(sprintf("flagged 'unlocated': %d   |   unplaced + unflagged: %d", flagged, still_na))

dbFinish(conn)

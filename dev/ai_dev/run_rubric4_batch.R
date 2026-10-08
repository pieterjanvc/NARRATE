# Run the AI batch review pipeline for the 60 rubric-4 AI review assignments
# created by dev/ai_dev/copy_rubric3_to_4.R (reviewer 1, rubric 4, status 0).
# Steps: extract -> retry extraction failures once -> resolve rule-2 conflicts
# -> score. All 60 are submitted in a single batch per step.
#
# Adapted from dev/run_rubric3_batch.R; the only difference is that the review
# assignments already exist, so nothing is created here.
#
# Usage:  Rscript dev/ai_dev/run_rubric4_batch.R
#
# Resumable: progress is checkpointed to STATE_FILE after every batch step, so
# re-running picks up where it left off. Delete STATE_FILE to start over.
#
# PushOver: batch_status_notify() fires a phone notification when each batch
# settles (extraction, every conflict-resolve round, scoring).

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# ── Settings ─────────────────────────────────────────────────────────────────
db_path      <- "local/narrate.db"
reviewer_id  <- 1L                       # AI reviewer
rubric_id    <- 4L
n_expected   <- 60L

STATE_FILE   <- "dev/ai_dev/rubric4_batch_state.json"
POLL_SEC     <- 60
MAX_WAIT_SEC <- 6 * 3600

Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn(db_path)

log <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
save_state  <- function(s) jsonlite::write_json(s, STATE_FILE, auto_unbox = TRUE, pretty = TRUE)

status_of <- function(ids) {
  tbl(conn, "review_assignment") |>
    filter(id %in% local(ids)) |>
    select(id, evaluation_id, statusCode) |>
    collect() |>
    arrange(id)
}
status_summary <- function(ids) {
  status_of(ids) |> count(statusCode) |> arrange(statusCode)
}

wait_for_batch <- function(batch_id) {
  t0 <- Sys.time()
  repeat {
    st <- llm_batch_status(batch_id, conn)$statusCode
    log(sprintf("  batch %s statusCode = %s", batch_id, st))
    if (st == 3) return(TRUE)
    if (st < 0) { log("  batch did NOT complete - aborting"); return(FALSE) }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > MAX_WAIT_SEC) {
      log("  local wait timed out - aborting"); return(FALSE)
    }
    Sys.sleep(POLL_SEC)
  }
}

# ── State: fresh vs resume ───────────────────────────────────────────────────
if (file.exists(STATE_FILE)) {
  state <- jsonlite::read_json(STATE_FILE, simplifyVector = TRUE)
  log("Resuming from state:", state$stage, "| reviews:", length(state$review_ids))
} else {
  assigned <- tbl(conn, "review_assignment") |>
    filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id)) |>
    select(id, statusCode) |>
    collect()
  if (nrow(assigned) != n_expected || any(assigned$statusCode != 0L)) {
    print(count(assigned, statusCode))
    stop("Expected ", n_expected, " rubric-", rubric_id,
         " AI assignments all at statusCode 0")
  }
  review_ids <- sort(assigned$id)
  log("Found", length(review_ids), "rubric", rubric_id, "AI assignments, ids",
      min(review_ids), "-", max(review_ids))

  # Backup before the first write
  backup_path <- file.path(
    "local/backup",
    sprintf("%s_pre-rubric4-ai_%s.db",
            tools::file_path_sans_ext(basename(db_path)),
            format(Sys.time(), "%Y%m%d-%H%M%S"))
  )
  dir.create(dirname(backup_path), showWarnings = FALSE, recursive = TRUE)
  stopifnot(file.copy(db_path, backup_path))
  log("Backup written to", backup_path)

  state <- list(db_path = db_path, review_ids = review_ids, stage = "assigned")
  save_state(state)
}
rids <- state$review_ids

# Ordered pipeline stages. Every transition only moves forward: helpers below
# guard each phase so a resume (e.g. at "score_submitted") never re-enters and
# clobbers an earlier phase's state.
STAGES    <- c("assigned", "extract_submitted", "extract_done",
               "extract_retry_submitted", "extract_retried",
               "resolve_submitted", "resolve_done", "score_submitted", "done")
stage_idx <- function(s) match(s, STAGES)
before    <- function(target) stage_idx(state$stage) < stage_idx(target)
set_stage <- function(s) { state$stage <<- s; save_state(state) }

# ── STEP 1 — competency extraction ───────────────────────────────────────────
if (state$stage == "assigned") {
  new_ids <- tbl(conn, "review_assignment") |>
    filter(id %in% local(rids), statusCode == 0L) |> pull(id)
  log("STEP 1 extraction: submitting", length(new_ids), "reviews")
  b <- llm_comp_extract_batch_submit(conn, new_ids, verbose = TRUE)
  state$extract_batch_id <- b$id; set_stage("extract_submitted")
  batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
  log("  batch", b$id, "submitted, PushOver monitor launched")
}
if (state$stage == "extract_submitted") {
  if (!wait_for_batch(state$extract_batch_id)) quit(save = "no", status = 1)
  log("STEP 1 extraction: processing results")
  batch_extract_process(state$extract_batch_id, conn)
  print(status_summary(rids))
  set_stage("extract_done")
}

# ── STEP 1c — retry extraction failures once ─────────────────────────────────
# A statusCode -2 is usually a transient model failure (truncated / looping
# output); the batch is non-deterministic so a plain resubmit almost always
# clears it. <=5 failures go real-time to skip the batch turnaround.
if (state$stage == "extract_retry_submitted") {
  if (!wait_for_batch(state$retry_batch_id)) quit(save = "no", status = 1)
  batch_extract_process(state$retry_batch_id, conn)
  print(status_summary(rids))
  set_stage("extract_retried")
}
if (state$stage == "extract_done") {
  failed <- tbl(conn, "review_assignment") |>
    filter(id %in% local(rids), statusCode == -2L) |> pull(id)
  if (length(failed) == 0) {
    log("STEP 1c: no extraction failures to retry")
  } else if (length(failed) <= 5) {
    log("STEP 1c: retrying", length(failed), "failure(s) real-time:",
        paste(failed, collapse = ", "))
    llm_comp_extract_run(conn, review_ids = failed, verbose = TRUE, force = TRUE)
    print(status_summary(rids))
  } else {
    log("STEP 1c: retrying", length(failed), "failures via batch")
    b <- llm_comp_extract_batch_submit(conn, failed, verbose = TRUE, force = TRUE)
    state$retry_batch_id <- b$id; set_stage("extract_retry_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    if (!wait_for_batch(b$id)) quit(save = "no", status = 1)
    batch_extract_process(b$id, conn)
    print(status_summary(rids))
  }
  set_stage("extract_retried")
}

# ── STEP 1b — resolve rule-2 extraction conflicts (may take several rounds) ───
# Finish a resolve batch that was already in flight when a previous run stopped.
if (state$stage == "resolve_submitted") {
  if (!wait_for_batch(state$resolve_batch_id)) quit(save = "no", status = 1)
  batch_resolve_process(state$resolve_batch_id, conn)
  print(status_summary(rids))
  set_stage("extract_retried")   # back to loop-eligible
}
if (before("score_submitted")) {
  repeat {
    conflict_ids <- tbl(conn, "review_assignment") |>
      filter(id %in% local(rids), statusCode == 6L) |> pull(id)
    if (length(conflict_ids) == 0) { log("STEP 1b: no reviews at statusCode 6"); break }
    log("STEP 1b: resolving", length(conflict_ids), "conflicted reviews")
    b <- llm_comp_resolve_batch_submit(conn, conflict_ids)
    state$resolve_batch_id <- b$id; set_stage("resolve_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    if (!wait_for_batch(b$id)) quit(save = "no", status = 1)
    batch_resolve_process(b$id, conn)
    print(status_summary(rids))
    set_stage("extract_retried")
  }
  if (before("resolve_done")) set_stage("resolve_done")
}

# ── STEP 2 — competency scoring ──────────────────────────────────────────────
if (state$stage == "resolve_done") {
  ready <- tbl(conn, "review_assignment") |>
    filter(id %in% local(rids), statusCode == 3L) |> pull(id)
  if (length(ready) > 0) {
    log("STEP 2 scoring: submitting", length(ready), "reviews")
    b <- llm_comp_score_batch_submit(conn, ready, verbose = TRUE)
    state$score_batch_id <- b$id; set_stage("score_submitted")
    batch_status_notify(b$id, db_path, feq_sec = POLL_SEC, max_wait = MAX_WAIT_SEC)
    log("  batch", b$id, "submitted, PushOver monitor launched")
  } else {
    log("STEP 2 scoring: nothing at statusCode 3 to score")
    set_stage("done")
  }
}
if (state$stage == "score_submitted") {
  if (!wait_for_batch(state$score_batch_id)) quit(save = "no", status = 1)
  log("STEP 2 scoring: processing results")
  batch_score_process(state$score_batch_id, conn)
  set_stage("done")
}

# ── Summary ──────────────────────────────────────────────────────────────────
log("PIPELINE COMPLETE - final statusCode breakdown:")
print(status_summary(rids))
codes <- status_codes(conn, "review_assignment")
stuck <- status_of(rids) |> filter(statusCode != 5L)
if (nrow(stuck) > 0) {
  log(nrow(stuck), "review(s) not at statusCode 5 (scoring complete):")
  print(stuck |> left_join(codes |> select(statusCode = code, description), by = "statusCode"))
} else {
  log("All", length(rids), "reviews complete (statusCode 5)")
}

dbFinish(conn)

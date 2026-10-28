# Run the AI batch review pipeline for every formative evaluation
# (summary_flg = 0; summative ones are excluded) that does not yet have a
# review by `ai_model` under `rubric_id` (initially: the 1,938 formative
# evaluations outside the 60-evaluation rubric 5 set that gpt-6-luna reviewed
# live).
# Steps: extract -> retry extraction failures -> resolve rule-2 conflicts
# -> score. Large steps are split into chunks of `chunk_size` that run in
# parallel.
#
# Usage:  Rscript dev/ai_dev/run_rubric5_batch.R
#
# Check-and-exit: every run does whatever is possible right now and then quits.
#   1. creates missing review assignments (first run only, after a DB backup)
#   2. checks all open batches; completed ones are processed
#   3. submits the next step for every review that is ready for it
#   4. prints progress
# Just rerun the same command to check progress / advance the pipeline. There
# is no state file: progress is derived from review_assignment.statusCode and
# the batch / batch_review tables, so nothing is ever started over.
#
# PushOver: batch_status_notify() launches a background monitor per submitted
# batch that sends a phone notification when it settles (= time to rerun).
# Monitors die with a reboot / shutdown; a rerun relaunches one for every batch
# still running that has none. A final notification is sent by the run that
# completes the pipeline.

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# ── Settings ─────────────────────────────────────────────────────────────────
rubric_id <- 5L
ai_model <- "gpt-6-luna" # must match reviewer.model of an existing AI reviewer

db_path <- "local/narrate.db"
chunk_size <- 700L # max reviews per batch, chunks are submitted in parallel
max_extract_attempts <- 2L # extraction tries per review before leaving it at -2
notify_max_wait <- 26 * 3600 # PushOver monitor gives up after this (24h window)

Sys.setenv(HUIT_API_NARRATE = keyring::key_get("HUIT_API_NARRATE"))
conn <- dbGetConn(db_path)

log <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

push <- function(message) {
  auth <- keyring::key_get("PUSHOVER_API", "default") |> jsonlite::fromJSON()
  try(
    httr2::request(auth$url) |>
      httr2::req_body_form(token = auth$key, user = auth$user, message = message) |>
      httr2::req_perform(),
    silent = TRUE
  )
}

# ── Checks ───────────────────────────────────────────────────────────────────
reviewer_id <- tbl(conn, "reviewer") |>
  filter(model == local(ai_model), human == 0) |>
  pull(id)
if (length(reviewer_id) != 1) {
  stop(
    "Expected exactly one AI reviewer with model '", ai_model, "', found ",
    length(reviewer_id), ". Create one with dbReviewerAI(conn, model = ...)"
  )
}

rubric <- tbl(conn, "rubric") |> filter(id == local(rubric_id)) |> collect()
if (nrow(rubric) != 1 || is.na(rubric$prompt_extract_id) ||
  is.na(rubric$prompt_score_id)) {
  stop("Rubric ", rubric_id, " missing or has no extract / score prompt")
}

log(sprintf(
  "Rubric %d | model %s (reviewer %d) | %s",
  rubric_id, ai_model, reviewer_id, db_path
))

# ── 1. Create missing review assignments ─────────────────────────────────────
done_evals <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id)) |>
  pull(evaluation_id)
formative_evals <- tbl(conn, "evaluation") |>
  filter(summary_flg == 0) |>
  pull(id)
new_evals <- setdiff(formative_evals, done_evals)

if (length(new_evals) > 0) {
  backup_path <- sprintf(
    "local/backup/narrate_pre_rubric%d_%s_batch_%s.db",
    rubric_id, ai_model, format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  dir.create(dirname(backup_path), showWarnings = FALSE)
  stopifnot(file.copy(db_path, backup_path))
  log("Backup written to", backup_path)

  dbReviewAssignment(
    conn,
    reviewer_id = reviewer_id,
    evaluation_id = new_evals,
    rubric_id = rubric_id,
    redacted = TRUE,
    include_questions = TRUE
  )
  log("Created", length(new_evals), "new review assignments")
}

# All reviews in scope (incl. ones finished earlier, e.g. the live run)
rids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id)) |>
  pull(id)

ids_at <- function(codes) {
  tbl(conn, "review_assignment") |>
    filter(id %in% local(rids), statusCode %in% local(codes)) |>
    pull(id)
}

# Batches holding any of our reviews, with their latest local status
our_batches <- function() {
  tbl(conn, "batch_review") |>
    filter(review_assignment_id %in% local(rids)) |>
    distinct(id = batch_id) |>
    inner_join(tbl(conn, "batch"), by = "id") |>
    select(id, created, statusCode, n_requests) |>
    collect() |>
    arrange(id)
}

# A batch's step follows from the "submitted" status its reviews still carry
submitted_status <- c(extract = 1L, score = 4L, resolve = 7L)
batch_step <- function(batch_id) {
  st <- tbl(conn, "batch_review") |>
    filter(batch_id == local(batch_id)) |>
    inner_join(
      tbl(conn, "review_assignment") |> select(id, statusCode),
      by = c("review_assignment_id" = "id")
    ) |>
    distinct(statusCode) |>
    pull(statusCode)
  names(submitted_status)[submitted_status %in% st][1]
}

submit_chunks <- function(step, ids, force = FALSE) {
  submit_fun <- switch(step,
    extract = llm_comp_extract_batch_submit,
    resolve = llm_comp_resolve_batch_submit,
    score = llm_comp_score_batch_submit
  )
  chunks <- split(sort(ids), ceiling(seq_along(ids) / chunk_size))
  for (chunk in chunks) {
    b <- submit_fun(conn, chunk, model = ai_model, verbose = TRUE, force = force)
    if (is.null(b)) next
    batch_status_notify(b$id, db_path, max_wait = notify_max_wait)
    log(sprintf("  %s batch %d submitted (%d reviews)", step, b$id, length(chunk)))
  }
}

# ── 2. Check open batches, process completed ones ────────────────────────────
n_processed <- 0L
failed_ids <- list()

for (bid in filter(our_batches(), statusCode %in% c(1, 2, 3))$id) {
  st <- llm_batch_status(bid, conn)$statusCode
  step <- batch_step(bid)

  if (is.na(step)) {
    log(sprintf("Batch %d: no reviews left at a submitted status - skipped", bid))
  } else if (st == 3) {
    log(sprintf("Batch %d (%s) completed - processing results", bid, step))
    switch(step,
      extract = batch_extract_process(bid, conn),
      resolve = batch_resolve_process(bid, conn),
      score = batch_score_process(bid, conn)
    )
    n_processed <- n_processed + 1L
  } else if (st < 0) {
    # Failed / expired / cancelled: its reviews are stuck at the submitted
    # status, so resubmit them for the same step below
    ids <- tbl(conn, "batch_review") |>
      filter(batch_id == local(bid)) |>
      pull(review_assignment_id) |>
      intersect(ids_at(submitted_status[[step]]))
    log(sprintf(
      "Batch %d (%s) did not complete (statusCode %d) - resubmitting %d reviews",
      bid, step, st, length(ids)
    ))
    failed_ids[[step]] <- c(failed_ids[[step]], ids)
  } else {
    # Monitors don't survive a reboot - relaunch one if none is running
    # (matches the script path batch_status_notify() starts Rscript with)
    monitor <- suppressWarnings(system2(
      "pgrep", c("-f", shQuote(sprintf("notify_%d_[0-9-]+\\.R", bid))),
      stdout = TRUE, stderr = FALSE
    ))
    if (length(monitor) == 0) {
      batch_status_notify(bid, db_path, max_wait = notify_max_wait)
      log(sprintf("Batch %d (%s) still running - PushOver monitor relaunched", bid, step))
    } else {
      log(sprintf("Batch %d (%s) still running", bid, step))
    }
  }
}

# ── 3. Submit the next step ──────────────────────────────────────────────────
for (step in names(failed_ids)) {
  submit_chunks(step, failed_ids[[step]], force = TRUE)
}

# Step 1 - extraction of new reviews
to_extract <- ids_at(0L)
if (length(to_extract) > 0) {
  log("STEP 1 extraction: submitting", length(to_extract), "reviews")
  submit_chunks("extract", to_extract)
}

# Step 1c - retry extraction failures. A -2 is usually a transient model
# failure (truncated / looping output) that a plain resubmit clears. A -2
# review never got past extraction, so its batch count = extraction attempts.
extract_failed <- ids_at(-2L)
if (length(extract_failed) > 0) {
  attempts <- tbl(conn, "batch_review") |>
    filter(review_assignment_id %in% local(extract_failed)) |>
    count(review_assignment_id) |>
    collect()
  to_retry <- extract_failed[
    coalesce(attempts$n[match(extract_failed, attempts$review_assignment_id)], 0L) <
      max_extract_attempts
  ]
  if (length(to_retry) > 0) {
    log("STEP 1c: retrying", length(to_retry), "extraction failures")
    submit_chunks("extract", to_retry, force = TRUE)
  }
}

# Step 1b - resolve rule-2 conflicts (batch_resolve_process() sends a review
# back to 6 for another round, or to -4 once attempts are exhausted)
to_resolve <- ids_at(6L)
if (length(to_resolve) > 0) {
  log("STEP 1b: resolving", length(to_resolve), "conflicted reviews")
  submit_chunks("resolve", to_resolve)
}

# Step 2 - scoring
to_score <- ids_at(3L)
if (length(to_score) > 0) {
  log("STEP 2 scoring: submitting", length(to_score), "reviews")
  submit_chunks("score", to_score)
}

# ── 4. Progress ──────────────────────────────────────────────────────────────
codes <- status_codes(conn, "review_assignment") |>
  select(statusCode = code, description)
progress <- tbl(conn, "review_assignment") |>
  filter(id %in% local(rids)) |>
  count(statusCode) |>
  collect() |>
  left_join(codes, by = "statusCode") |>
  arrange(statusCode)

open_batches <- filter(our_batches(), statusCode %in% c(1, 2, 3))

cat("\nReview status (", length(rids), " reviews):\n", sep = "")
print(as.data.frame(progress), row.names = FALSE)
if (nrow(open_batches) > 0) {
  cat("\nOpen batches (1 = submitted, 2 = in progress, 3 = completed):\n")
  print(as.data.frame(open_batches), row.names = FALSE)
  log("Rerun this script when PushOver reports the batches have finished")
} else {
  n_done <- sum(progress$n[progress$statusCode == 5])
  msg <- sprintf(
    "Rubric %d %s pipeline complete: %d / %d reviews scored",
    rubric_id, ai_model, n_done, length(rids)
  )
  log(msg)
  stuck <- filter(progress, statusCode != 5)
  if (nrow(stuck) > 0) {
    log("Reviews needing attention (-2 extraction failed, -3 scoring failed,",
        "-4 conflict unresolved):")
    print(as.data.frame(stuck), row.names = FALSE)
  }
  # Only notify from the run that actually finished the work
  if (n_processed > 0) push(msg)
}

dbFinish(conn)

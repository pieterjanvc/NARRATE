# ─── Full AI review pipeline ──────────────────────────────────────────────────
# Check-and-exit driver that takes a set of evaluations through the whole batch
# review pipeline (extract -> retry -> resolve -> score) across repeated calls,
# deriving progress from the database only (review_assignment.statusCode and
# the batch / batch_review tables), so it never starts over.

#' Run (or advance) the AI batch review pipeline
#'
#' Does whatever is possible right now and returns; call it again to check
#' progress and advance the pipeline. Each call:
#'   1. creates missing review assignments for the AI reviewer of `model`
#'      under `rubric_id`
#'   2. checks all open batches holding these reviews: completed ones are
#'      processed, failed / expired ones resubmitted, and running ones get a
#'      PushOver monitor if theirs died (e.g. after a reboot)
#'   3. submits the next step for every review that is ready for it:
#'      extraction (statusCode 0), an extraction retry (-2, up to
#'      `max_extract_attempts`), conflict resolution (6) and scoring (3).
#'      Steps are split into batches of `chunk_size` that run in parallel
#'   4. prints progress
#'   5. once no batches are open: reruns truncated reviews (-5 extraction /
#'      -6 scoring, output hit max_output_tokens) live with
#'      `live_max_output_tokens`, taking them through extract -> resolve ->
#'      score synchronously
#'
#' With `notify = TRUE`, every submitted batch gets a background monitor
#' (batch_status_notify()) that sends a PushOver notification when it settles,
#' i.e. when it is time to call this function again. The call that completes
#' the pipeline sends a final notification. Monitors run on this machine and
#' die with a reboot / shutdown; batches keep running regardless.
#'
#' The API key is taken from the HUIT_API_NARRATE environment variable or, if
#' that is not set, the keyring entry of the same name. Notifications need a
#' keyring entry PUSHOVER_API (see batch_status_notify()).
#'
#' @param db_path Path to the SQLite database
#' @param rubric_id Rubric to review under
#' @param model Model name; must match reviewer.model of an existing AI
#'   reviewer (create one with dbReviewerAI()). Default = llm_default_model
#' @param evaluation_ids Evaluations to review. Default = NULL: all formative
#'   evaluations (summary_flg = 0). Evaluations already reviewed by this
#'   reviewer under this rubric are kept as they are
#' @param chunk_size Max reviews per batch; chunks are submitted in parallel.
#'   Default = 700
#' @param max_extract_attempts Extraction tries per review before leaving it at
#'   statusCode -2. Truncated output (-5) is not retried in batch, see
#'   `live_fix_truncated`. Default = 2
#' @param live_fix_truncated Rerun truncated (-5 / -6) reviews live once no
#'   batches are open. Default = TRUE
#' @param live_fix_confirm Ask for confirmation on the console before the live
#'   rerun (no input / anything but y or yes skips it). Default = TRUE
#' @param live_max_output_tokens Output token cap for the live rerun of
#'   truncated reviews. Default = 8000
#' @param notify Send PushOver notifications. Default = TRUE
#' @param notify_max_wait Seconds after which a batch monitor gives up.
#'   Default = 26 hours (batches have a 24h completion window)
#' @param backup_dir Directory for the database backup made once per call,
#'   right before its first write (a call that only finds batches still
#'   running makes none). Default = "backup" next to the database
#'
#' @import dplyr
#' @returns Invisibly, a list with:
#'   - complete: TRUE if no batches are open (nothing left to wait for)
#'   - progress: review count per statusCode, with description
#'   - open_batches: batches still open (statusCode 1-3)
#' @export
llm_review_pipeline <- function(
  db_path,
  rubric_id,
  model = llm_default_model,
  evaluation_ids = NULL,
  chunk_size = 700L,
  max_extract_attempts = 2L,
  live_fix_truncated = TRUE,
  live_fix_confirm = TRUE,
  live_max_output_tokens = 8000L,
  notify = TRUE,
  notify_max_wait = 26 * 3600,
  backup_dir = file.path(dirname(db_path), "backup")
) {
  if (!nzchar(Sys.getenv("HUIT_API_NARRATE"))) {
    key <- tryCatch(keyring::key_get("HUIT_API_NARRATE"), error = function(e) "")
    if (!nzchar(key)) {
      stop(
        "Set the HUIT_API_NARRATE environment variable (or keyring entry) ",
        "to the API key first"
      )
    }
    Sys.setenv(HUIT_API_NARRATE = key)
  }

  conn <- dbGetConn(db_path)

  # Checks run here (not in pipeline_steps) so the real error isn't masked by
  # sqlife's missing-dbFinish() error
  reviewer_id <- tbl(conn, "reviewer") |>
    filter(model == local(model), human == 0) |>
    pull(id)
  rubric <- tbl(conn, "rubric") |>
    filter(id == local(rubric_id)) |>
    collect()

  if (length(reviewer_id) != 1) {
    dbFinish(conn)
    stop(
      "Expected exactly one AI reviewer with model '", model, "', found ",
      length(reviewer_id), ". Create one with dbReviewerAI(conn, model = ...)"
    )
  }
  if (nrow(rubric) != 1 || is.na(rubric$prompt_extract_id) ||
    is.na(rubric$prompt_score_id)) {
    dbFinish(conn)
    stop("Rubric ", rubric_id, " missing or has no extract / score prompt")
  }

  # An error inside would otherwise only surface as sqlife's generic
  # missing-dbFinish() message, so report the real one before unwinding
  result <- withCallingHandlers(
    pipeline_steps(
      conn,
      db_path = db_path,
      reviewer_id = reviewer_id,
      rubric_id = rubric_id,
      model = model,
      evaluation_ids = evaluation_ids,
      chunk_size = chunk_size,
      max_extract_attempts = max_extract_attempts,
      live_fix_truncated = live_fix_truncated,
      live_fix_confirm = live_fix_confirm,
      live_max_output_tokens = live_max_output_tokens,
      notify = notify,
      notify_max_wait = notify_max_wait,
      backup_dir = backup_dir
    ),
    error = function(e) message("llm_review_pipeline() failed: ", conditionMessage(e))
  )

  dbFinish(conn)
  invisible(result)
}

#' Pipeline steps for llm_review_pipeline()
#'
#' Internal: runs on the connection opened by llm_review_pipeline(), see its
#' docs for the arguments and steps.
#'
#' @import dplyr
#' @returns See llm_review_pipeline()
pipeline_steps <- function(
  conn,
  db_path,
  reviewer_id,
  rubric_id,
  model,
  evaluation_ids,
  chunk_size,
  max_extract_attempts,
  live_fix_truncated,
  live_fix_confirm,
  live_max_output_tokens,
  notify,
  notify_max_wait,
  backup_dir
) {
  log <- function(...) {
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(...))
  }

  log(sprintf(
    "Rubric %d | model %s (reviewer %d) | %s",
    rubric_id, model, reviewer_id, db_path
  ))

  # One backup per call, made right before its first write (a call that only
  # finds batches still running writes nothing and makes no backup)
  backup_done <- FALSE
  backup <- function() {
    if (backup_done) return(invisible())
    backup_path <- file.path(
      backup_dir,
      sprintf(
        "%s_pre_rubric%d_%s_pipeline_%s.db",
        tools::file_path_sans_ext(basename(db_path)), rubric_id, model,
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
    )
    dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
    stopifnot(file.copy(db_path, backup_path))
    log("Backup written to", backup_path)
    backup_done <<- TRUE
  }

  # ── 1. Create missing review assignments ───────────────────────────────────
  if (is.null(evaluation_ids)) {
    evaluation_ids <- tbl(conn, "evaluation") |>
      filter(summary_flg == 0) |>
      pull(id)
  }
  done_evals <- tbl(conn, "review_assignment") |>
    filter(reviewer_id == local(reviewer_id), rubric_id == local(rubric_id)) |>
    pull(evaluation_id)
  new_evals <- setdiff(evaluation_ids, done_evals)

  if (length(new_evals) > 0) {
    backup()
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

  # All reviews in scope (incl. ones finished earlier)
  rids <- tbl(conn, "review_assignment") |>
    filter(
      reviewer_id == local(reviewer_id),
      rubric_id == local(rubric_id),
      evaluation_id %in% local(evaluation_ids)
    ) |>
    pull(id)

  ids_at <- function(codes) {
    tbl(conn, "review_assignment") |>
      filter(id %in% local(rids), statusCode %in% local(codes)) |>
      pull(id)
  }

  submit <- function(step, ids, force = FALSE) {
    backup()
    llm_comp_batch_submit_chunks(
      conn, step, ids,
      chunk_size = chunk_size, model = model, force = force,
      notify = notify, notify_max_wait = notify_max_wait
    )
  }

  # ── 2. Check open batches, process completed ones ──────────────────────────
  n_processed <- 0L
  submitted_status <- c(extract = 1L, score = 4L, resolve = 7L)

  for (bid in db_review_batches(conn, rids, open_only = TRUE)$id) {
    st <- llm_batch_status(bid, conn)$statusCode
    step <- db_batch_step(conn, bid)

    if (is.na(step)) {
      log(sprintf("Batch %d: no reviews left at a submitted status - skipped", bid))
    } else if (st == 3) {
      log(sprintf("Batch %d (%s) completed - processing results", bid, step))
      backup()
      batch_process(bid, conn)
      n_processed <- n_processed + 1L
    } else if (st < 0) {
      # Failed / expired / cancelled: its reviews are stuck at the submitted
      # status, so resubmit them for the same step
      ids <- tbl(conn, "batch_review") |>
        filter(batch_id == local(bid)) |>
        pull(review_assignment_id) |>
        intersect(ids_at(submitted_status[[step]]))
      log(sprintf(
        "Batch %d (%s) did not complete (statusCode %d) - resubmitting %d reviews",
        bid, step, st, length(ids)
      ))
      submit(step, ids, force = TRUE)
    } else if (notify && !batch_monitor_running(bid)) {
      # Monitors don't survive a reboot - relaunch one if none is running
      batch_status_notify(bid, db_path, max_wait = notify_max_wait)
      log(sprintf("Batch %d (%s) still running - PushOver monitor relaunched", bid, step))
    } else {
      log(sprintf("Batch %d (%s) still running", bid, step))
    }
  }

  # ── 3. Submit the next step ────────────────────────────────────────────────
  to_extract <- ids_at(0L)
  if (length(to_extract) > 0) {
    log("Extraction: submitting", length(to_extract), "reviews")
    submit("extract", to_extract)
  }

  # A -2 is usually a transient model failure that a plain resubmit clears
  extract_failed <- ids_at(-2L)
  to_retry <- extract_failed[
    db_extract_attempts(conn, extract_failed) < max_extract_attempts
  ]
  if (length(to_retry) > 0) {
    log("Extraction retry:", length(to_retry), "failed reviews")
    submit("extract", to_retry, force = TRUE)
  }

  # batch_resolve_process() sends a review back to 6 for another round, or to
  # -4 once attempts are exhausted
  to_resolve <- ids_at(6L)
  if (length(to_resolve) > 0) {
    log("Conflict resolution:", length(to_resolve), "conflicted reviews")
    submit("resolve", to_resolve)
  }

  to_score <- ids_at(3L)
  if (length(to_score) > 0) {
    log("Scoring: submitting", length(to_score), "reviews")
    submit("score", to_score)
  }

  # ── 4. Progress ────────────────────────────────────────────────────────────
  progress <- db_review_progress(conn, rids)
  open_batches <- db_review_batches(conn, rids, open_only = TRUE)

  cat("\nReview status (", length(rids), " reviews):\n", sep = "")
  print(as.data.frame(progress), row.names = FALSE)

  if (nrow(open_batches) > 0) {
    cat("\nOpen batches (1 = submitted, 2 = in progress, 3 = completed):\n")
    print(as.data.frame(open_batches), row.names = FALSE)
    log("Run again when the batches have finished")
    return(list(complete = FALSE, progress = progress, open_batches = open_batches))
  }

  # ── 5. Live rerun of truncated reviews ─────────────────────────────────────
  n_trunc <- length(ids_at(c(-5L, -6L)))
  n_live <- 0L

  if (live_fix_truncated && n_trunc > 0) {
    proceed <- TRUE
    if (live_fix_confirm) {
      answer <- console_ask(sprintf(
        "\n%d extraction-truncated (-5) and %d scoring-truncated (-6) reviews can be rerun live with max_output_tokens = %d. Proceed? [y/N] ",
        length(ids_at(-5L)), length(ids_at(-6L)), live_max_output_tokens
      ))
      # isTRUE: no input at all (EOF) gives character(0) -> treated as "no"
      proceed <- isTRUE(tolower(trimws(answer)) %in% c("y", "yes"))
    }

    if (proceed) {
      backup()
      llm_comp_fix_truncated_run(
        conn, rids,
        model = model, max_output_tokens = live_max_output_tokens
      )
      n_live <- n_trunc
      progress <- db_review_progress(conn, rids)
      cat("\nReview status after live rerun:\n")
      print(as.data.frame(progress), row.names = FALSE)
    } else {
      log("Skipped live rerun")
    }
  }

  # ── 6. Summary ─────────────────────────────────────────────────────────────
  n_done <- sum(progress$n[progress$statusCode == 5])
  msg <- sprintf(
    "Rubric %d %s pipeline complete: %d / %d reviews scored",
    rubric_id, model, n_done, length(rids)
  )
  log(msg)
  stuck <- filter(progress, statusCode != 5)
  if (nrow(stuck) > 0) {
    log(
      "Reviews needing attention (-2 extraction failed, -3 scoring failed,",
      "-4 conflict unresolved, -5 / -6 extraction / scoring truncated):"
    )
    print(as.data.frame(stuck), row.names = FALSE)
    if (any(stuck$statusCode %in% c(-5, -6))) {
      log("Truncated reviews left: rerun with a higher live_max_output_tokens")
    }
  }
  # Only notify from the call that actually finished the work
  if (notify && n_processed + n_live > 0) pushover_send(msg)

  list(complete = TRUE, progress = progress, open_batches = open_batches)
}

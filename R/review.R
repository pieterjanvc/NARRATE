# ─── Main review pipeline functions ──────────────────────────────────────────
# Primary functions for running the extraction and scoring pipeline.
# Each step has a real-time (synchronous) variant and a batch variant.
#
# After a successful extraction write, dbCompExtractionCheckConflicts() checks
# for rule-2 ("one competency per quote") violations. If any are found, the
# review is left at statusCode 6 (Extraction conflict pending) instead of 3,
# which also keeps it out of scoring (db_fetch_review_score() requires == 3).
# The resolve step (llm_comp_resolve_run() live, or
# llm_comp_resolve_batch_submit() + batch_resolve_process() batch) asks the
# LLM to pick a single winning competency per conflict and re-checks, up to a
# capped number of attempts; if still unresolved it's left at -4 (Extraction
# conflict unresolved) for human review instead of looping forever. The batch
# path holds submitted reviews at statusCode 7 and persists the attempt count
# in review_assignment.note between rounds.
#
# Typical pipeline (batch):
#   batch <- llm_comp_extract_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)          # poll until statusCode == 3
#   batch_extract_process(batch$id, conn)
#   # for any review left at statusCode 6 (Extraction conflict pending),
#   # repeat submit + process until it clears (3) or is exhausted (-4):
#   batch <- llm_comp_resolve_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)
#   batch_resolve_process(batch$id, conn)
#   batch <- llm_comp_score_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)
#   batch_score_process(batch$id, conn)
#
# Typical pipeline (real-time):
#   llm_comp_extract_run(conn, review_ids)
#   llm_comp_resolve_run(conn, review_ids)    # only for reviews left at statusCode 6
#   llm_comp_score_run(conn, review_ids)

# ─── Real-time ────────────────────────────────────────────────────────────────

#' Run synchronous competency extraction for a set of review assignments
#'
#' Fetches evaluation text and prompts from the database, calls llm_comp_extract()
#' for each review assignment, and writes results back immediately.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages. Default = FALSE
#' @param force Reprocess even if statusCode != 0. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Data frame summarising results per review_id
#'   (review_id, statusCode, tokens_in, tokens_out). statusCode 6 means
#'   extraction succeeded but dbCompExtractionCheckConflicts() found a rule-2
#'   violation (see status_codes table).
#' @export
llm_comp_extract_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_extract(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    if (verbose) message("Processing review ", rid, "...")

    result <- llm_comp_extract(
      evaluation_text = review_info$evaluation[i],
      prompt = review_info$prompt[i],
      model = model,
      endpoint = endpoint
    )

    new_status <- if (result$statusCode == 2) 3L else -2L

    if (result$statusCode == 2 && length(result$data) > 0) {
      write_result <- dbCompExtraction(conn, rid, result$data, commit = FALSE)
      if (!isTRUE(write_result$success)) {
        new_status <- -2L
      } else if (isTRUE(dbCompExtractionCheckConflicts(conn, rid)$has_conflicts)) {
        new_status <- 6L # Extraction conflict pending (rule-2 violation detected)
      }
    }

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn, "review_assignment", returnData = FALSE, commit = TRUE
    )

    data.frame(
      review_id = rid, statusCode = new_status,
      tokens_in = result$tokens_in, tokens_out = result$tokens_out
    )
  })

  bind_rows(results)
}

#' Resolve rule-2 conflicts for a set of review assignments (real-time)
#'
#' For each review currently at statusCode 6 (Extraction conflict pending),
#' builds a resolve prompt from its conflicting quotes (build_resolve_conflicts()),
#' asks the LLM to pick one winning competency per conflict (or discard),
#' applies the answer via dbCompConflictResolve(), and re-checks. Retries up
#' to max_attempts times before giving up and marking the review -4
#' (Extraction conflict unresolved) for human review. Review_assignment IDs
#' not currently at statusCode 6 are skipped. No batch equivalent yet.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param max_attempts Maximum resolve attempts per review before giving up. Default = 2
#' @param verbose Print progress messages. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Data frame summarising results per review_id (review_id,
#'   statusCode, attempts, tokens_in, tokens_out), or NULL if none of the
#'   given IDs are at statusCode 6
#' @export
llm_comp_resolve_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  max_attempts = 2,
  verbose = FALSE
) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids), statusCode == 6) |>
    select(review_id = id, rubric_id) |>
    collect()

  if (nrow(review_info) == 0) {
    warning(
      "No review assignments at statusCode 6 (Extraction conflict pending) ",
      "among the given IDs."
    )
    return(invisible(NULL))
  }

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    rubric_id <- review_info$rubric_id[i]
    if (verbose) message("Resolving review ", rid, "...")

    prompt <- prompt_generate_resolve(conn, rubric_id)
    tokens_in_total <- 0
    tokens_out_total <- 0
    attempt <- 0L
    new_status <- 6L

    repeat {
      check <- dbCompExtractionCheckConflicts(conn, rid)
      if (!isTRUE(check$has_conflicts)) {
        new_status <- 3L
        break
      }
      if (attempt >= max_attempts) {
        new_status <- -4L
        break
      }
      attempt <- attempt + 1L

      comp <- prompt_build_competencies(conn, rubric_id)
      built <- build_resolve_conflicts(check$conflicts, comp$comp_data, conn)

      result <- llm_comp_resolve(built$text, prompt, model = model, endpoint = endpoint)
      if (!is.na(result$tokens_in)) tokens_in_total <- tokens_in_total + result$tokens_in
      if (!is.na(result$tokens_out)) tokens_out_total <- tokens_out_total + result$tokens_out

      if (result$statusCode != 2) next # API/parse failure - counts as a spent attempt, try again

      dbCompConflictResolve(conn, built$clusters, result$data, commit = FALSE)
    }

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        note = sprintf("conflict_resolve_attempts:%d", attempt),
        tokens_in = tokens_in_total,
        tokens_out = tokens_out_total,
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn, "review_assignment", returnData = FALSE, commit = TRUE
    )

    data.frame(
      review_id = rid, statusCode = new_status, attempts = attempt,
      tokens_in = tokens_in_total, tokens_out = tokens_out_total
    )
  })

  bind_rows(results)
}

#' Re-anchor paraphrased competency evidence for a set of reviews (real-time)
#'
#' Phase 2 of the paraphrased-quote fix (dev/paraphrase_fix_plan.md). For each
#' review at statusCode 5 (Batch scoring complete) that still has
#' `competency_text` rows with no located position, builds one re-anchor
#' request (build_reanchor_items()) from the evaluation text and the unplaced
#' quotes, asks the LLM for the verbatim span each quote refers to
#' (llm_comp_reanchor()), and applies the answer via dbCompReanchorApply().
#'
#' Per review, the resulting statusCode is:
#'   - 3  if any row was re-anchored (text_match changed) and no rule-2
#'        conflict resulted - ready for re-scoring
#'   - 6  if a re-anchor introduced a rule-2 conflict - needs resolve first
#'   - 5  unchanged (nothing re-anchored, or the API call failed) - any rows
#'        the model couldn't anchor are now marked locate_status = 'unlocated'
#'        for human review
#'
#' No batch equivalent yet - see llm_comp_reanchor_batch_submit() when added.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages. Default = FALSE
#' @param force Process even if not at statusCode 5. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Data frame summarising results per review (review_id, statusCode,
#'   n_items, n_reanchored, n_flagged, call_status, tokens_in, tokens_out), or
#'   NULL if none of the given IDs have anything to re-anchor
#' @export
llm_comp_reanchor_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_reanchor(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  prompt <- paste(
    readLines(prompt_template_path("prompt_comp_reanchor.md"), warn = FALSE),
    collapse = "\n"
  )

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    items <- review_info$items[[i]]
    if (verbose) {
      message("Re-anchoring review ", rid, " (", nrow(items), " items)...")
    }

    body_text <- build_reanchor_items(review_info$evaluation[i], items)
    result <- llm_comp_reanchor(body_text, prompt, model = model, endpoint = endpoint)

    applied <- NULL
    new_status <- 5L
    if (result$statusCode == 2) {
      applied <- dbCompReanchorApply(conn, rid, items, result$data, commit = TRUE)
      if (applied$summary[["n_reanchored"]] > 0) {
        new_status <- if (
          isTRUE(dbCompExtractionCheckConflicts(conn, rid)$has_conflicts)
        ) {
          6L
        } else {
          3L
        }
      }
    }

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn, "review_assignment", returnData = FALSE, commit = TRUE
    )

    data.frame(
      review_id = rid,
      statusCode = new_status,
      n_items = nrow(items),
      n_reanchored = if (is.null(applied)) NA_integer_ else applied$summary[["n_reanchored"]],
      n_flagged = if (is.null(applied)) NA_integer_ else applied$summary[["n_flagged"]],
      call_status = result$statusCode,
      tokens_in = result$tokens_in,
      tokens_out = result$tokens_out
    )
  })

  bind_rows(results)
}

#' Run synchronous competency scoring for a set of review assignments
#'
#' Fetches extracted competency texts from the database, calls llm_comp_score()
#' for each review assignment, and writes results back immediately.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages. Default = FALSE
#' @param force Reprocess even if statusCode != 3. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Data frame summarising results per review_id
#'   (review_id, statusCode, tokens_in, tokens_out)
#' @export
llm_comp_score_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_score(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  extractions <- db_fetch_extractions(conn, review_info$review_id)
  score_maps  <- db_fetch_score_maps(conn, unique(review_info$rubric_id))

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid      <- review_info$review_id[i]
    maps     <- score_maps[[as.character(review_info$rubric_id[i])]]
    if (verbose) message("Scoring review ", rid, "...")

    rows <- extractions[extractions$review_assignment_id == rid, ]
    extr <- lapply(
      split(rows, rows$competency_id),
      function(g) list(cIndex = g$comp_order[[1]], text = as.list(g$text_match))
    )

    result <- llm_comp_score(
      extractions = unname(extr),
      prompt = review_info$prompt[i],
      model = model,
      endpoint = endpoint
    )

    new_status <- if (result$statusCode == 2) 5L else -3L

    if (result$statusCode == 2) {
      db_write_score_specificity(conn, rid, result$data$competencies, commit = FALSE)
    }

    util_val <- if (result$statusCode == 2) result$data$utility else NA_integer_
    sent_val <- if (result$statusCode == 2) result$data$sentiment else NA_integer_

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        utility_score_value = util_val,
        utility_score_id = if (is.na(util_val)) NA_integer_ else
          maps$util$utility_id[maps$util$value == as.character(util_val)],
        sentiment_score_value = sent_val,
        sentiment_score_id = if (is.na(sent_val)) NA_integer_ else
          maps$sent$sentiment_id[maps$sent$value == as.character(sent_val)],
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn, "review_assignment", returnData = FALSE, commit = TRUE
    )

    data.frame(
      review_id = rid, statusCode = new_status,
      tokens_in = result$tokens_in, tokens_out = result$tokens_out
    )
  })

  bind_rows(results)
}

# ─── Batch submit ─────────────────────────────────────────────────────────────

#' Submit a batch competency extraction job
#'
#' Builds and uploads a batch of extraction requests for a set of review
#' assignments, creates the batch job, and records it in the database.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure batch deployment name. Default = "gpt-5.1-batch"
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @param verbose Print progress messages. Default = FALSE
#' @param force Resubmit even if already in-progress or completed. Default = FALSE
#'
#' @returns Inserted batch record data frame
#' @export
llm_comp_extract_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_extract(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  requests <- setNames(
    lapply(seq_len(nrow(review_info)), function(i) {
      llm_build_extract_body(review_info$evaluation[i], review_info$prompt[i])
    }),
    paste0("review-", review_info$review_id)
  )

  if (verbose) message("Uploading ", length(requests), " requests...")
  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model), endpoint, api_key
  )

  if (verbose) message("Creating batch job...")
  batch_id <- llm_batch_create(file_input_id, endpoint, api_key)

  db_record_batch(conn, file_input_id, batch_id, review_info$review_id, review_status = 1L)
}

#' Submit a batch competency scoring job
#'
#' Fetches extracted competency texts from the database, builds and uploads a
#' batch of scoring requests, creates the batch job, and records it.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure batch deployment name. Default = "gpt-5.1-batch"
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @param verbose Print progress messages. Default = FALSE
#' @param force Resubmit even if already scored. Default = FALSE
#'
#' @returns Inserted batch record data frame
#' @export
llm_comp_score_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_score(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  extractions <- db_fetch_extractions(conn, review_info$review_id)

  requests <- setNames(
    lapply(review_info$review_id, function(rid) {
      rows <- extractions[extractions$review_assignment_id == rid, ]
      extr <- lapply(
        split(rows, rows$competency_id),
        function(g) list(cIndex = g$comp_order[[1]], text = as.list(g$text_match))
      )
      llm_build_score_body(
        unname(extr),
        review_info$prompt[review_info$review_id == rid]
      )
    }),
    paste0("review-", review_info$review_id)
  )

  if (verbose) message("Uploading ", length(requests), " requests...")
  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model), endpoint, api_key
  )

  if (verbose) message("Creating batch job...")
  batch_id <- llm_batch_create(file_input_id, endpoint, api_key)

  db_record_batch(conn, file_input_id, batch_id, review_info$review_id, review_status = 4L)
}

#' Submit a batch rule-2 conflict-resolution job
#'
#' Batch equivalent of llm_comp_resolve_run(), for when conflict volume is too
#' high to resolve one-by-one in real time. For each review currently at
#' statusCode 6 (Extraction conflict pending), builds a resolve request from
#' its current conflicts (dbCompExtractionCheckConflicts() +
#' build_resolve_conflicts()) and the per-rubric resolve prompt
#' (prompt_generate_resolve()), uploads them as one batch, and records it.
#' Submitted reviews move to statusCode 7 (Conflict resolve batch submitted);
#' batch_resolve_process() applies the results.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure batch deployment name. Default = "gpt-5.1-batch"
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @param verbose Print progress messages. Default = FALSE
#' @param force Resubmit even if not at statusCode 6. Default = FALSE
#'
#' @returns Inserted batch record data frame
#' @export
llm_comp_resolve_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_resolve(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  # Per-rubric resolve prompt and competency data are the same for every
  # review sharing a rubric, so build them once per rubric_id.
  prompt_cache <- list()
  comp_cache <- list()

  requests <- setNames(
    lapply(seq_len(nrow(review_info)), function(i) {
      rubric_id <- review_info$rubric_id[i]
      rubric_key <- as.character(rubric_id)
      if (is.null(prompt_cache[[rubric_key]])) {
        prompt_cache[[rubric_key]] <<- prompt_generate_resolve(conn, rubric_id)
        comp_cache[[rubric_key]] <<- prompt_build_competencies(conn, rubric_id)$comp_data
      }

      built <- build_resolve_conflicts(
        review_info$conflicts[[i]], comp_cache[[rubric_key]], conn
      )
      llm_build_resolve_body(built$text, prompt_cache[[rubric_key]])
    }),
    paste0("review-", review_info$review_id)
  )

  if (verbose) message("Uploading ", length(requests), " requests...")
  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model), endpoint, api_key
  )

  if (verbose) message("Creating batch job...")
  batch_id <- llm_batch_create(file_input_id, endpoint, api_key)

  db_record_batch(conn, file_input_id, batch_id, review_info$review_id, review_status = 7L)
}

#' Submit a batch competency-evidence re-anchor job
#'
#' Batch equivalent of llm_comp_reanchor_run() (Phase 2 of the paraphrased-quote
#' fix). For each review at statusCode 5 with unplaced competency_text rows,
#' builds one re-anchor request (build_reanchor_items() + the shared
#' inst/prompt_comp_reanchor.md), uploads them as one batch, and records it.
#' Submitted reviews move to statusCode 8 (Reanchor batch submitted);
#' batch_reanchor_process() applies the results.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure batch deployment name. Default = "gpt-5.1-batch"
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @param verbose Print progress messages. Default = FALSE
#' @param force Submit even if not at statusCode 5. Default = FALSE
#'
#' @returns Inserted batch record data frame
#' @export
llm_comp_reanchor_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = FALSE,
  force = FALSE
) {
  review_info <- db_fetch_review_reanchor(conn, review_ids, force)
  if (is.null(review_info)) return(invisible(NULL))

  prompt <- paste(
    readLines(prompt_template_path("prompt_comp_reanchor.md"), warn = FALSE),
    collapse = "\n"
  )

  requests <- setNames(
    lapply(seq_len(nrow(review_info)), function(i) {
      body_text <- build_reanchor_items(
        review_info$evaluation[i], review_info$items[[i]]
      )
      llm_build_reanchor_body(body_text, prompt)
    }),
    paste0("review-", review_info$review_id)
  )

  if (verbose) message("Uploading ", length(requests), " requests...")
  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model), endpoint, api_key
  )

  if (verbose) message("Creating batch job...")
  batch_id <- llm_batch_create(file_input_id, endpoint, api_key)

  db_record_batch(
    conn, file_input_id, batch_id, review_info$review_id, review_status = 8L
  )
}

# ─── Batch process ────────────────────────────────────────────────────────────

#' Process completed batch extraction results
#'
#' Fetches and parses the batch output, writes competency extraction data to
#' the database via dbCompExtraction(), and updates review_assignment status codes.
#' Reviews with a rule-2 conflict (see dbCompExtractionCheckConflicts()) are
#' left at statusCode 6 (Extraction conflict pending) instead of 3, which
#' also keeps them out of scoring.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
batch_extract_process <- function(batch_id, conn) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode != 3) {
    message("No results to process")
    return(batch_info)
  }

  results <- batch_results_preprocess(batch_info$file_output_id)

  # Write competency data first so a failed write (e.g. the LLM emitted a
  # duplicate cIndex) can downgrade that review's status to -2 below, instead
  # of leaving it marked as complete or aborting the whole batch.
  write_status <- sapply(results, function(r) {
    if (r$statusCode != 2) return(-2L)

    extractions <- r$data$extractions
    if (length(extractions) == 0) return(3L)

    # fromJSON with simplifyVector=FALSE returns text as a list; dbCompExtraction
    # needs character vectors so data.frame() doesn't treat values as column names
    for (i in seq_along(extractions)) {
      extractions[[i]]$text <- unlist(extractions[[i]]$text)
    }
    result <- dbCompExtraction(conn, r$review_id, extractions, commit = FALSE)
    if (!isTRUE(result$success)) return(-2L)

    if (isTRUE(dbCompExtractionCheckConflicts(conn, r$review_id)$has_conflicts)) {
      6L # Extraction conflict pending (rule-2 violation detected)
    } else {
      3L
    }
  })

  to_update <- lapply(results, "[", c("review_id", "tokens_in", "tokens_out")) |>
    bind_rows() |>
    mutate(
      statusCode = write_status,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) |>
    rename(id = review_id)

  tbl_update(to_update, conn, "review_assignment", returnData = FALSE, commit = FALSE)

  tbl_update(
    data.frame(
      id = batch_id, statusCode = 4L,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    ),
    conn, "batch"
  )
}

#' Process completed batch conflict-resolution results
#'
#' Batch equivalent of the apply/re-check loop inside llm_comp_resolve_run().
#' Fetches and parses the batch output, and for each review re-derives its
#' current conflict clusters (dbCompExtractionCheckConflicts() +
#' build_resolve_conflicts(), deterministic for the same conflicts), applies
#' the model's decisions via dbCompConflictResolve(), and re-checks. The
#' retry count persists in review_assignment.note
#' ("conflict_resolve_attempts:N") between rounds, since the batch path is
#' multi-step (submit -> poll -> process) rather than a single in-memory loop.
#'
#' Per review, the resulting statusCode is:
#'   - 3  if no conflicts remain
#'   - 6  if conflicts remain and attempts are still under max_attempts
#'        (eligible for another llm_comp_resolve_batch_submit() round)
#'   - -4 if conflicts remain and attempts are exhausted (needs human review)
#' A failed or unparseable result still counts as a spent attempt, matching
#' the live path.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#' @param max_attempts Resolve rounds allowed before giving up. Default = 2
#'   (matches llm_comp_resolve_run())
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
batch_resolve_process <- function(batch_id, conn, max_attempts = 2) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode != 3) {
    message("No results to process")
    return(batch_info)
  }

  results <- batch_results_preprocess(batch_info$file_output_id)
  review_ids <- sapply(results, "[[", "review_id")

  meta <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, rubric_id, note) |>
    collect()

  # comp_data is the same for every review sharing a rubric - build once each
  comp_cache <- list()
  comp_data_for <- function(rubric_id) {
    key <- as.character(rubric_id)
    if (is.null(comp_cache[[key]])) {
      comp_cache[[key]] <<- prompt_build_competencies(conn, rubric_id)$comp_data
    }
    comp_cache[[key]]
  }

  parse_attempts <- function(note) {
    m <- regmatches(note, regexpr("conflict_resolve_attempts:\\d+", note))
    if (length(m) == 0 || is.na(note)) return(0L)
    as.integer(sub(".*:", "", m))
  }

  to_update <- lapply(results, function(r) {
    rid <- r$review_id
    row <- meta[meta$review_id == rid, ]
    attempts <- parse_attempts(row$note) + 1L # this round counts, pass or fail

    if (r$statusCode == 2) {
      check <- dbCompExtractionCheckConflicts(conn, rid)
      if (isTRUE(check$has_conflicts)) {
        built <- build_resolve_conflicts(
          check$conflicts, comp_data_for(row$rubric_id), conn
        )
        # r$data is the full parsed object; resolutions is NULL if the model
        # omitted the key, which dbCompConflictResolve() treats as "none
        # answered" - a spent attempt, same as a parse failure
        dbCompConflictResolve(conn, built$clusters, r$data$resolutions, commit = FALSE)
      }
    }

    remaining <- isTRUE(dbCompExtractionCheckConflicts(conn, rid)$has_conflicts)
    new_status <- if (!remaining) 3L else if (attempts >= max_attempts) -4L else 6L

    data.frame(
      id = rid,
      statusCode = new_status,
      note = sprintf("conflict_resolve_attempts:%d", attempts),
      tokens_in = r$tokens_in,
      tokens_out = r$tokens_out,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  tbl_update(to_update, conn, "review_assignment", returnData = FALSE, commit = FALSE)

  tbl_update(
    data.frame(
      id = batch_id, statusCode = 4L,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    ),
    conn, "batch"
  )
}

#' Process completed batch re-anchor results
#'
#' Batch equivalent of llm_comp_reanchor_run(). Fetches and parses the batch
#' output and, per review, re-derives the itemId -> competency_text_id mapping
#' from its current unplaced rows (deterministic - nothing touches a review
#' between submit at statusCode 8 and here), applies the model's spans via
#' dbCompReanchorApply(), and re-checks for rule-2 conflicts.
#'
#' Per review, the resulting statusCode is:
#'   - 3  if >= 1 row was re-anchored and no rule-2 conflict resulted
#'   - 6  if a re-anchor created a rule-2 conflict (goes to the resolve step)
#'   - 5  unchanged (nothing re-anchored, a parse failure, or the current
#'        unplaced-row count no longer matches the request) - any rows the
#'        model couldn't anchor are marked locate_status = 'unlocated'
#'
#' A parse failure or count mismatch leaves the review at 5 for another
#' llm_comp_reanchor_batch_submit() round.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
batch_reanchor_process <- function(batch_id, conn) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode != 3) {
    message("No results to process")
    return(batch_info)
  }

  results <- batch_results_preprocess(batch_info$file_output_id)
  review_ids <- vapply(results, "[[", integer(1), "review_id")

  # itemId -> competency_text_id mapping, rebuilt from each review's current
  # unplaced rows (force = TRUE: reviews are at statusCode 8, not 5)
  info <- db_fetch_review_reanchor(conn, review_ids, force = TRUE)

  to_update <- lapply(results, function(r) {
    rid <- r$review_id
    row <- if (is.null(info)) NULL else info[info$review_id == rid, ]

    new_status <- 5L
    if (r$statusCode == 2 && !is.null(row) && nrow(row) == 1) {
      items <- row$items[[1]]
      anchors <- r$data$anchors
      # only apply if the response covers exactly the current item set
      n_ans <- if (is.null(anchors)) 0L else length(anchors)
      if (n_ans == nrow(items)) {
        applied <- dbCompReanchorApply(conn, rid, items, anchors, commit = FALSE)
        if (applied$summary[["n_reanchored"]] > 0) {
          new_status <- if (
            isTRUE(dbCompExtractionCheckConflicts(conn, rid)$has_conflicts)
          ) {
            6L
          } else {
            3L
          }
        }
      }
    }

    data.frame(
      id = rid,
      statusCode = new_status,
      tokens_in = r$tokens_in,
      tokens_out = r$tokens_out,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  tbl_update(to_update, conn, "review_assignment", returnData = FALSE, commit = FALSE)

  tbl_update(
    data.frame(
      id = batch_id, statusCode = 4L,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    ),
    conn, "batch"
  )
}

#' Process completed batch scoring results
#'
#' Fetches and parses the batch output, updates competency specificity scores
#' via db_write_score_specificity(), and writes utility and sentiment to
#' review_assignment.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
batch_score_process <- function(batch_id, conn) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode != 3) {
    message("No results to process")
    return(batch_info)
  }

  results <- batch_results_preprocess(batch_info$file_output_id)
  success <- sapply(results, "[[", "statusCode") == 2

  # Fetch rubric_id for each review so we can look up the correct score IDs
  all_review_ids <- sapply(results, "[[", "review_id")
  rubric_map <- tbl(conn, "review_assignment") |>
    filter(id %in% local(all_review_ids)) |>
    select(review_id = id, rubric_id) |>
    collect()
  score_maps <- db_fetch_score_maps(conn, unique(rubric_map$rubric_id))

  to_update <- lapply(results, function(r) {
    rid      <- r$review_id
    maps     <- score_maps[[as.character(rubric_map$rubric_id[rubric_map$review_id == rid])]]
    util_val <- if (r$statusCode == 2) r$data$utility else NA_integer_
    sent_val <- if (r$statusCode == 2) r$data$sentiment else NA_integer_
    data.frame(
      id = rid,
      statusCode = if (r$statusCode == 2) 5L else -3L,
      tokens_in = r$tokens_in,
      tokens_out = r$tokens_out,
      utility_score_value = util_val,
      utility_score_id = if (is.na(util_val)) NA_integer_ else
        maps$util$utility_id[maps$util$value == as.character(util_val)],
      sentiment_score_value = sent_val,
      sentiment_score_id = if (is.na(sent_val)) NA_integer_ else
        maps$sent$sentiment_id[maps$sent$value == as.character(sent_val)],
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  tbl_update(to_update, conn, "review_assignment", returnData = FALSE, commit = FALSE)

  for (r in results[success]) {
    db_write_score_specificity(conn, r$review_id, r$data$competencies, commit = FALSE)
  }

  tbl_update(
    data.frame(
      id = batch_id, statusCode = 4L,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    ),
    conn, "batch"
  )
}

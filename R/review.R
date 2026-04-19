# ─── Main review pipeline functions ──────────────────────────────────────────
# Primary functions for running the extraction and scoring pipeline.
# Each step has a real-time (synchronous) variant and a batch variant.
#
# Typical pipeline (batch):
#   batch <- llm_comp_extract_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)          # poll until statusCode == 3
#   batch_extract_process(batch$id, conn)
#   batch <- llm_comp_score_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)
#   batch_score_process(batch$id, conn)
#
# Typical pipeline (real-time):
#   llm_comp_extract_run(conn, review_ids)
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
#'   (review_id, statusCode, tokens_in, tokens_out)
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

    new_status <- if (result$statusCode == 2) 3L else -1L

    if (result$statusCode == 2 && length(result$data) > 0) {
      dbCompExtraction(conn, rid, result$data, commit = FALSE)
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

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    if (verbose) message("Scoring review ", rid, "...")

    rows <- extractions[extractions$review_assignment_id == rid, ]
    extr <- lapply(
      split(rows, rows$competency_id),
      function(g) list(cID = g$competency_id[[1]], text = as.list(g$text_match))
    )

    result <- llm_comp_score(
      extractions = unname(extr),
      prompt = review_info$prompt[i],
      model = model,
      endpoint = endpoint
    )

    new_status <- if (result$statusCode == 2) 5L else -1L

    if (result$statusCode == 2) {
      db_write_score_specificity(conn, rid, result$data$competencies, commit = FALSE)
    }

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        utility = if (result$statusCode == 2) result$data$utility else NA_integer_,
        sentiment = if (result$statusCode == 2) result$data$sentiment else NA_integer_,
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
        function(g) list(cID = g$competency_id[[1]], text = as.list(g$text_match))
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

# ─── Batch process ────────────────────────────────────────────────────────────

#' Process completed batch extraction results
#'
#' Fetches and parses the batch output, writes competency extraction data to
#' the database via dbCompExtraction(), and updates review_assignment status codes.
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
  success <- sapply(results, "[[", "statusCode") == 2

  to_update <- lapply(results, "[", c("review_id", "statusCode", "tokens_in", "tokens_out")) |>
    bind_rows() |>
    mutate(
      statusCode = ifelse(statusCode == 2, 3L, -1L),
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) |>
    rename(id = review_id)

  tbl_update(to_update, conn, "review_assignment", returnData = FALSE, commit = FALSE)

  for (r in results[success]) {
    extractions <- r$data$extractions
    if (length(extractions) > 0) {
      # fromJSON with simplifyVector=FALSE returns text as a list; dbCompExtraction
      # needs character vectors so data.frame() doesn't treat values as column names
      for (i in seq_along(extractions)) {
        extractions[[i]]$text <- unlist(extractions[[i]]$text)
      }
      dbCompExtraction(conn, r$review_id, extractions, commit = FALSE)
    }
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

  to_update <- lapply(results, function(r) {
    data.frame(
      id = r$review_id,
      statusCode = if (r$statusCode == 2) 5L else -1L,
      tokens_in = r$tokens_in,
      tokens_out = r$tokens_out,
      utility = if (r$statusCode == 2) r$data$utility else NA_integer_,
      sentiment = if (r$statusCode == 2) r$data$sentiment else NA_integer_,
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

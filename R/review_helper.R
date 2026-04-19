# ─── Intermediate helpers for the review pipeline ────────────────────────────
# Internal functions shared between batch and real-time review workflows.
# Most of these are not exported — use the functions in review.R instead.

# ─── Request body builders ────────────────────────────────────────────────────

#' Build a responses API request body for competency extraction
#'
#' @param evaluation_text Evaluation text string
#' @param prompt System prompt (extraction instructions)
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_extract_body <- function(evaluation_text, prompt) {
  list(
    instructions = prompt,
    input = paste0(evaluation_text, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 10000L
  )
}

#' Build a responses API request body for competency scoring
#'
#' @param extractions List of extraction items; each has cID (integer) and
#'   text (character vector)
#' @param prompt System prompt (scoring instructions)
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_score_body <- function(extractions, prompt) {
  user_msg <- jsonlite::toJSON(
    list(extractions = lapply(extractions, function(item) {
      list(cID = item$cID, text = as.list(item$text))
    })),
    auto_unbox = TRUE
  )
  list(
    instructions = prompt,
    input = paste0(user_msg, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 4000L
  )
}

# ─── LLM call wrappers ───────────────────────────────────────────────────────

#' Extract competencies and verbatim text from a clerkship evaluation
#'
#' Calls the Azure responses API and parses the JSON output into a structured
#' extraction list. Shared by llm_comp_extract_run() and used as the real-time
#' counterpart of the batch extraction workflow.
#'
#' @param evaluation_text Character string with the evaluation text
#' @param prompt System prompt (extraction instructions)
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON
#' @returns List with:
#'   - statusCode: 0 = API error, 1 = parse error, 2 = success
#'   - data: list of extraction items on success (each has cID and text), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_extract <- function(
  evaluation_text,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_extract_body(evaluation_text, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("extractions" %in% names(parsed))) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  data <- parsed$extractions
  for (i in seq_along(data)) data[[i]]$text <- unlist(data[[i]]$text)

  list(
    statusCode = 2, data = data,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

#' Score extracted competencies from a clerkship evaluation
#'
#' Takes the extracted competency texts and asks the LLM to assign a
#' specificity score to each, plus overall utility and sentiment ratings.
#' Shared by llm_comp_score_run() and used as the real-time counterpart of
#' the batch scoring workflow.
#'
#' @param extractions List of extraction items from llm_comp_extract()$data;
#'   each element has cID (integer) and text (character vector)
#' @param prompt System prompt (scoring instructions)
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON toJSON
#' @returns List with:
#'   - statusCode: 0 = API error, 1 = parse error, 2 = success
#'   - data: list with competencies, utility and sentiment on success, NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_score <- function(
  extractions,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_score_body(extractions, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (
    is.null(parsed) ||
      !all(c("competencies", "utility", "sentiment") %in% names(parsed))
  ) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  list(
    statusCode = 2, data = parsed,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

# ─── DB fetch helpers ─────────────────────────────────────────────────────────

#' Fetch review info and evaluation text for the extraction step
#'
#' Joins review_assignment with the extraction prompt and full evaluation text.
#' Filters to statusCode == 0 unless force = TRUE.
#' Used by both llm_comp_extract_run() and llm_comp_extract_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with columns review_id, evaluation_id, evaluation, prompt,
#'   or NULL if there is nothing to process
db_fetch_review_extract <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, evaluation_id, prompt_extract_id) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_extract_id = id, prompt),
      by = "prompt_extract_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 0)) == 0) {
      warning(
        "No new review assignments to process (statusCode == 0). ",
        "Use force = TRUE to reprocess."
      )
      return(NULL)
    }
    not_new <- review_info$review_id[review_info$statusCode != 0]
    if (length(not_new) > 0) {
      warning(
        length(not_new), " review_assignment(s) skipped (statusCode != 0): ",
        paste(not_new, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 0)
    }
  }

  review_info <- select(review_info, -statusCode)

  dbGetEvals(review_info$evaluation_id, conn) |>
    select(evaluation_id, evaluation) |>
    left_join(review_info, by = "evaluation_id")
}

#' Fetch review info for the scoring step
#'
#' Joins review_assignment with the scoring prompt. Filters to statusCode == 3
#' (extraction complete) unless force = TRUE.
#' Used by both llm_comp_score_run() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with columns review_id, prompt, or NULL if nothing to process
db_fetch_review_score <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, prompt_score_id) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_score_id = id, prompt),
      by = "prompt_score_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 3)) == 0) {
      warning(
        "No review assignments ready to score (statusCode == 3). ",
        "Run extraction first, or use force = TRUE."
      )
      return(NULL)
    }
    not_ready <- review_info$review_id[review_info$statusCode != 3]
    if (length(not_ready) > 0) {
      warning(
        length(not_ready), " review_assignment(s) skipped (statusCode != 3): ",
        paste(not_ready, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 3)
    }
  }

  select(review_info, -statusCode)
}

#' Fetch extracted competency texts for a set of review assignments
#'
#' Used by both llm_comp_score_run() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#'
#' @import dplyr
#' @returns Data frame with columns review_assignment_id, competency_id,
#'   competency_score_id, text_match
db_fetch_extractions <- function(conn, review_ids) {
  tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(review_ids)) |>
    select(review_assignment_id, competency_id, competency_score_id = id) |>
    left_join(
      tbl(conn, "competency_text") |> select(competency_score_id, text_match),
      by = "competency_score_id"
    ) |>
    collect()
}

# ─── DB write helpers ─────────────────────────────────────────────────────────

#' Update competency_score.specificity for a single review assignment
#'
#' Used by both llm_comp_score_run() and batch_score_process().
#'
#' @param conn DB connection
#' @param rid review_assignment ID
#' @param competencies List of items with cID and specificity fields
#' @param commit Commit the transaction. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
db_write_score_specificity <- function(conn, rid, competencies, commit = FALSE) {
  if (is.null(competencies) || length(competencies) == 0) {
    return(invisible(NULL))
  }

  existing <- tbl(conn, "competency_score") |>
    filter(review_assignment_id == local(rid)) |>
    collect()

  updates <- data.frame(
    competency_id = sapply(competencies, "[[", "cID"),
    specificity = sapply(competencies, "[[", "specificity"),
    stringsAsFactors = FALSE
  ) |>
    left_join(existing |> select(id, competency_id), by = "competency_id") |>
    select(id, specificity)

  if (nrow(updates) > 0) {
    tbl_update(updates, conn, "competency_score", returnData = FALSE, commit = commit)
  }
}

#' Record a submitted batch job and link review assignments
#'
#' Inserts a batch record, creates batch_review links, and updates
#' review_assignment.statusCode for all submitted IDs.
#' Used by both llm_comp_extract_batch_submit() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param file_input_id File ID from llm_batch_upload()
#' @param batch_id Batch ID from llm_batch_create()
#' @param review_ids Integer vector of review_assignment IDs in this batch
#' @param review_status statusCode to set on linked review_assignments
#'
#' @importFrom sqlife tbl_insert tbl_update
#' @returns Inserted batch record data frame
db_record_batch <- function(conn, file_input_id, batch_id, review_ids, review_status) {
  batch_info <- tbl_insert(
    data.frame(
      file_input_id = file_input_id,
      batch_id = batch_id,
      statusCode = 1L,
      n_requests = length(review_ids)
    ),
    conn, "batch"
  )

  tbl_insert(
    data.frame(batch_id = batch_info$id, review_assignment_id = review_ids),
    conn, "batch_review", returnData = FALSE
  )

  tbl_update(
    data.frame(
      id = review_ids,
      statusCode = review_status,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    conn, "review_assignment", returnData = FALSE, commit = TRUE
  )

  batch_info
}

# ─── Batch status and result parsing ─────────────────────────────────────────

#' Check and update the status of a batch job
#'
#' Queries the Azure batch API and updates the batch table in the database.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#' @param check Query the API for an updated status. Default = TRUE
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#'
#' @import httr2
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
llm_batch_status <- function(
  batch_id,
  conn,
  check = TRUE,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  batch_info <- tbl(conn, "batch") |> filter(id == local(batch_id)) |> collect()

  if (!batch_info$statusCode %in% c(1, 2) || !check) return(batch_info)

  status <- c(
    "validating" = 2, "in_progress" = 2, "finalizing" = 2,
    "completed" = 3, "failed" = -1, "expired" = -2,
    "cancelling" = -3, "cancelled" = -3
  )

  resp <- request(paste0(endpoint, "/openai/v1/batches/", batch_info$batch_id)) |>
    req_headers("api-key" = api_key) |>
    req_error(is_error = ~FALSE) |>
    req_perform() |>
    resp_body_json()

  statusCode <- as.integer(status[names(status) == resp$status])

  batch_update <- data.frame(
    id = batch_id,
    file_output_id = resp$output_file_id,
    checked = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    statusCode = statusCode
  )

  if (statusCode == 3 && !is.null(resp$completed_at)) {
    batch_update$finished <- format(
      as.POSIXct(resp$completed_at, origin = "1970-01-01"),
      "%Y-%m-%d %H:%M:%S"
    )
  }

  tbl_update(batch_update, conn, "batch", commit = TRUE)
}

#' Parse raw batch API output into a structured list
#'
#' Converts the raw JSONL output from llm_batch_results() into a named list
#' keyed by custom_id, with review_id, statusCode, parsed data, and token counts.
#' Used by both batch_extract_process() and batch_score_process().
#'
#' @param file_output_id output_file_id from the completed batch status object
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract
#' @returns Named list (one element per custom_id) with:
#'   - review_id: integer extracted from the custom_id
#'   - statusCode: -1 = missing, -2 = parse error, 2 = success
#'   - data: parsed JSON object on success, NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
batch_results_preprocess <- function(file_output_id) {
  raw <- llm_batch_results(file_output_id)

  lapply(names(raw), function(id) {
    r <- raw[[id]]
    review_id <- str_extract(id, "\\d+$") |> as.integer()

    if (is.null(r)) {
      return(list(
        review_id = review_id, statusCode = -1,
        data = NULL, tokens_in = NA, tokens_out = NA
      ))
    }

    rb <- r$response$body
    raw_text <- rb$output[[1]]$content[[1]]$text
    tokens_in <- rb$usage$input_tokens
    tokens_out <- rb$usage$output_tokens

    parsed <- tryCatch(
      fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed)) {
      return(list(
        review_id = review_id, statusCode = -2,
        data = NULL, tokens_in = tokens_in, tokens_out = tokens_out
      ))
    }

    list(
      review_id = review_id, statusCode = 2,
      data = parsed, tokens_in = tokens_in, tokens_out = tokens_out
    )
  }) |>
    setNames(names(raw))
}

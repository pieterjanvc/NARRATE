# ─── Low-level batch helpers ──────────────────────────────────────────────────

#' Build JSONL content for a batch of chat completion requests
#'
#' @param requests Named list; names become custom_ids, each element is a
#'   chat completions body (messages, response_format, token params, etc.)
#' @param model Azure deployment name (must be the same for all requests)
#' @returns Single character string (JSONL, one JSON object per line)
llm_batch_build_jsonl <- function(requests, model) {
  lines <- mapply(
    function(body, id) {
      body$model <- model
      jsonlite::toJSON(
        list(
          custom_id = id,
          method = "POST",
          url = "/v1/responses",
          body = body
        ),
        auto_unbox = TRUE
      )
    },
    requests,
    names(requests),
    SIMPLIFY = TRUE
  )
  paste(lines, collapse = "\n")
}

#' Upload a JSONL file to Azure OpenAI Files API
#'
#' @param jsonl_content Output of llm_batch_build_jsonl()
#' @returns File ID string (e.g. "file-abc123")
llm_batch_upload <- function(
  jsonl_content,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp))
  writeLines(jsonl_content, tmp, useBytes = TRUE)

  resp <- request(paste0(endpoint, "/openai/v1/files")) |>
    req_headers("api-key" = api_key) |>
    req_body_multipart(
      purpose = "batch",
      file = curl::form_file(tmp, type = "application/json")
    ) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (!resp_status(resp) %in% c(200, 201)) {
    stop("File upload failed: ", resp_body_string(resp))
  }
  resp_body_json(resp)$id
}

#' Submit a batch job
#'
#' @param file_input_id From llm_batch_upload()
#' @returns Batch ID string
llm_batch_create <- function(
  file_input_id,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  resp <- request(paste0(endpoint, "/openai/v1/batches")) |>
    req_headers("api-key" = api_key, "Content-Type" = "application/json") |>
    req_body_json(list(
      input_file_id = file_input_id,
      endpoint = "/responses",
      completion_window = "24h"
    )) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop("Batch creation failed: ", resp_body_string(resp))
  }
  resp_body_json(resp)$id
}

#' Download and parse a batch output file
#'
#' @param file_output_id output_file_id from the completed batch status object
#' @returns Named list keyed by custom_id; each element is the full parsed
#'   response object (response$body, response$status_code, error)
llm_batch_results <- function(
  file_output_id,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  resp <- request(paste0(
    endpoint,
    "/openai/v1/files/",
    file_output_id,
    "/content"
  )) |>
    req_headers("api-key" = api_key) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop("Failed to fetch results: ", resp_body_string(resp))
  }

  lines <- strsplit(resp_body_string(resp), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  rows <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)

  setNames(rows, vapply(rows, "[[", character(1), "custom_id"))
}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(x, y) if (!is.null(x)) x else y


# ─── llm_comp_extract batch wrapper ──────────────────────────────────────────

#' Batch version of llm_comp_extract
#'
#' Submits all evaluations in a single batch job and returns results in the
#' same format as calling llm_comp_extract() individually on each.
#'
#' @param evaluation_texts Named character vector. Names used as custom_ids;
#'   unnamed vectors get ids "eval-1", "eval-2", etc.
#' @param prompt System prompt
#' @param model Azure deployment name
#' @param endpoint Azure endpoint
#' @param poll_interval Seconds between status polls
#'
#' @returns Named list (same names as evaluation_texts); each element matches
#'   llm_comp_extract() output: statusCode, data, tokens_in, tokens_out
#' @export
llm_comp_extract_batch <- function(
  evaluation_texts,
  prompt,
  model = "gpt-5-mini",
  endpoint = "https://azure-ai.hms.edu",
  poll_interval = 60
) {
  if (is.null(names(evaluation_texts))) {
    names(evaluation_texts) <- paste0("eval-", seq_along(evaluation_texts))
  }

  requests <- lapply(evaluation_texts, function(text) {
    list(
      instructions = prompt,
      input = paste0(text, "\n\nRespond with JSON as instructed."),
      text = list(format = list(type = "json_object")),
      max_output_tokens = 10000L
    )
  })

  message("Uploading ", length(requests), " requests...")
  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model),
    endpoint
  )

  message("Creating batch job...")
  batch_id <- llm_batch_create(file_input_id, endpoint)

  final <- llm_batch_wait(batch_id, poll_interval, endpoint)

  if (tolower(final$status) != "completed") {
    stop("Batch ended with status: ", final$status)
  }

  raw <- llm_batch_results(final$output_file_id, endpoint)

  # Parse into same structure as llm_comp_extract()
  lapply(names(evaluation_texts), function(id) {
    r <- raw[[id]]

    if (is.null(r) || r$response$status_code != 200) {
      return(list(statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA))
    }

    rb <- r$response$body
    raw_text <- rb$output[[1]]$content[[1]]$text
    tokens_in <- rb$usage$input_tokens
    tokens_out <- rb$usage$output_tokens

    parsed <- tryCatch(
      jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed) || !("extractions" %in% names(parsed))) {
      return(list(
        statusCode = 1,
        data = NULL,
        tokens_in = tokens_in,
        tokens_out = tokens_out
      ))
    }

    data <- parsed$extractions
    for (i in seq_along(data)) {
      data[[i]]$text <- unlist(data[[i]]$text)
    }

    list(
      statusCode = 2,
      data = data,
      tokens_in = tokens_in,
      tokens_out = tokens_out
    )
  }) |>
    setNames(names(evaluation_texts))
}

#' Poll a batch job until it reaches a terminal state
#'
#' @param batch_id Batch ID to check / process
#' @param conn DB connection
#' @param check Use the API to check the status if not finished yet
#'
#' @importFrom stringr str_extract
#' @returns Data frame with batch info
#' @export
llm_batch_status <- function(
  batch_id,
  conn,
  check = T,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  batch_info <- tbl(conn, "batch") |> filter(id == local(batch_id)) |> collect()

  if (!batch_info$statusCode %in% c(1, 2) | !check) {
    return(batch_info)
  }

  # https://developers.openai.com/api/docs/guides/batch#4-check-the-status-of-a-batch
  status <- c(
    "validating" = 2,
    "in_progress" = 2,
    "finalizing" = 2,
    "completed" = 3,
    "failed" = -1,
    "expired" = -2,
    "cancelling" = -3,
    "cancelled" = -3
  )

  resp <- request(paste0(
    endpoint,
    "/openai/v1/batches/",
    batch_info$batch_id
  )) |>
    req_headers("api-key" = api_key) |>
    req_error(is_error = ~FALSE) |>
    req_perform() |>
    resp_body_json()

  statusCode = as.integer(status[names(status) == resp$status])

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

  tbl_update(batch_update, conn, "batch", commit = T)
}

#' Pre-process batch API data for use in this project
#'
#' @param file_output_id Batch job output_file_id to process
#'
#' @importFrom stringr str_extract
#' @returns Data frame with batch info
batch_results_preprocess <- function(file_output_id) {
  raw <- llm_batch_results(file_output_id)

  lapply(names(raw), function(id) {
    r <- raw[[id]]
    review_id <- str_extract(id, "\\d+$") |> as.integer()

    if (is.null(r)) {
      return(list(
        review_id = review_id,
        statusCode = -1,
        data = NULL,
        tokens_in = NA,
        tokens_out = NA
      ))
    }

    rb <- r$response$body
    raw_text <- rb$output[[1]]$content[[1]]$text
    tokens_in <- rb$usage$input_tokens
    tokens_out <- rb$usage$output_tokens

    parsed <- tryCatch(
      jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed)) {
      return(list(
        review_id = review_id,
        statusCode = -2,
        data = NULL,
        tokens_in = tokens_in,
        tokens_out = tokens_out
      ))
    }

    list(
      review_id = review_id,
      statusCode = 2,
      data = parsed,
      tokens_in = tokens_in,
      tokens_out = tokens_out
    )
  }) |>
    setNames(names(raw))
}

#' Process batch extraction data
#'
#' @param batch_id Batch ID to process the competency extraction for
#' @param conn DB connection
#'
#' @importFrom stringr str_extract
#' @returns Data frame with batch info
batch_extract_process <- function(batch_id, conn) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode == 3) {
    results <- batch_results_preprocess(batch_info$file_output_id)

    success <- sapply(results, "[[", "statusCode") == 2

    to_update <- lapply(
      results,
      "[",
      c("review_id", "statusCode", "tokens_in", "tokens_out")
    ) |>
      bind_rows() |>
      mutate(
        statusCode = ifelse(statusCode == 2, 3, -1),
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ) |>
      rename(id = "review_id")

    tbl_update(to_update, conn, "review_assignment", returnData = F, commit = F)

    comp_data <- lapply(results, "[[", c("data", "extractions"))
    comp_data <- comp_data[sapply(comp_data, length) > 0]

    to_insert <- data.frame(
      review_assignment_id = rep(
        as.integer(str_extract(names(comp_data), "\\d+$")),
        lengths(comp_data)
      ),
      competency_id = unlist(
        lapply(comp_data, sapply, "[[", "cID"),
        use.names = F
      )
    )

    comp_info <- tbl_insert(to_insert, conn, "competency_score", commit = F)

    to_insert <- do.call(
      rbind,
      lapply(names(comp_data), function(nm) {
        id <- as.integer(str_extract(nm, "\\d+$"))
        do.call(
          rbind,
          lapply(comp_data[[nm]], function(item) {
            data.frame(
              review_assignment_id = id,
              competency_id = item$cID,
              text_match = unlist(item$text),
              stringsAsFactors = FALSE
            )
          })
        )
      })
    ) |>
      left_join(
        comp_info |>
          select(competency_score_id = id, review_assignment_id, competency_id),
        by = c("review_assignment_id", "competency_id")
      ) |>
      select(competency_score_id, text_match)

    tbl_insert(to_insert, conn, "competency_text", returnData = F, commit = F)

    batch_update <- data.frame(
      id = batch_id,
      statusCode = 4,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    )

    batch_info <- tbl_update(batch_update, conn, "batch")
  } else {
    message("No results to process")
  }

  batch_info
}

#' Process batch scoring data
#'
#' @param batch_id Batch ID to process the competency scoring for
#' @param conn DB connection
#'
#' @importFrom stringr str_extract
#' @returns Data frame with batch info
#' @export
batch_score_process <- function(batch_id, conn) {
  batch_info <- llm_batch_status(batch_id, conn)

  if (batch_info$statusCode == 3) {
    results <- batch_results_preprocess(batch_info$file_output_id)

    success <- sapply(results, "[[", "statusCode") == 2

    # Build per-review update: statusCode, tokens, utility, sentiment
    to_update <- lapply(results, function(r) {
      row <- data.frame(
        id = r$review_id,
        statusCode = if (r$statusCode == 2) 5L else -1L,
        tokens_in = r$tokens_in,
        tokens_out = r$tokens_out,
        utility = if (r$statusCode == 2) r$data$utility else NA_integer_,
        sentiment = if (r$statusCode == 2) r$data$sentiment else NA_integer_,
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      row
    }) |>
      bind_rows()

    tbl_update(to_update, conn, "review_assignment", returnData = F, commit = F)

    # Update competency_score.specificity for each (review_assignment_id, competency_id)
    score_results <- results[success]

    if (length(score_results) > 0) {
      review_ids <- sapply(score_results, "[[", "review_id")

      existing_scores <- tbl(conn, "competency_score") |>
        filter(review_assignment_id %in% local(review_ids)) |>
        collect()

      comp_updates <- do.call(
        rbind,
        lapply(score_results, function(r) {
          comps <- r$data$competencies
          if (is.null(comps) || length(comps) == 0) {
            return(NULL)
          }
          data.frame(
            review_assignment_id = r$review_id,
            competency_id = sapply(comps, "[[", "cID"),
            specificity = sapply(comps, "[[", "specificity"),
            stringsAsFactors = FALSE
          )
        })
      )

      if (!is.null(comp_updates) && nrow(comp_updates) > 0) {
        comp_updates <- comp_updates |>
          left_join(
            existing_scores |> select(id, review_assignment_id, competency_id),
            by = c("review_assignment_id", "competency_id")
          ) |>
          select(id, specificity)

        tbl_update(
          comp_updates,
          conn,
          "competency_score",
          returnData = F,
          commit = F
        )
      }
    }

    batch_update <- data.frame(
      id = batch_id,
      statusCode = 4,
      tokens_in = sum(to_update$tokens_in, na.rm = TRUE),
      tokens_out = sum(to_update$tokens_out, na.rm = TRUE)
    )

    batch_info <- tbl_update(batch_update, conn, "batch")
  } else {
    message("No results to process")
  }

  batch_info
}

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
          url = "/v1/chat/completions",
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
#' @param file_id From llm_batch_upload()
#' @returns Batch ID string
llm_batch_create <- function(
  file_id,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  resp <- request(paste0(endpoint, "/openai/v1/batches")) |>
    req_headers("api-key" = api_key, "Content-Type" = "application/json") |>
    req_body_json(list(
      input_file_id = file_id,
      endpoint = "/chat/completions",
      completion_window = "24h"
    )) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop("Batch creation failed: ", resp_body_string(resp))
  }
  resp_body_json(resp)$id
}

#' Poll a batch job until it reaches a terminal state
#'
#' @param batch_id From llm_batch_create()
#' @param poll_interval Seconds between polls (Azure recommends >= 60)
#' @returns Final batch status object (list)
llm_batch_wait <- function(
  batch_id,
  poll_interval = 60,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  terminal <- c("completed", "failed", "expired", "cancelled")

  repeat {
    resp <- request(paste0(endpoint, "/openai/v1/batches/", batch_id)) |>
      req_headers("api-key" = api_key) |>
      req_error(is_error = ~FALSE) |>
      req_perform()

    obj <- resp_body_json(resp)
    status <- tolower(obj$status)
    counts <- obj$request_counts

    message(sprintf(
      "[%s] %s — %s  (%s/%s completed)",
      format(Sys.time(), "%H:%M:%S"),
      batch_id,
      status,
      counts$completed %||% "?",
      counts$total %||% "?"
    ))

    if (status %in% terminal) {
      return(obj)
    }
    Sys.sleep(poll_interval)
  }
}

#' Download and parse a batch output file
#'
#' @param file_id output_file_id from the completed batch status object
#' @returns Named list keyed by custom_id; each element is the full parsed
#'   response object (response$body, response$status_code, error)
llm_batch_results <- function(
  file_id,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  resp <- request(paste0(endpoint, "/openai/v1/files/", file_id, "/content")) |>
    req_headers("api-key" = api_key) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop("Failed to fetch results: ", resp_body_string(resp))
  }

  lines <- strsplit(resp_body_string(resp), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  rows <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  setNames(rows, vapply(rows, `[[`, character(1), "custom_id"))
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

  is_gpt4o <- grepl("gpt-4o", model)
  token_param <- if (is_gpt4o) "max_tokens" else "max_completion_tokens"
  token_limit <- if (is_gpt4o) 800L else 4000L

  requests <- lapply(evaluation_texts, function(text) {
    body <- list(
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = text)
      ),
      response_format = list(type = "json_object")
    )
    if (is_gpt4o) {
      body$temperature <- 0
    }
    body[[token_param]] <- token_limit
    body
  })

  message("Uploading ", length(requests), " requests...")
  file_id <- llm_batch_upload(llm_batch_build_jsonl(requests, model), endpoint)

  message("Creating batch job...")
  batch_id <- llm_batch_create(file_id, endpoint)

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
    raw_text <- rb$choices[[1]]$message$content
    tokens_in <- rb$usage$prompt_tokens
    tokens_out <- rb$usage$completion_tokens

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

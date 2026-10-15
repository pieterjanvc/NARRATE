# ─── Generic OpenAI API functions (HUIT ais-openai-direct gateway) ────────────
# No project-specific logic. All functions are independent of the NARRATE project.

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Default OpenAI endpoint (HUIT ais-openai-direct v2 gateway)
llm_default_endpoint <- "https://go.apis.huit.harvard.edu/ais-openai-direct/v2"

# Default OpenAI model used by all LLM functions
llm_default_model <- "gpt-6-luna"

#' Build a base request to the OpenAI gateway
#'
#' Note that the default API key is read from the environment variable
#' HUIT_API_NARRATE. You can set this up using
#' `Sys.setenv(HUIT_API_NARRATE = "API token here")`
#'
#' @param path API path relative to the endpoint, e.g. "/responses"
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#' @param api_key Default = HUIT_API_NARRATE env var
#'
#' @import httr2
#' @returns httr2 request with URL and api-key header set
llm_request <- function(
  path,
  endpoint = llm_default_endpoint,
  api_key = Sys.getenv("HUIT_API_NARRATE")
) {
  request(paste0(endpoint, path)) |>
    req_headers("api-key" = api_key)
}

#' Extract the output text from a responses API result
#'
#' Reasoning models can return a reasoning item before the message, so the
#' text is taken from the first item of type "message" rather than output[[1]]
#'
#' @param resp Parsed responses API result
#' @returns Character string, or NULL if no message text is present
llm_output_text <- function(resp) {
  for (item in resp$output) {
    if (identical(item$type, "message")) {
      return(item$content[[1]]$text)
    }
  }
  NULL
}

# ─── Real-time API calls ──────────────────────────────────────────────────────

#' Call the OpenAI responses API
#'
#' Note that this function expects an environment variable HUIT_API_NARRATE
#' that contains the API key. You can set this up using
#' `Sys.setenv(HUIT_API_NARRATE = "API token here")`
#'
#' @param input User input text
#' @param instructions System instructions. Default = "You are a helpful AI assistant"
#' @param log If set, token usage is appended to this CSV file
#' @param model Default = llm_default_model. OpenAI model name
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#'
#' @import httr2
#' @returns Parsed response list
#' @export
llm_responses <- function(
  input,
  instructions,
  log,
  model = llm_default_model,
  endpoint = llm_default_endpoint
) {
  instructions <- ifelse(
    missing(instructions),
    "You are a helpful AI assistant",
    instructions
  )

  req <- llm_request("/responses", endpoint) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      model = model,
      input = input,
      instructions = instructions
    )) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$input_tokens,
        resp$usage$output_tokens
      ),
      log,
      append = TRUE
    )
  }

  resp
}

#' Call the OpenAI chat completions API
#'
#' Note that this function expects an environment variable HUIT_API_NARRATE
#' that contains the API key. You can set this up using
#' `Sys.setenv(HUIT_API_NARRATE = "API token here")`
#'
#' @param user User prompt
#' @param system System prompt. Default = "You are a helpful AI assistant"
#' @param log If set, token usage is appended to this CSV file
#' @param model Default = llm_default_model. OpenAI model name
#' @param maxTokens Default = 500. Maximum tokens to return
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#'
#' @import httr2
#' @returns Parsed response list
#' @export
llm_chat_completion <- function(
  user,
  system,
  log,
  model = llm_default_model,
  maxTokens = 500,
  endpoint = llm_default_endpoint
) {
  system <- ifelse(missing(system), "You are a helpful AI assistant", system)

  req <- llm_request("/chat/completions", endpoint) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system),
        list(role = "user", content = user)
      ),
      max_completion_tokens = maxTokens
    )) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$prompt_tokens,
        resp$usage$completion_tokens
      ),
      log,
      append = TRUE
    )
  }

  resp
}

# ─── Batch API helpers ────────────────────────────────────────────────────────

#' Build JSONL content for a batch of responses API requests
#'
#' @param requests Named list; names become custom_ids, each element is a
#'   responses API body (instructions, input, text format params, etc.)
#' @param model OpenAI model name (added to every request body)
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

#' Upload a JSONL file to the OpenAI Files API
#'
#' @param jsonl_content Output of llm_batch_build_jsonl()
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#' @param api_key API key. Default = HUIT_API_NARRATE env var
#' @returns File ID string
llm_batch_upload <- function(
  jsonl_content,
  endpoint = llm_default_endpoint,
  api_key = Sys.getenv("HUIT_API_NARRATE")
) {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp))
  writeLines(jsonl_content, tmp, useBytes = TRUE)

  resp <- llm_request("/files", endpoint, api_key) |>
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
#' @param file_input_id File ID from llm_batch_upload()
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#' @param api_key API key. Default = HUIT_API_NARRATE env var
#' @returns Batch ID string
llm_batch_create <- function(
  file_input_id,
  endpoint = llm_default_endpoint,
  api_key = Sys.getenv("HUIT_API_NARRATE")
) {
  resp <- llm_request("/batches", endpoint, api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(list(
      input_file_id = file_input_id,
      endpoint = "/v1/responses",
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
#' @param endpoint Default = llm_default_endpoint. Gateway base URL
#' @param api_key API key. Default = HUIT_API_NARRATE env var
#' @returns Named list keyed by custom_id; each element is the parsed response object
llm_batch_results <- function(
  file_output_id,
  endpoint = llm_default_endpoint,
  api_key = Sys.getenv("HUIT_API_NARRATE")
) {
  resp <- llm_request(
    paste0("/files/", file_output_id, "/content"),
    endpoint,
    api_key
  ) |>
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

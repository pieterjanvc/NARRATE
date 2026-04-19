# ─── Generic Azure OpenAI API functions ──────────────────────────────────────
# No project-specific logic. All functions are independent of the CFME project.

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ─── Real-time API calls ──────────────────────────────────────────────────────

#' Call the Azure OpenAI responses API
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key. You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param input User input text
#' @param instructions System instructions. Default = "You are a helpful AI assistant"
#' @param log If set, token usage is appended to this CSV file
#' @param model Default = gpt-5-mini. Azure deployment name
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint base URL
#'
#' @import httr2
#' @returns Parsed response list
#' @export
llm_responses <- function(
  input,
  instructions,
  log,
  model = "gpt-5-mini",
  endpoint = "https://azure-ai.hms.edu"
) {
  instructions <- ifelse(
    missing(instructions), "You are a helpful AI assistant", instructions
  )

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(list(model = model, input = input, instructions = instructions)) |>
    req_perform()

  if (resp_status(req) != 200) stop(req)

  resp <- resp_body_json(req)

  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$input_tokens,
        resp$usage$output_tokens
      ),
      log, append = TRUE
    )
  }

  resp
}

#' Call the Azure OpenAI chat completions API
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key. You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param user User prompt
#' @param system System prompt. Default = "You are a helpful AI assistant"
#' @param log If set, token usage is appended to this CSV file
#' @param model Default = gpt-4o-1120. Azure deployment name
#' @param maxTokens Default = 500. Maximum tokens to return
#' @param version Default = 2024-10-21. API version
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint base URL
#'
#' @import httr2
#' @returns Parsed response list
#' @export
llm_chat_completion <- function(
  user,
  system,
  log,
  model = "gpt-4o-1120",
  maxTokens = 500,
  version = "2024-10-21",
  endpoint = "https://azure-ai.hms.edu"
) {
  system <- ifelse(missing(system), "You are a helpful AI assistant", system)

  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint, model, version
  )

  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(list(
      messages = list(
        list(role = "system", content = system),
        list(role = "user", content = user)
      ),
      max_tokens = maxTokens
    )) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) stop(req)

  resp <- resp_body_json(req)

  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$prompt_tokens,
        resp$usage$completion_tokens
      ),
      log, append = TRUE
    )
  }

  resp
}

# ─── Batch API helpers ────────────────────────────────────────────────────────

#' Build JSONL content for a batch of responses API requests
#'
#' @param requests Named list; names become custom_ids, each element is a
#'   responses API body (instructions, input, text format params, etc.)
#' @param model Azure deployment name (added to every request body)
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
    requests, names(requests),
    SIMPLIFY = TRUE
  )
  paste(lines, collapse = "\n")
}

#' Upload a JSONL file to the Azure OpenAI Files API
#'
#' @param jsonl_content Output of llm_batch_build_jsonl()
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @returns File ID string
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
#' @param file_input_id File ID from llm_batch_upload()
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
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
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#' @returns Named list keyed by custom_id; each element is the parsed response object
llm_batch_results <- function(
  file_output_id,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  resp <- request(
    paste0(endpoint, "/openai/v1/files/", file_output_id, "/content")
  ) |>
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

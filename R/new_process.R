library(httr2)
library(jsonlite)
library(dplyr)

#' Extract competencies and verbatim text from a clerkship evaluation
#'
#' @param evaluation_text Character string with the evaluation text
#' @param prompt System prompt (contents of inst/extractPrompt.md)
#' @param model Azure deployment name
#' @param endpoint Azure endpoint
#' @param debug (Default = F) Return the raw model output as well
#'
#' @returns List with:
#'   - statusCode: 0 = API error, 1 = parse error, 2 = success
#'   - data: data.frame with columns cID and text_match (on success), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response string for debugging
#' @export
llm_comp_extract <- function(
  evaluation_text,
  prompt,
  model = "gpt-5-mini",
  endpoint = "https://azure-ai.hms.edu",
  version = "2024-10-21",
  debug = F
) {
  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint,
    model,
    version
  )

  # gpt-4o uses max_tokens + supports temperature;
  # gpt-5-mini uses max_completion_tokens and does not accept temperature
  is_gpt4o <- grepl("gpt-4o", model)
  token_param <- if (is_gpt4o) "max_tokens" else "max_completion_tokens"

  body <- c(
    list(
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = evaluation_text)
      ),
      response_format = list(type = "json_object")
    ),
    if (is_gpt4o) list(temperature = 0) else list(),
    # gpt-4o: 800 output tokens is sufficient
    # gpt-5-mini: reasoning model — tokens cover internal thinking + output,
    # so budget must be large enough for both; 4000 leaves ~3200 for actual output
    setNames(list(if (is_gpt4o) 800L else 4000L), token_param)
  )

  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0,
      data = NULL,
      tokens_in = NA,
      tokens_out = NA,
      raw = if (debug) {
        resp_body_string(req)
      } else {
        NULL
      }
    ))
  }

  resp <- resp_body_json(req)
  raw <- resp$choices[[1]]$message$content
  tokens_in <- resp$usage$prompt_tokens
  tokens_out <- resp$usage$completion_tokens

  # Parse and validate
  parsed <- tryCatch(
    fromJSON(raw, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("extractions" %in% names(parsed))) {
    return(list(
      statusCode = 1,
      data = NULL,
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      raw = if (debug) {
        resp_body_string(req)
      } else {
        NULL
      }
    ))
  }

  # Flatten multiple pieces of text into a vector
  data <- parsed$extractions
  for (i in 1:length(data)) {
    data[[i]]$text <- unlist(data[[i]]$text)
  }

  list(
    statusCode = 2,
    data = data,
    tokens_in = tokens_in,
    tokens_out = tokens_out,
    raw = if (debug) {
      resp_body_string(req)
    } else {
      NULL
    }
  )
}

#' Parse and validate the extraction output
#'
#' Checks that:
#' - cID values are integers in 1:8
#' - text_match values are non-empty strings
#' - no duplicate (cID, text_match) pairs
#'
#' @param result Output of llm_comp_extract()
#' @returns List with statusCode (2 = valid, 1 = validation issue) and data
#' @export
comp_extraction_validate <- function(result, nComp = 8) {
  if (result$statusCode != 2) {
    return(list(success = nchar(issues == 0), issues = "LLM step failed"))
  }
  d <- result$data

  if (length(d) == 0) {
    return(list(success = T, issues = "No competencies detected"))
  }

  issues <- NULL

  check <- sapply(data, "[[", "cID") %in% 1:nComp

  if (!all(check)) {
    issues <- c(
      issues,
      paste(
        "cID out of range for items:",
        paste(which(!check), collapse = ", ")
      )
    )
  }

  check <- sapply(data, "[[", "text") |>
    sapply(function(x) {
      any(nchar(x) == 0)
    })

  if (!all(!check)) {
    issues <- c(
      issues,
      paste(
        "Empty text selection for items:",
        paste(which(check), collapse = ", ")
      )
    )
  }

  if (length(issues) > 0) {
    message("Extraction validation: ", paste(issues, collapse = "; "))
  }

  return(list(success = is.null(issues), issues = issues))
}

#' Submit a batch competency extraction job
#'
#' Builds and uploads a batch of competency extraction requests for a set of
#' review assignments, creates the batch job, and records it in the database.
#'
#' @param conn Database connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param prompt_id Integer ID of the prompt to use (from the prompt table)
#' @param model Azure batch deployment name
#' @param endpoint Azure endpoint base URL
#' @param api_key The api key to use if not set in env
#' @param verbose Print progress messages (Default = FALSE)
#' @param force If TRUE, resubmit even if already in-progress or completed (Default = FALSE)
#'
#' @returns Invisibly, the result of tbl_insert() for the new batch record
#' @export
llm_comp_extract_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = F,
  force = F
) {
  # Get the evaluations + extract prompt for the given review IDs
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, evaluation_id, prompt_extract_id) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_extract_id = id, prompt),
      by = "prompt_extract_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(review_info |> filter(statusCode == 0)) == 0) {
      warning(
        "No new review assignments to submit (statusCode == 0). Use force = TRUE to resubmit."
      )
      return(NULL)
    }

    not_new <- review_info$review_id[review_info$statusCode != 0]
    if (length(not_new) > 0) {
      warning(
        length(not_new),
        " review_assignment(s) skipped — already submitted or processed ",
        "(statusCode != 0): ",
        paste(not_new, collapse = ", ")
      )
      review_info <- review_info |> filter(statusCode == 0)
    }
  }

  review_info <- review_info |> select(-statusCode)

  review_info <- dbGetEvals(review_info$evaluation_id, conn) |>
    select(evaluation_id, evaluation) |>
    left_join(
      review_info,
      by = c("evaluation_id")
    )

  review_info$name <- paste0("review-", review_info$review_id)

  # Build the LLM API request
  requests <- apply(review_info, 1, function(x) {
    body <- list(
      messages = list(
        list(role = "system", content = x["prompt"]),
        list(role = "user", content = x["evaluation"])
      ),
      response_format = list(type = "json_object")
    )
    body
  }) |>
    setNames(review_info$name)

  if (verbose) {
    message("Uploading ", length(requests), " requests...")
  }

  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model),
    endpoint
  )

  if (verbose) {
    message("Creating batch job...")
  }
  batch_id <- llm_batch_create(file_input_id, endpoint)

  # Update the request in the DB batch table
  batch_info <- tbl_insert(
    data.frame(
      file_input_id = file_input_id,
      batch_id = batch_id,
      statusCode = 1,
      n_requests = nrow(review_info)
    ),
    conn,
    "batch"
  )

  submitted_ids <- review_info$review_id

  # Link review assignments to this batch
  tbl_insert(
    data.frame(
      batch_id = batch_info$id,
      review_assignment_id = submitted_ids
    ),
    conn,
    "batch_review",
    returnData = F
  )

  # Mark submitted review assignments as in-progress
  tbl_update(
    data.frame(
      id = submitted_ids,
      statusCode = 1,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    conn,
    "review_assignment",
    returnData = F,
    commit = T
  )

  batch_info
}

#' Submit a batch competency scoring job
#'
#' Builds and uploads a batch of competency scoring requests for a set of
#' review assignments, creates the batch job, and records it in the database.
#'
#' @param conn Database connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure batch deployment name
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages (Default = FALSE)
#' @param force If TRUE, resubmit even if already in-progress or completed (Default = FALSE)
#'
#' @returns Invisibly, the result of tbl_insert() for the new batch record
#' @export
llm_comp_score_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  verbose = F,
  force = F
) {
  # Get score prompt per review assignment, checking extraction is complete
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, prompt_score_id) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_score_id = id, prompt),
      by = "prompt_score_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(review_info |> filter(statusCode == 3)) == 0) {
      warning(
        "There are no review assignments with these IDs to score (statusCode == 3). ",
        "Run batch_extract_process() first, or use force = TRUE to resubmit."
      )
      return(NULL)
    }

    not_ready <- review_info$review_id[review_info$statusCode != 3]
    if (length(not_ready) > 0) {
      warning(
        length(not_ready),
        " review_assignment(s) skipped — extraction not complete or already scored ",
        "(statusCode != 3): ",
        paste(not_ready, collapse = ", ")
      )

      review_info <- review_info |> filter(statusCode == 3)
    }
  }

  review_info <- review_info |> select(-statusCode)

  # Fetch extracted competency texts for the given review assignments
  ready_ids <- review_info$review_id
  extractions <- tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(ready_ids)) |>
    select(review_assignment_id, competency_id, competency_score_id = id) |>
    left_join(
      tbl(conn, "competency_text") |> select(competency_score_id, text_match),
      by = "competency_score_id"
    ) |>
    collect()

  # Build the LLM API requests
  requests <- setNames(
    lapply(review_info$review_id, function(rid) {
      rows <- extractions[extractions$review_assignment_id == rid, ]
      comps <- lapply(
        split(rows, rows$competency_id),
        function(g) {
          list(
            cID = g$competency_id[[1]],
            text = as.list(g$text_match)
          )
        }
      )
      user_msg <- toJSON(
        list(extractions = unname(comps)),
        auto_unbox = TRUE
      )
      prompt <- review_info$prompt[review_info$review_id == rid]
      list(
        messages = list(
          list(role = "system", content = prompt),
          list(role = "user", content = user_msg)
        ),
        response_format = list(type = "json_object")
      )
    }),
    paste0("review-", review_info$review_id)
  )

  if (verbose) {
    message("Uploading ", length(requests), " requests...")
  }

  file_input_id <- llm_batch_upload(
    llm_batch_build_jsonl(requests, model),
    endpoint
  )

  if (verbose) {
    message("Creating batch job...")
  }
  batch_id <- llm_batch_create(file_input_id, endpoint)

  batch_info <- tbl_insert(
    data.frame(
      file_input_id = file_input_id,
      batch_id = batch_id,
      statusCode = 1,
      n_requests = nrow(review_info)
    ),
    conn,
    "batch"
  )

  # Link review assignments to this batch
  tbl_insert(
    data.frame(
      batch_id = batch_info$id,
      review_assignment_id = ready_ids
    ),
    conn,
    "batch_review",
    returnData = F
  )

  # Mark submitted review assignments as scoring in-progress
  tbl_update(
    data.frame(
      id = ready_ids,
      statusCode = 4,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    conn,
    "review_assignment",
    returnData = F,
    commit = T
  )

  batch_info
}

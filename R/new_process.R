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
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  version = "2024-10-21",
  debug = F
) {
  baseURL <- sprintf(
    "%s/openai/v1/responses",
    endpoint
  )

  body <- list(
    instructions = prompt,
    input = paste0(evaluation_text, "\n\nRespond with JSON as instructed."),
    model = model,
    text = list(format = list(type = "json_object")),
    # Reasoning model — tokens cover internal thinking + output,
    # so budget must be large enough for both; 10000 leaves ~8000+ for actual output
    max_output_tokens = 10000L
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
  raw <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

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

#' Score extracted competencies from a clerkship evaluation
#'
#' Takes the extracted competency texts (output of the extraction step) and
#' asks the LLM to assign a specificity score to each, plus overall utility
#' and sentiment ratings for the evaluation.
#'
#' @param extractions List of extractions as returned by llm_comp_extract()
#'   (i.e. result$data), where each element has cID (integer) and text
#'   (character vector)
#' @param prompt System prompt (contents of inst/prompt_comp_score.md)
#' @param model Azure deployment name
#' @param endpoint Azure endpoint
#' @param version API version
#' @param debug (Default = F) Return the raw model output as well
#'
#' @returns List with:
#'   - statusCode: 0 = API error, 1 = parse error, 2 = success
#'   - data: list with competencies (list of cID + specificity), utility,
#'     and sentiment (on success), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response string for debugging
#' @export
llm_comp_score <- function(
  extractions,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  version = "2025-03-01-preview",
  debug = F
) {
  baseURL <- sprintf("%s/openai/v1/responses", endpoint)

  user_msg <- toJSON(
    list(
      extractions = lapply(extractions, function(item) {
        list(cID = item$cID, text = as.list(item$text))
      })
    ),
    auto_unbox = TRUE
  )

  body <- list(
    model = model,
    instructions = prompt,
    input = paste0(user_msg, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 4000L
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
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (
    is.null(parsed) ||
      !all(c("competencies", "utility", "sentiment") %in% names(parsed))
  ) {
    return(list(
      statusCode = 1,
      data = NULL,
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  list(
    statusCode = 2,
    data = parsed,
    tokens_in = tokens_in,
    tokens_out = tokens_out,
    raw = if (debug) resp_body_string(req) else NULL
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
    list(
      instructions = x["prompt"],
      input = paste0(x["evaluation"], "\n\nRespond with JSON as instructed."),
      text = list(format = list(type = "json_object")),
      max_output_tokens = 10000L
    )
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

#' Run synchronous competency extraction for a set of review assignments
#'
#' Fetches evaluation text and prompts from the database, calls
#' llm_comp_extract() for each review assignment, and writes results back.
#' Unlike the batch workflow, this combines the submit and process steps into a
#' single call and does not use the batch or batch_review tables.
#'
#' @param conn Database connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages (Default = FALSE)
#' @param force If TRUE, reprocess even if already done (Default = FALSE)
#'
#' @returns Invisibly, a data frame summarizing results per review_id
#' @export
llm_comp_extract_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  verbose = F,
  force = F
) {
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
        "No new review assignments to process (statusCode == 0). Use force = TRUE to reprocess."
      )
      return(NULL)
    }

    not_new <- review_info$review_id[review_info$statusCode != 0]
    if (length(not_new) > 0) {
      warning(
        length(not_new),
        " review_assignment(s) skipped — already processed ",
        "(statusCode != 0): ",
        paste(not_new, collapse = ", ")
      )
      review_info <- review_info |> filter(statusCode == 0)
    }
  }

  review_info <- review_info |> select(-statusCode)

  review_info <- dbGetEvals(review_info$evaluation_id, conn) |>
    select(evaluation_id, evaluation) |>
    left_join(review_info, by = "evaluation_id")

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    if (verbose) {
      message("Processing review ", rid, "...")
    }

    result <- llm_comp_extract(
      evaluation_text = review_info$evaluation[i],
      prompt = review_info$prompt[i],
      model = model,
      endpoint = endpoint
    )

    new_status <- if (result$statusCode == 2) 3L else -1L

    # Insert competency data before updating review_assignment so all writes
    # can be committed together
    if (result$statusCode == 2 && length(result$data) > 0) {
      dbCompExtraction(conn, rid, result$data, commit = F)
    }

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn,
      "review_assignment",
      returnData = F,
      commit = T
    )

    data.frame(
      review_id = rid,
      statusCode = new_status,
      tokens_in = result$tokens_in,
      tokens_out = result$tokens_out
    )
  })

  bind_rows(results)
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
        instructions = prompt,
        input = paste0(user_msg, "\n\nRespond with JSON as instructed."),
        text = list(format = list(type = "json_object")),
        max_output_tokens = 4000L
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

#' Run synchronous competency scoring for a set of review assignments
#'
#' Fetches extracted competency texts from the database, calls llm_comp_score()
#' for each review assignment, and writes results back. Unlike the batch
#' workflow, this combines the submit and process steps into a single call and
#' does not use the batch or batch_review tables.
#'
#' @param conn Database connection
#' @param review_ids Integer vector of review_assignment IDs to process
#' @param model Azure deployment name
#' @param endpoint Azure endpoint base URL
#' @param verbose Print progress messages (Default = FALSE)
#' @param force If TRUE, reprocess even if already scored (Default = FALSE)
#'
#' @returns A data frame summarizing results per review_id
#' @export
llm_comp_score_run <- function(
  conn,
  review_ids,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  verbose = F,
  force = F
) {
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
        "Run llm_comp_extract_run() first, or use force = TRUE to reprocess."
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

  results <- lapply(seq_len(nrow(review_info)), function(i) {
    rid <- review_info$review_id[i]
    if (verbose) {
      message("Scoring review ", rid, "...")
    }

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

    # Update competency_score.specificity before committing review_assignment
    if (result$statusCode == 2) {
      comps <- result$data$competencies
      if (length(comps) > 0) {
        existing_scores <- tbl(conn, "competency_score") |>
          filter(review_assignment_id == local(rid)) |>
          collect()

        comp_updates <- data.frame(
          competency_id = sapply(comps, "[[", "cID"),
          specificity = sapply(comps, "[[", "specificity"),
          stringsAsFactors = FALSE
        ) |>
          left_join(
            existing_scores |> select(id, competency_id),
            by = "competency_id"
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

    tbl_update(
      data.frame(
        id = rid,
        statusCode = new_status,
        tokens_in = result$tokens_in,
        tokens_out = result$tokens_out,
        utility = if (result$statusCode == 2) {
          result$data$utility
        } else {
          NA_integer_
        },
        sentiment = if (result$statusCode == 2) {
          result$data$sentiment
        } else {
          NA_integer_
        },
        modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      conn,
      "review_assignment",
      returnData = F,
      commit = T
    )

    data.frame(
      review_id = rid,
      statusCode = new_status,
      tokens_in = result$tokens_in,
      tokens_out = result$tokens_out
    )
  })

  bind_rows(results)
}

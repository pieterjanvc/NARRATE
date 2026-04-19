#' Check if the LLM response is in the expected CSV format
#'
#' @param string To parse as CSV
#' @param reviewID (Optional) Review assingment ID to add to the output
#'
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite fromJSON
#'
#' @returns List with two elements
#'  - statusCode: 0 not valid CSV, 1 column names / number issue,
#'     2 column type error, 3 expected CSV format
#'  - data: List of 3 data frames with results for statusCode 3, NULL otherwise
#' @export
#'
llm_csv_response <- function(string, reviewID) {
  suppressWarnings({
    tryCatch(
      {
        # Try and load as list
        check <- fromJSON(string, simplifyVector = T, simplifyDataFrame = F)

        # Check the names
        if (!all(names(check) %in% c("compScores", "util", "sent"))) {
          return(list(statusCode = 1, data = NULL))
        }

        if (
          !all(names(check$compScores) %in% c("cID", "specificity", "text"))
        ) {
          return(list(statusCode = 1, data = NULL))
        }

        # Check integer values
        allInts <- all(
          unlist(c(
            sapply(check$compScores, "[[", "cID"),
            sapply(check$compScores, "[[", "specificity"),
            check[c("util", "sent")]
          )) %%
            1 ==
            0
        )
        if (!allInts) {
          return(list(statusCode = 2, data = NULL))
        }

        # Get global eval
        globalEval <- data.frame(util = check$util, sent = check$sent)

        # Get comp scores
        compScores <- data.frame(
          cID = sapply(check$compScores, "[[", "cID"),
          specificity = sapply(check$compScores, "[[", "specificity")
        )

        # Get comp text
        compText <- do.call(
          rbind,
          lapply(check$compScores, function(x) {
            data.frame(cID = x$cID, text_match = x$text)
          })
        )

        # Add review_assignment_id if provided
        if (!missing(reviewID)) {
          globalEval <- globalEval |> mutate(id = reviewID, .before = util)
          compScores <- compScores |> mutate(id = reviewID, .before = cID)
          compText <- compText |> mutate(id = reviewID, .before = cID)
        }

        return(list(
          statusCode = 3,
          data = list(
            overallScores = globalEval,
            compScores = compScores,
            compText = compText
          )
        ))
      },
      error = function(e) {
        return(list(statusCode = 0, data = NULL))
      }
    )
  })
}

#' Call an Azure chat completion LLM
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param user User prompt
#' @param system Default = "You are a helpful AI assistant". System prompt
#' @param log If set, the token usage is kept track of in this CSV file
#' @param model Default = gpt-4o-1120. LLM model to use
#' @param maxTokens Default = 500. Max Number of tokens to return
#' @param version Default = 2024-10-21.
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint
#'
#' @import httr2
#'
#' @returns LLM Response object (list).
#'
#' If a log file is et a line is added to a CSV file with columns
#' timestamp, promptTokens and responseTokens (headers not included)
#'
#' @export
#'
llm_chat_completion <- function(
  user,
  system,
  log,
  model = "gpt-4o-1120",
  maxTokens = 500,
  version = "2024-10-21",
  endpoint = "https://azure-ai.hms.edu"
) {
  # API info https://learn.microsoft.com/en-us/azure/ai-foundry/openai/reference

  # Build the URL
  baseURL <- sprintf(
    "%s/openai/deployments/%s/chat/completions?api-version=%s",
    endpoint,
    model,
    version
  )

  # Build the body
  system <- ifelse(missing(system), "You are a helpful AI assistant", system)
  body <- list(
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    ),
    max_tokens = maxTokens
  )

  # Send the request
  req <- request(baseURL) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  # Write token usage to log file if set
  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$prompt_tokens,
        resp$usage$completion_tokens
      ),
      log,
      append = T
    )
  }

  return(resp)
}

#' Call an Azure response LLM
#'
#' Note that this function expects an environment variable HMS_AZURE_API
#' that contains the API key You can set this up using
#' `Sys.setenv(HMS_AZURE_API = "API token here")`
#'
#' @param input User prompt
#' @param instructions Default = "You are a helpful AI assistant". System prompt
#' @param log If set, the token usage is kept track of in this CSV file
#' @param model Default = gpt-4o-1120. LLM model to use
#' @param endpoint Default = https://azure-ai.hms.edu. Azure endpoint
#'
#' @import httr2
#'
#' @returns LLM Response object (list).
#'
#' If a log file is et a line is added to a CSV file with columns
#' timestamp, promptTokens and responseTokens (headers not included)
#'
#' @export
#'
llm_responses <- function(
  input,
  instructions,
  log,
  model = "gpt-5-mini",
  endpoint = "https://azure-ai.hms.edu"
) {
  # API info https://learn.microsoft.com/en-us/azure/ai-foundry/openai/reference

  # Build the body
  instructions <- ifelse(
    missing(instructions),
    "You are a helpful AI assistant",
    instructions
  )

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(list(
      model = model,
      input = input,
      instructions = instructions
    )) |>
    # req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    stop(req)
  }

  resp <- resp_body_json(req)

  # Write token usage to log file if set
  if (!missing(log)) {
    write(
      sprintf(
        '"%s",%i,%i',
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        resp$usage$input_tokens,
        resp$usage$output_tokens
      ),
      log,
      append = T
    )
  }

  return(resp)
}

#' Get the data from an LLM evaluation
#'
#' @param conn Connection to a CFME database
#' @param review_assignment_ids IDs in review_assignment table
#' @param log (Optional) Save token usage to extra file (will also be in database)
#' @param maxTries (Default = 3) How many times to try in case the response is
#' not in a valid format
#'
#' @returns A list with results for each review_assignment_id
#' - statusCode = final status code (3 = success)
#' - review_assignment_id: ID from review_assignment table in database
#' - data: If successful, data frame with response, otherwise NULL
#' - tries: Number of times tried for valid response
#' - tokens in / out: token used
#' @export
#'
llm_review <- function(
  conn,
  review_assignment_ids,
  log,
  maxTries = 3
) {
  # Get assignment info
  assignments <- tbl(conn, "review_assignment") |>
    filter(id %in% review_assignment_ids, statusCode != 2) |>
    left_join(
      tbl(conn, "reviewer") |> select(id, model),
      by = c("reviewer_id" = "id")
    ) |>
    collect()

  check <- setdiff(review_assignment_ids, assignments$id)

  if (length(check) > 0) {
    stop(paste(
      "The following review_assignment_ids were not found",
      paste(check, collapse = ", ")
    ))
  }

  # Get all unique prompts needed
  prompts <- tbl(conn, "prompt") |>
    filter(id %in% local(unique(assignments$prompt_id))) |>
    select(id, prompt) |>
    collect()

  evals <- assignments |>
    select(id, evaluation_id, prompt_id, model) |>
    left_join(
      dbGetEvals(
        ids = assignments$evaluation_id,
        conn = conn,
        redacted = assignments$redacted,
        includeQuestions = assignments$include_questions
      ),
      by = "evaluation_id"
    )

  # Call LLM for each review
  info <- lapply(1:nrow(evals), function(j, log = log) {
    for (i in 1:maxTries) {
      # Actual LLM call
      tryCatch(
        {
          duration <- Sys.time()
          if (evals$model[j] %in% c("gpt-4o-1120")) {
            result <- llm_chat_completion(
              user = evals$evaluation[j],
              system = prompts |>
                filter(id == evals$prompt_id[j]) |>
                pull(prompt),
              model = evals$model[j]
            )

            result <- data.frame(
              content = result$choices[[1]]$message$content,
              tokens_in = result$usage$prompt_tokens,
              tokens_out = result$usage$completion_tokens
            )
          } else {
            result <- llm_responses(
              input = evals$evaluation[j],
              instructions = prompts |>
                filter(id == evals$prompt_id[j]) |>
                pull(prompt),
              model = evals$model[j]
            )

            result <- data.frame(
              content = result$output[[2]]$content[[1]]$text,
              tokens_in = result$usage$input_tokens,
              tokens_out = result$usage$output_tokens
            )
          }

          duration <- difftime(Sys.time(), duration, units = "sec") |>
            as.numeric() |>
            round(2)
        },
        error = function(e) {
          print(e)
        }
      )

      # Convert output CSV string to data frame (and check)
      output <- llm_csv_response(
        result$content,
        reviewID = evals$id[j]
      )

      # Add the response metadata
      output$review_assignment_id = evals$id[j]
      output$tokens_in = result$tokens_in
      output$tokens_out = result$tokens_out
      output$duration = duration = duration
      output$tries = i

      if (output$statusCode == 3) {
        break
      }
    }

    output
  })

  return(info)
}

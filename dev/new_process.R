# ARGUMENTS
# *********
seed <- 20260407
db_path <- "local/new_process.db"
dbSetup(db_path, "inst/cfme.sql")
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))

# SETUP
# *****

# Add all data
conn <- dbGetConn(db_path)
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)
. <- dbAddEvaluations(combined_data, db_path, redactedOnly = T)
# Add default AI reviewer
. <- dbReviewerAI(conn, model = formals(llm_responses)$model)
# Add default prompt
prompt <- readLines("inst/rubricPrompt.md") |> paste(collapse = "\n")
review_prompt_id <- dbAddPrompt(prompt, conn)

# Assign the same n random evals to each reviewer
set.seed(seed)
evalSample <-
  tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 3) |>
  pull(id)
assingments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)


prompt_comp_extract <- readLines("inst/prompt_comp_extract.md") |>
  paste(collapse = "\n")

for (id in assingments$id[-1]) {
  print(paste("Start eval", id))
  eval_text <- dbGetEvals(id, conn)
  result <- llm_comp_extract(eval_text$evaluation, prompt_comp_extract)
  test <- dbCompExtraction(conn, 1, result$data, return_tables = F)
}

# --- BATCH TEST

evaluation_texts <- dbGetEvals(1:3, conn)$evaluation
prompt <- readLines("inst/prompt_comp_extract.md") |>
  paste(collapse = "\n")

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

file_id = "file-3bd21c53a4f2455f982529c371007be8"

test <- llm_batch_create(file_id)

llm_batch_wait(batch_id)

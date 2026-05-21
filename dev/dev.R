# pak::pak("pieterjanvc/sqlife", ref = "expandConnections")

# Get latest pinned version from online (you need ot manually refresh)
dbInfo <- "local/cfme.db"
dbInfo <- "local/demo.db"
# pin_dev_get("cfme_db_export", dbInfo) # Uncomment when new data is needed
usernames <- c("Demo", "TK", "AW")
seed <- 54321

# Prev seeds
# seed <- 12345

# dbInfo <- "local/dev.db"

# Start from scratch ----
# ***********************
if (file.exists(dbInfo)) {
  file.remove(dbInfo)
}

# Setup DB and add evaluation data
dbSetup(dbInfo, "inst/cfme.sql", validateSchema = T)
conn <- dbGetConn(dbInfo)
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)
. <- dbAddEvaluations(combined_data, dbInfo, redactedOnly = T)
# Add human reviewers
. <- dbReviewerHuman(conn, username = usernames)
# Add default AI reviewer
. <- dbReviewerAI(conn, model = formals(llm_responses)$model)
# Add prompt
prompt <- readLines("inst/rubricPrompt.md") |> paste(collapse = "\n")
prompt_id <- dbAddPrompt(prompt, conn)
# Assign the same n random evals to each reviewer
set.seed(seed)
evalSample <- tbl(conn, "evaluation") |>
  select(summary_flg, complete, id) |>
  collect() |> # collect because seeding not possible in SQLite
  group_by(summary_flg, complete) |>
  slice_sample(n = 2) |>
  pull(id)
. <- dbReviewAssignment(
  conn,
  reviewer_id = rep(1:((length(usernames) + 1)), each = 6),
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)

# Run eval through LLM and insert into DB
# ***************************************
review_assignment_ids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 4) |>
  pull(id)

# Only do two reviews for now
llmReview <- llm_review(
  dbInfo,
  review_assignment_id = review_assignment_ids,
  log = "local/apiLog.csv",
  maxTries = 3
)

saveRDS(llmReview, "local/llmReviewBackup.rds")
# llmReview <- readRDS("local/llmReviewBackup.rds")

dbAIreview(conn, llmReview)

# Open the DB
system(paste("xdg-open", normalizePath(dbInfo)), wait = F)
system(paste('start ""', normalizePath(dbInfo)), wait = F)

pin_dev_set("cfme_db_import", dbInfo)

# Generate manual review doc
# **************************

man <- bind_rows(
  readxl::read_xlsx("local/manualReviewScores.xlsx", 1) |>
    mutate(reviewer_id = 3),
  readxl::read_xlsx("local/manualReviewScores.xlsx", 2) |>
    mutate(reviewer_id = 2)
) |>
  mutate(evaluation_id = str_extract(eID, "\\d+") |> as.integer()) |>
  tidyr::fill(evaluation_id) |>
  filter(!is.na(cID), !is.na(specificity), !is.na(utility), !is.na(sent)) |>
  filter(specificity != 0) |>
  select(-eID)

# temp
. <- dbReviewAssignment(
  conn,
  reviewer_id = 4,
  evaluation_id = 1209,
  redacted = T,
  include_questions = T
)

llmReview <- llm_review(
  dbInfo,
  review_assignment_id = 76,
  log = "local/apiLog.csv",
  maxTries = 3
)

saveRDS(llmReview, "local/llmReviewBackup.rds")

dbAIreview(conn, llmReview)


test <- llm_responses(
  input = "Blue bird",
  instructions = "Limerick style",
  log = "local/apiLog.csv",
  model = "gpt-5-mini"
)

test$output[[2]]$content[[1]]$text

req <- request("https://azure-ai.hms.edu/openai/v1/responses") |>
  req_headers(
    "Content-Type" = "application/json",
    "api-key" = Sys.getenv("HMS_AZURE_API")
  ) |>
  req_body_json(list(
    model = "gpt-5-mini",
    input = "Describe Earth to an alien",
    instructions = "In one or two sentences"
  )) |>
  req_error(is_error = ~FALSE) |>
  req_perform()

test <- resp_body_json(req)

pin_dev_set("cfme_db_import", "local/demo.db")

pin_dev_get("cfme_db_export", "local/test.db")
system(paste("xdg-open", normalizePath("local/test.db")), wait = F)

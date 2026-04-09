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
. <- dbReviewerAI(conn, model = "gpt-5.1")
# Add default prompt
prompt <- readLines("inst/prompt_comp_extract.md") |> paste(collapse = "\n")
prompt_id <- dbAddPrompt(prompt, conn, task = "comp_extract")

# Assign the same n random to the AI
set.seed(seed)
evalSample <-
  tbl(conn, "evaluation") |>
  select(id, summary_flg, complete) |>
  group_by(summary_flg, complete) |>
  collect() |>
  slice_sample(n = 3) |>
  pull(id)

assingments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T
)

review_ids <- tbl(conn, "review_assignment") |> pull(id)

batch <- llm_comp_extract_batch_submit(conn, review_ids, 1)

llm_batch_status(batch_id = batch$id, conn)

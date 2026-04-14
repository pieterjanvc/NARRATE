# ARGUMENTS
# *********
seed <- 20260414
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
prompt_extract_id <- dbAddPrompt(prompt, conn, task = "comp_extract")
prompt <- readLines("inst/prompt_comp_score.md") |> paste(collapse = "\n")
prompt_score_id <- dbAddPrompt(prompt, conn, task = "comp_score")

# Assign the same n random to the AI
set.seed(seed)
evalSample <-
  tbl(conn, "evaluation") |>
  select(id, summary_flg, complete) |>
  group_by(summary_flg, complete) |>
  collect() |>
  slice_sample(n = 1) |>
  pull(id)

assingments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = evalSample,
  redacted = T,
  include_questions = T,
  prompt_extract_id = prompt_extract_id,
  prompt_score_id = prompt_score_id
)

review_ids <- tbl(conn, "review_assignment") |>
  filter(statusCode == 0) |>
  pull(id)

batch1 <- llm_comp_extract_batch_submit(conn, review_ids)

llm_batch_status(batch_id = batch1$id, conn)

test <- batch_extract_process(3)

batch2 <- llm_comp_score_batch_submit(conn, review_ids)

llm_batch_status(batch_id = batch2$id, conn)

tbl(conn, "batch") |> select(file_output_id)

batch_id = 3

test <- batch_results_preprocess(
  file_output_id = "file-8cf2d1d8a85049688ee1d7b6a41af4ed"
)

# --------
review_ids <- tbl(conn, "review_assignment") |>
  filter(statusCode == 0) |>
  pull(id)

batch1_submit <- llm_comp_extract_batch_submit(conn, review_ids)
llm_batch_status(batch1_submit$id, conn)
batch1_result <- batch_extract_process(batch1_submit$id, conn)
batch2_submit <- llm_comp_score_batch_submit(conn, review_ids)
llm_batch_status(batch2_submit$id, conn)
batch2_result <- batch_score_process(batch2_submit$id, conn)

tbl(conn, "review_assignment") |> filter(statusCode == 4)

# --- test single synchonous

assignments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = 15,
  redacted = T,
  include_questions = T
)

test <- llm_comp_extract_run(
  conn,
  review_ids = 4,
  model = "gpt-5.1",
  force = T
)
test <- llm_comp_score_run(conn, review_ids = 4, model = "gpt-5.1")

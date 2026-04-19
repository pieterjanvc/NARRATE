# ARGUMENTS
# *********
seed <- 20260414
db_path <- "local/batch_test.db"
dbSetup(db_path, "inst/cfme.sql")
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn(db_path)

# SETUP
# *****

# Add all data
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
  slice_sample(n = 3) |>
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

# -------- BATCH - 2148 total

assingments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = 1001:2148,
  redacted = T,
  include_questions = T
)

review_ids <- tbl(conn, "review_assignment") |>
  filter(statusCode == 0) |>
  pull(id)

tbl(conn, "batch") |> filter(statusCode < 4)

batch_submit <- llm_comp_extract_batch_submit(conn, review_ids)
llm_batch_status(batch_submit$id, conn)
# bg_check <- batch_status_notify(batch_id = batch_submit$id, db_path = db_path)
batch1_result <- batch_extract_process(batch_submit$id, conn)
batch_submit <- llm_comp_score_batch_submit(conn, review_ids)
llm_batch_status(batch_submit$id, conn)
batch2_result <- batch_score_process(batch_submit$id, conn)

tbl(conn, "review_assignment") |> filter(statusCode == 5) |> summarise(n = n())

# --- test single synchonous

assignments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = 201:500,
  redacted = T,
  include_questions = T
)

tbl(conn, "review_assignment") |> filter(statusCode < 3)

review_ids <- 10
test <- llm_comp_extract_run(
  conn,
  review_ids = review_ids,
  model = "gpt-5.1",
  force = T
)
test <- llm_comp_score_run(
  conn,
  review_ids = review_ids,
  model = "gpt-5.1",
  force = T
)

dbGetEvals(28, conn)$evaluation |> cat()

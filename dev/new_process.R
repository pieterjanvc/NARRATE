# ARGUMENTS
# *********
seed <- 20260504
db_path <- "local/narrate-ai.db"
# file.remove(db_path)
dbSetup(db_path, "inst/narrate.sql")
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
. <- dbReviewerHuman(conn, username = c("TK", "AW", "KM", "PJ"))
# Process the rubric and generate prompts
rubric_process(conn)

dbAddCoreFaculty("local/core_faculty.csv", conn)

core_faculty <- read.csv("local/core_faculty.csv")

# Get all reviews written by a core-faculty member, before and after start

evalSample <- tbl(conn, "evaluator") |>
  filter(!is.na(core_faculty_start)) |>
  left_join(
    tbl(conn, "evaluation"),
    by = c("id" = "evaluator_id")
  ) |>
  pull(id.y)

# set.seed(seed)
# evalSample <-
#   tbl(conn, "evaluation") |>
#   select(id, summary_flg, complete) |>
#   group_by(summary_flg, complete) |>
#   collect() |>
#   slice_sample(n = 3) |>
#   pull(id)

# #Assign
# evalSample <- c(1115, 336, 1577, 937)

for (i in 1:5) {
  assingments <- dbReviewAssignment(
    conn,
    reviewer_id = i,
    evaluation_id = evalSample,
    rubric_id = 1,
    redacted = T,
    include_questions = T
  )
}


# review_ids <- 1:3
# review_ids <- tbl(conn, "review_assignment") |>
#   filter(statusCode == 0) |>
#   pull(id)

# -------- BATCH

review_ids <- tbl(conn, "review_assignment") |>
  filter(statusCode == 0, reviewer_id == 1) |>
  pull(id)

tbl(conn, "batch") |> filter(statusCode < 4)

# batch_submit <- list(id = 1)
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

review_ids <- 4

test <- llm_comp_extract_run(
  conn,
  review_ids = review_ids,
  model = "gpt-5.1"
)
test <- llm_comp_score_run(
  conn,
  review_ids = review_ids,
  model = "gpt-5.1"
)


# ------------ human review ----

dbReviewerHuman(conn, username = "test_user_1")

assingments <- dbReviewAssignment(
  conn,
  reviewer_id = 2,
  evaluation_id = 1:3,
  redacted = T,
  include_questions = T
)

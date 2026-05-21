# ARGUMENTS
# *********
seed <- 20260121
db_path <- "local/cfme.db"

dbSetup(db_path, "inst/cfme.sql")
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))

# SETUP
# *****

conn <- dbGetConn(db_path)

# Add all evaluation data
combined_data <- readxl::read_xlsx(
  "local/BIDMC_Med_Neuro_SPE_Comments_Dataset_07242025.xlsx"
)
. <- dbAddEvaluations(combined_data, db_path, redactedOnly = TRUE)

# Add default AI reviewer
. <- dbReviewerAI(conn, model = formals(llm_comp_extract)$model)

# Parse rubric.md, sync competency/score tables, create rubric row and prompts.
# Re-run whenever inst/rubric.md or the prompt templates change.
rubric <- rubric_process(conn)

# Assign a random sample of evaluations to reviewer 1
# rubric_id defaults to the most recently created rubric (rubric$id above)
set.seed(seed)
eval_sample <- tbl(conn, "evaluation") |>
  group_by(summary_flg, complete) |>
  slice_sample(n = 3) |>
  pull(id)

assignments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = eval_sample,
  rubric_id = 1,
  redacted = TRUE,
  include_questions = TRUE
)

# STEP 1 — Competency extraction
# ********************************
review_ids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 1, statusCode == 0) |>
  pull(id)

# Real-time (synchronous):
# llm_comp_extract_run(conn, review_ids)

# Batch (preferred for large sets):
batch_extract <- llm_comp_extract_batch_submit(conn, review_ids)
llm_batch_status(batch_extract$id, conn) # poll until statusCode == 3
batch_status_notify(batch_extract$id, db_path)
batch_extract_process(batch_extract$id, conn)

# STEP 2 — Competency scoring
# *****************************
review_ids_ready <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 1, statusCode == 3) |>
  pull(id)

# Real-time:
# llm_comp_score_run(conn, review_ids_ready)

# Batch:
batch_score <- llm_comp_score_batch_submit(conn, review_ids_ready)
llm_batch_status(batch_score$id, conn) # poll until statusCode == 3
# batch_status_notify(batch_score$id, db_path)
batch_score_process(batch_score$id, conn)

# SUMMARY STATS
# *************
specificityScaling <- 0.3
utilScaling <- 1.5
sentScaling <- 0.3

tbl(conn, "review_assignment") |>
  filter(statusCode == 5) |>
  left_join(
    tbl(conn, "competency_score") |>
      group_by(id = review_assignment_id) |>
      summarise(
        score = sum(specificity * specificityScaling),
        nComp = n(),
        minSpecificity = min(specificity),
        maxSpecificity = max(specificity),
        meanSpecificity = mean(specificity)
      ),
    by = "id"
  ) |>
  left_join(
    tbl(conn, "answer") |>
      group_by(id = evaluation_id) |>
      summarise(nchar = sum(nchar(answer_txt_redacted))),
    by = "evaluation_id"
  ) |>
  collect() |>
  mutate(
    score = score +
      utility_score_value * utilScaling +
      sentiment_score_value * sentScaling
  ) |>
  arrange(desc(score))

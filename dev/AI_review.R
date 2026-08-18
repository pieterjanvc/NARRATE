# ARGUMENTS
# *********
seed <- 20260121
db_path <- "local/narrate.db"

dbSetup(db_path, "inst/narrate.sql")
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

# The initial rubric (competencies, disambiguation, scores, rules) is seeded
# directly by inst/narrate.sql. Generate and link its prompts here — re-run
# whenever the prompt templates change.
rubric_id <- tbl(conn, "rubric") |>
  summarise(id = max(id, na.rm = TRUE)) |>
  pull(id)
rubric <- rubric_link_prompts(conn, rubric_id)

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
  filter(reviewer_id == 1, statusCode == 0, rubric_id == 3) |>
  pull(id)

# Real-time (synchronous):
# test <- llm_comp_extract_run(conn, review_ids, verbose = T, force = F)

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
# test <- llm_comp_score_run(conn, review_ids_ready, verbose = T, force = F)

# Batch:
batch_score <- llm_comp_score_batch_submit(conn, review_ids_ready)
llm_batch_status(batch_score$id, conn) # poll until statusCode == 3
# batch_status_notify(batch_score$id, db_path)
batch_score_process(batch_score$id, conn)

# SUMMARY STATS
# *************
# Uses review_scores() (R/analysis.R) so this matches the ANALYSIS tab's
# quality score instead of computing a separate formula.
review_ids_complete <- tbl(conn, "review_assignment") |>
  filter(statusCode == 5) |>
  pull(id)

eval_length <- tbl(conn, "review_assignment") |>
  filter(id %in% local(review_ids_complete)) |>
  select(review_id = id, evaluation_id) |>
  left_join(
    tbl(conn, "answer") |>
      group_by(evaluation_id) |>
      summarise(nchar = sum(nchar(answer_txt_redacted))),
    by = "evaluation_id"
  ) |>
  collect()

review_scores(conn, review_ids_complete) |>
  mutate(score = coverage + avg_specificity + utility) |>
  left_join(eval_length, by = "review_id") |>
  arrange(desc(score))

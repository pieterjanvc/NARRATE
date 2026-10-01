# Test dbCompExtractionCheckConflicts() end-to-end via the LIVE (synchronous)
# AI extraction pipeline, using two real evaluations that are known to have
# produced a rule-2 ("one competency per quote") violation in local/narrate.db
# (review_assignment 54 -> evaluation 1652, review_assignment 55 -> evaluation
# 2061, both flagged by scanning local/narrate.db with the new checker).
#
# Builds a fresh local/test.db from the current inst/narrate.sql (so it gets
# the new "Extraction conflict pending" status code and the new toggleable
# rule 6), copies in just those two evaluations (with their full student /
# evaluator / clerkship / rotation / question / answer FK chain) from
# local/narrate.db, assigns them to a fresh AI reviewer under the default
# rubric, and runs llm_comp_extract_run() (real-time, not batch) against them.
#
# Follows the same setup pattern as dev/AI_review.R.

# ARGUMENTS
# *********
src_db_path <- "local/narrate.db"
db_path <- "local/test.db"
eval_ids <- c(1652, 2061) # known to have produced a rule-2 conflict previously

dbSetup(db_path, "inst/narrate.sql")
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))

# SETUP
# *****

conn <- dbGetConn(db_path)

# --- Copy just the two target evaluations (+ their FK chain) from
# local/narrate.db into the fresh test db, preserving IDs. Both DBs share the
# same schema and neither has these operational tables (student, evaluator,
# clerkship, rotation, evaluation, question, answer) pre-seeded, so id
# collisions aren't a concern in a fresh db.
DBI::dbExecute(conn, sprintf("ATTACH DATABASE '%s' AS src", src_db_path))

id_list <- function(ids) paste(ids, collapse = ",")
query_src <- function(table, col, ids) {
  DBI::dbGetQuery(conn, sprintf(
    "SELECT * FROM src.%s WHERE %s IN (%s)", table, col, id_list(ids)
  ))
}
copy_rows <- function(table, ids) {
  if (length(ids) == 0) return(invisible(NULL))
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO %s SELECT * FROM src.%s WHERE id IN (%s)",
    table, table, id_list(ids)
  ))
}

eval_info <- query_src("evaluation", "id", eval_ids)
stopifnot(nrow(eval_info) == length(eval_ids))

rotation_info <- query_src("rotation", "id", eval_info$rotation_id)

copy_rows("evaluator", unique(eval_info$evaluator_id))
copy_rows("student", unique(rotation_info$student_id))
copy_rows("clerkship", unique(rotation_info$clerkship_id))
copy_rows("rotation", rotation_info$id)
copy_rows("evaluation", eval_info$id)

answer_info <- query_src("answer", "evaluation_id", eval_ids)
copy_rows("question", unique(answer_info$question_id))
copy_rows("answer", answer_info$id)

DBI::dbExecute(conn, "DETACH DATABASE src")

# --- Add default AI reviewer + link prompts for the seeded rubric (id = 1,
# includes the new rule 6 - see inst/narrate.sql)
. <- dbReviewerAI(conn, model = formals(llm_comp_extract)$model)
rubric <- rubric_link_prompts(conn, rubric_id = 1)

# --- Assign both evaluations to the AI reviewer
assignments <- dbReviewAssignment(
  conn,
  reviewer_id = 1,
  evaluation_id = eval_ids,
  rubric_id = 1,
  redacted = TRUE, # source data is redacted-only (see local/narrate.db)
  include_questions = TRUE
)

# STEP 1 — Competency extraction (real-time / live, not batch)
# ***************************************************************
review_ids <- tbl(conn, "review_assignment") |>
  filter(reviewer_id == 1, statusCode == 0, rubric_id == 1) |>
  pull(id)

result <- llm_comp_extract_run(conn, review_ids, verbose = TRUE, force = FALSE)
print(result)

# CHECK — did the live pipeline flag either review as a rule-2 conflict?
# ************************************************************************
# statusCode 6 = "Extraction conflict pending" (new); 3 = clean/complete
final_status <- tbl(conn, "review_assignment") |>
  filter(id %in% local(review_ids)) |>
  select(id, evaluation_id, statusCode) |>
  collect()
print(final_status)

for (rid in review_ids) {
  cat("\n--- review_assignment_id", rid, "---\n")
  print(dbCompExtractionCheckConflicts(conn, rid)$conflicts)
}

dbFinish(conn)

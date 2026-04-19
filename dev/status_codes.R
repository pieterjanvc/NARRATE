status_codes <- data.frame(
  table = c(
    rep("review_assignment", 7),
    rep("batch", 7)
  ),
  status_code = c(
    # review_assignment
    -1,
    0,
    1,
    2,
    3,
    4,
    5,
    # batch
    -3,
    -2,
    -1,
    1,
    2,
    3,
    4
  ),
  status_text = c(
    # review_assignment — from review_app.R:245-248, dbOperations.R:410, batch.R:359
    "Completed with flag",
    "New",
    "Extraction submitted",      # set by llm_comp_extract_batch_submit on submission
    "Completed",                 # set by legacy single-call path
    "Batch extraction complete", # set by batch_extract_process on success
    "Scoring submitted",         # set by llm_comp_score_batch_submit on submission
    "Batch scoring complete",    # set by batch_score_process on success
    # batch — from batch.R:252-261 (maps OpenAI API statuses) + insert at statusCode=1
    "Cancelled",
    "Expired",
    "Failed",
    "Submitted",   # set on batch creation before first status check
    "In progress", # validating / in_progress / finalizing
    "Completed",   # set by llm_batch_status when API reports completed
    "Processed"    # set by batch_extract_process / batch_score_process after writing results
  )
)

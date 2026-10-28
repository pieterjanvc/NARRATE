# Run (or advance) the AI batch review pipeline for all formative evaluations
# with gpt-6-luna under rubric 5. See ?llm_review_pipeline for the steps.
#
# Usage:  Rscript dev/ai_dev/run_rubric5_batch.R
#
# Check-and-exit: every run does whatever is possible right now and quits, so
# just rerun it (e.g. after a PushOver notification) to check progress and
# advance the pipeline. Nothing is ever started over.

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))

Sys.setenv(HUIT_API_NARRATE = keyring::key_get("HUIT_API_NARRATE"))

llm_review_pipeline(
  db_path = "local/narrate.db",
  rubric_id = 5L,
  model = "gpt-6-luna",
  chunk_size = 700L,
  max_extract_attempts = 2L,
  live_fix_truncated = TRUE,
  live_fix_confirm = TRUE,
  live_max_output_tokens = 8000L
)

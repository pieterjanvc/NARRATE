# Rubric 4 AI batch review — plan

## Scope

- 60 AI review assignments (reviewer 1, rubric 4), review_assignment ids
  2681–2740, all at statusCode 0. Created by `copy_rubric3_to_4.R` for the
  evaluations in `rubric4_eval_ids.csv`.
- Rubric 4 uses extract prompt 6 and score prompt 7. Both include the revised
  competencies; the score prompt includes the 4-level utility scale.

## Pipeline (`dev/ai_dev/run_rubric4_batch.R`)

Same as `dev/run_rubric3_batch.R`, except that the assignments already exist
and are not created.

0. Pre-flight: check that there are exactly 60 rubric-4 AI assignments and all
   are at status 0, then back up the database to `local/backup/`.
1. Extraction: one batch of all 60 (`llm_comp_extract_batch_submit`), then
   `batch_extract_process`.
2. Retry extraction failures (status -2) once: real-time if there are 5 or
   fewer, otherwise as a batch.
3. Resolve rule-2 conflicts: loop over reviews at status 6 with
   `llm_comp_resolve_batch_submit` and `batch_resolve_process` until none are
   left.
4. Scoring: one batch of all reviews at status 3
   (`llm_comp_score_batch_submit`), then `batch_score_process`.
5. Summary: print the status breakdown and list any review not at status 5.

Each step sends a PushOver notification when its batch settles. Progress is
checkpointed to `dev/ai_dev/rubric4_batch_state.json`, so re-running the
script resumes where it stopped.

## Run

```bash
Rscript dev/ai_dev/run_rubric4_batch.R 2>&1 | tee dev/ai_dev/rubric4_batch.log
```

The script polls each batch every 60 s, waiting up to 6 h per batch. Run it in
the background or in tmux.

## After the run

- Check that all 60 reviews are at status 5, and look into any that are not.
- Optional: compare AI and human rubric-4 scores once the humans finish.

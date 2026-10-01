# Plan: batch variant of the rule-2 conflict-resolution step

## Context

The live/synchronous resolve step is implemented and tested (`llm_comp_resolve_run()`
in `R/review.R`), used to fix rule-2 ("one competency per quote") conflicts
flagged by `dbCompExtractionCheckConflicts()` after extraction. It:

1. Fetches reviews at `statusCode == 6` ("Extraction conflict pending")
2. Builds a resolve prompt (`prompt_generate_resolve()` + `build_resolve_conflicts()`
   in `R/prompt.R`)
3. Calls the LLM synchronously (`llm_comp_resolve()` in `R/review_helper.R`)
4. Applies the answer (`dbCompConflictResolve()` in `R/dbOperations.R`)
5. Retries up to `max_attempts` (default 2), then gives up with `statusCode = -4`
   ("Extraction conflict unresolved") for human review

This document scopes the **batch** equivalent, for when conflict volume is too
high to resolve one-by-one in real time - the same reason `llm_comp_extract_batch_submit()`
/ `batch_extract_process()` exist alongside `llm_comp_extract_run()`.

## What's already reusable as-is (no changes needed)

- `prompt_generate_resolve(conn, rubric_id)` - static per-rubric resolve prompt
- `build_resolve_conflicts(conflicts, comp_data)` - clusters pairwise conflicts
  and builds the CONFLICTS section text + option mapping
- `dbCompConflictResolve(conn, clusters, resolutions, commit)` - applies a
  parsed resolution list to the DB (delete losers, transfer position to
  winner if needed, clean up orphaned `competency_score` rows)
- `llm_build_resolve_body(conflicts_text, prompt)` - request body builder
- Generic Azure batch helpers in `R/llm.R`: `llm_batch_build_jsonl()`,
  `llm_batch_upload()`, `llm_batch_create()`, `llm_batch_results()` - these
  are content-agnostic (just JSONL request/response plumbing), identical to
  what extract/score batches already use
- `db_record_batch()` and `llm_batch_status()` in `R/review_helper.R` - batch
  tracking (`batch` + `batch_review` tables), also content-agnostic
- `batch_results_preprocess()` in `R/review_helper.R` - generic per-`custom_id`
  JSON response parser, reusable unchanged

## What needs to be built

### 1. `llm_comp_resolve_batch_submit(conn, review_ids, model, endpoint, api_key, verbose, force)`

New function in `R/review.R`, structurally identical to
`llm_comp_extract_batch_submit()` (`R/review.R`, ~line 186):

```r
llm_comp_resolve_batch_submit <- function(
  conn,
  review_ids,
  model = "gpt-5.1-batch",
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API"),
  verbose = FALSE,
  force = FALSE
) {
  # 1. Filter review_ids to statusCode == 6 (unless force) - mirrors the
  #    statusCode == 0 filter in db_fetch_review_extract(), but there is no
  #    existing db_fetch_review_resolve() helper yet, so this filtering logic
  #    needs to be written fresh (see "New DB fetch helper" below)
  # 2. For each review, fetch its current conflicts
  #    (dbCompExtractionCheckConflicts()) and rubric_id, build the resolve
  #    prompt (prompt_generate_resolve(), can cache per rubric_id within the
  #    loop to avoid rebuilding per review) and conflicts text
  #    (build_resolve_conflicts())
  # 3. Build one request per review via llm_build_resolve_body(conflicts_text, prompt)
  # 4. Upload + create batch (llm_batch_build_jsonl/upload/create) - identical
  #    pattern to llm_comp_extract_batch_submit()
  # 5. db_record_batch(conn, file_input_id, batch_id, review_ids, review_status = 6L)
  #    - reuse the SAME status (6) rather than inventing a new one; batch_review
  #    already indicates "this review has an active batch job", so a review at
  #    6 could mean either "not yet resubmitted" or "resolve batch in flight" -
  #    that distinction isn't needed for a first version (see "Open questions")
}
```

**New DB fetch helper needed**: `db_fetch_review_resolve(conn, review_ids, force = FALSE)`
in `R/review_helper.R`, mirroring `db_fetch_review_extract()` (~line 209) but:
- Filters to `statusCode == 6` instead of `== 0`
- For each review, also needs its **conflicts** (call
  `dbCompExtractionCheckConflicts()` per review_id - this can't be vectorized
  via a single join like the other fetch helpers since it does non-trivial
  per-review computation; a `lapply()` over review_ids is fine here since
  conflict-checking is cheap, no LLM call involved)
- Returns something like: one row per review with `review_id`, `rubric_id`,
  and a **list-column** of `conflicts` (data frame) per row, OR just return a
  named list keyed by review_id mapping to `list(rubric_id, conflicts)` -
  whichever shape is easiest to consume in the submit function's request-building loop

### 2. `batch_resolve_process(batch_id, conn)`

New function in `R/review.R`, structurally identical to `batch_extract_process()`
(`R/review.R`, ~line 285), except:
- Instead of calling `dbCompExtraction()` per result, call `dbCompConflictResolve()`
- **Important difference from extract/score processing**: `dbCompConflictResolve()`
  needs the `clusters` data frame (built by `build_resolve_conflicts()`), not
  just the raw parsed LLM response. This means either:
  - (a) re-derive `clusters` at process time by re-running
    `dbCompExtractionCheckConflicts()` + `build_resolve_conflicts()` for each
    review right before applying its result (safe as long as the conflicts
    haven't changed between submit and process - should hold, since nothing
    else writes to these reviews' `competency_text` rows in between), or
  - (b) persist the `clusters` mapping generated at submit time (e.g. as a
    JSON blob in `batch.note`, or a new lightweight table) and reuse it
    verbatim at process time, avoiding any risk of drift
  - **Recommendation: (a)** - simpler, no schema/storage changes, and the
    conflictId numbering only needs to be internally consistent between the
    prompt sent and the resolutions parsed back for a single submit+process
    round trip. Since `custom_id` in the batch response already ties each
    result back to its `review_id`, re-deriving `clusters` per review at
    process time using the *same* `build_resolve_conflicts()` call is
    deterministic (same conflicts in, same clusters out) as long as
    conflictId assignment order is stable - it is, since it's driven by
    `unique(cluster_roots)` in insertion order, which only depends on the
    conflicts data frame's row order, which itself only depends on
    `dbCompExtractionCheckConflicts()`'s deterministic pairwise scan order.
- Set final `statusCode` per review to 3 (clean) if
  `dbCompExtractionCheckConflicts()` reports no conflicts after applying, or
  keep at 6 for another resolve round if conflicts remain and attempts are
  under the cap, or -4 if attempts are exhausted (see "Retry counting" below)

### 3. Retry counting for the batch path

The live path (`llm_comp_resolve_run()`) loops internally within one function
call. The batch path is inherently multi-step (submit → poll → process, each
a separate manual call per the existing pipeline style documented at the top
of `R/review.R`), so the retry count can't live in a local loop variable -
it needs to persist in the DB between calls, same as the live path already
does: `review_assignment.note` (e.g. `"conflict_resolve_attempts:1"`).

`batch_resolve_process()` should:
1. Read the existing attempt count from `note` (default 0 if absent/unparseable)
2. Increment it after applying this round's resolution
3. If conflicts remain and the new count is still `< max_attempts`: leave
   `statusCode = 6` so the review is eligible for another
   `llm_comp_resolve_batch_submit()` round
4. If conflicts remain and the count has reached `max_attempts`: set
   `statusCode = -4`
5. If no conflicts remain: set `statusCode = 3`, and clear/leave the note
   (doesn't matter which, statusCode 3 makes it irrelevant going forward)

### 4. Pipeline documentation

Update the top-of-file pipeline comment in `R/review.R` (currently documents
only extract/score batch steps) to add the resolve step, e.g.:

```
#   batch <- llm_comp_extract_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)          # poll until statusCode == 3
#   batch_extract_process(batch$id, conn)
#   # repeat for any review left at statusCode 6 (Extraction conflict pending):
#   batch <- llm_comp_resolve_batch_submit(conn, review_ids)
#   llm_batch_status(batch$id, conn)
#   batch_resolve_process(batch$id, conn)
#   batch <- llm_comp_score_batch_submit(conn, review_ids)
#   ...
```

## Open questions to settle before/while implementing

1. **Does a review mid-resolve-batch need a distinct status from "not yet
   resubmitted"?** Recommendation from the original design pass: no - reusing
   `statusCode = 6` for both is fine, since `batch_review` already tells you
   whether a review is part of an active batch job. Revisit only if that
   turns out to cause confusion in practice (e.g. an operator accidentally
   resubmitting a review that's already mid-flight in another batch).
2. **Should `db_fetch_review_resolve()` be a real exported/shared helper, or
   just inline logic inside `llm_comp_resolve_batch_submit()`?** The live
   path's equivalent logic lives inline in `llm_comp_resolve_run()` rather
   than as a separate `db_fetch_*` function (unlike extract/score, which do
   have dedicated fetch helpers) - probably fine to keep it inline for batch
   too, for symmetry, unless the submit function gets unwieldy.
3. **Rate/cost of per-review `dbCompExtractionCheckConflicts()` calls in a
   large batch submit** - this is a DB-only read per review, cheap, but for a
   very large `review_ids` vector it's still N round trips rather than one
   set-based query. Not worth optimizing until batch volumes are large enough
   to matter.

## Testing approach (once implemented)

Follow the same pattern as `dev/test_rule2_conflict.R`: build/reuse a test DB
with reviews already sitting at statusCode 6 (e.g. by running the live
extract pipeline against known-problematic evaluations, same as that script
does), then:

```r
batch <- llm_comp_resolve_batch_submit(conn, review_ids)
llm_batch_status(batch$id, conn)   # poll until statusCode == 3
batch_resolve_process(batch$id, conn)
```

Verify: `review_assignment.statusCode` moves to 3 (or -4 after enough failed
rounds), `dbCompExtractionCheckConflicts()` reports no conflicts for resolved
reviews, and no orphaned `competency_score`/`competency_text` rows remain
(same checks already used for the live-path tests).

## Related

- GitHub issue #15 (github.com/pieterjanvc/NARRATE) - separate, deferred
  problem (the model duplicating unique/non-repeated quotes across
  competencies). Not affected by whether resolution runs live or batch -
  that issue is about reducing how often conflicts occur in the first place,
  this document is about resolving them at scale once they do.

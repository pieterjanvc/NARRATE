# Plan: fix paraphrased / unlocatable AI competency quotes

Status: **ready to start** — scoped 2026-09-08, all §6 decisions resolved.
Companion to the rule-2 conflict work committed in `b25be1e`.

---

## 1. Problem

`dbCompExtraction()` stores each AI-extracted quote in `competency_text` with a
`start`/`end` character offset produced by `mod_highlight_locate()`. When the
model's `text_match` string is **not a verbatim substring** of the evaluation
text, the locator can't place it and the row is written with
`start = NULL, end = NULL`.

Consequences:

- The review app (`inst/review_app.R`) filters `!is.na(start), !is.na(end)`
  when seeding highlights, so a human auditor sees **no highlight** for that
  piece of AI evidence — they can't check where it came from.
- A paraphrased quote violates extraction **rule 3 ("Verbatim only")** — it is
  a data-quality defect regardless of highlighting.
- Some paraphrases are near-duplicates of another competency's quote (a rule-2
  violation that the exact-substring check in
  `dbCompExtractionCheckConflicts()` cannot catch).

This is the residue left over from the rule-2 reprocess: that work only fixed
`NA` rows that were *overlap-losers* (a verbatim quote whose span was already
claimed). The rows addressed here are a different failure — the text genuinely
isn't in the evaluation.

---

## 2. What is already done (do not redo)

- `mod_highlight_locate()` (committed in `b25be1e`) now has a
  **whitespace-tolerant fallback**: if the exact `fixed = TRUE` search fails it
  retries with every whitespace run allowed to match `[[:space:]]*`. This
  recovers quotes lost only to whitespace differences (e.g. `<br>` between two
  words stripped to nothing). It runs **only at extraction time** — it does not
  retroactively fix rows already stored as `NA`.

---

## 3. Sizing (rerun before starting — numbers drift)

Query: for every `competency_text` row with `start IS NULL` under a
rubric-3 AI review at `statusCode 5`, rebuild the canonical locate text
(`mod_highlight_strip_tags(dbGetEvals(..., html = TRUE, subtitleTag = "b"))`)
and test whether `text_match` is findable (exact **or** whitespace-tolerant).

As of 2026-09-08 (post rule-2 reprocess):

| bucket | count | rows | reviews |
|---|---|---|---|
| total `NA`-position rows | 235 | — | 150 |
| findable now (locator quirk — pre-dated the ws fallback) | **71** | no LLM needed | — |
| genuinely not in text (paraphrase / fabrication) | **164** | needs LLM or drop | — |

There will also be `NA` rows outside rubric 3 / outside `statusCode 5`; decide
whether they are in scope.

A ready-made sizing script exists at
`/tmp/.../scratchpad/` from the scoping session — reproduce it as
`dev/size_paraphrase_na.R` if useful.

---

## 4. Root causes

### 4a. Locator quirk (the 71) — mechanical, no LLM

These `text_match` strings *are* verbatim (or whitespace-equivalent) but were
extracted before the ws-tolerant fallback existed, so they were stored `NA`.
Fixable by a one-off relocate pass.

### 4b. Text-source mismatch bug — makes 4a worse, still live

The model and the locator read **different renderings** of the same
evaluation:

| consumer | call | result |
|---|---|---|
| model input | `db_fetch_review_extract()` → `dbGetEvals(id, conn)` (`html = FALSE`) then `dedupe_repeated_paragraphs()` | keeps `---question` headers, `\n`, `\n\n` |
| locator | `dbCompExtraction()` → `mod_highlight_strip_tags(dbGetEvals(..., html = TRUE, subtitleTag = "b"))` | `<br>` → `""`, headers gone, blocks concatenated |

So a quote that spans a question boundary, or includes an internal newline, or
sits next to a header, is not a verbatim substring of the locator's text even
though the model copied it faithfully.

**Constraint:** `competency_text.start/end` must stay in the
`strip_tags(dbGetEvals(html = TRUE, subtitleTag = "b"))` coordinate system —
the review app renders highlights against exactly that
(`inst/review_app.R` `evalText()` + `mod_highlight_server`). Changing the
coordinate system means re-locating **every** existing AI `competency_text`
row (human rows carry offsets from the browser module and must not shift).

### 4c. Genuine paraphrase / fabrication (the 164) — needs judgement

The model rewrote, truncated, inserted `…`, or invented lead-in words
("there was one specific patient who…"). No mechanical fix; the true source
span has to be found (LLM) or the quote discarded.

---

## 5. Proposed approach (phased, each shippable alone)

### Phase 0 — relocate pass for the mechanically-recoverable rows — DONE 2026-09-08

`dbRelocateCompText(conn, review_assignment_ids, apply_moves, dry_run, commit)`
added to `R/dbOperations.R` (+ internal helper `db_locate_text()`, which
`dbCompExtraction()` now also uses). `dev/size_paraphrase_na.R` (dry-run
sizing) and `dev/relocate_comp_text.R` (backup + apply + validate) added.

Behaviour: per review, re-locate the **whole** `competency_text` set in `id`
order; write back only rows that were `NA` (`apply_moves = FALSE` default —
already-located rows never move, never reset to `NA`).

Result on `local/narrate.db` (backup
`local/backup/narrate_pre-paraphrase-phase0_20260908-211806.db`):

| | count |
|---|---|
| NA-position rows before | 236 |
| **filled by Phase 0** | **52** |
| already-located rows that would move (left untouched) | 9 |
| **still NA → Phase 2/3** | **184** (183 at rubric-3/statusCode 5 across 132 reviews; +1 in review 1650 @ statusCode -4, out of scope) |

Validation passed: 0 positions lost, every filled `[start,end)` slices its
`text_match` back out (ws-tolerant). No re-score needed (positions only).

(The 71/164 split in §3 was pre-ws-fallback; the rule-2 reprocess already ran
the ws-tolerant locator, so only 52 were left to recover mechanically.)

### Phase 1 — fix the text-source mismatch (4b) so it stops recurring

Pick one:

- **1a (lightest):** feed the model the exact locator text
  (`mod_highlight_strip_tags(dbGetEvals(html = TRUE, subtitleTag = "b"))`,
  deduped) instead of the `html = FALSE` rendering. Downside: questions run
  straight into answers ("…specific examples.[name_redact] was…"). Measure
  extraction quality impact on a sample first.
- **1b:** change `dbGetEvals(html = TRUE)` so tag-stripping leaves a readable
  separator (e.g. emit `\n` alongside `<br>`, header on its own line), then
  **re-locate all existing AI `competency_text` rows once** as a migration
  (coordinate-system change — verify the app still lines up, and that human
  rows are untouched).
- **1c:** keep the ws-tolerant locator as the only defence and accept the
  residual. (Cheapest, leaves ~some ongoing paraphrase-independent misses.)

Recommendation: try **1a**, fall back to **1c** if extraction quality drops.
Do **not** do 1b unless 1a is unacceptable — the migration is the risky part.

### Phase 2 — LLM re-anchor for genuine paraphrases — LIVE PATH CODE DONE 2026-09-08

Mirrors the resolve step. New code:

| file | added |
|---|---|
| `inst/prompt_comp_reanchor.md` | static instructions (verbatim span or `null`) |
| `R/review_helper.R` | `llm_build_reanchor_body()`, `llm_comp_reanchor()` (API call, statusCodes 0/1/2), `db_fetch_review_reanchor()` (per-review: eval text + `items` df of unplaced rows), `build_reanchor_items()` (formats the input body) |
| `R/dbOperations.R` | `dbCompReanchorApply(conn, ra_id, items, anchors, commit)` — validates each anchor is ws-tolerant-verbatim, re-locates the whole `competency_text` set with anchors swapped in (frozen located rows act as fixed claims, never moved), writes `text_match`+`start`+`end` on a clean placement, else sets `locate_status = 'unlocated'`. Cross-competency overlap guard flags rather than writes. |
| `R/review.R` | `llm_comp_reanchor_run()` (live) — one call per review; on ≥1 re-anchor → statusCode 3, or 6 if the change created a rule-2 conflict; else stays 5 with rows flagged |
| `dev/reanchor_live_test.R` | smoke test on N reviews against a working copy, prints each anchored span in context |

Contract: `{"anchors": [{"itemId": N, "anchor": "<span>" | null}]}`, one entry
per unplaced row (grouped by review, non-overlap preserved). Separate op from
extraction and resolve — no competency re-selection.

Tested with hand-written anchors on a DB copy: valid span → row rewritten,
slice matches; `null` → `locate_status='unlocated'`, `start` stays NA;
non-verbatim span → flagged.

**Live smoke test (17 reviews / 19 quotes, 2026-09-09):** 19/19 re-anchored,
0 false flags after two fixes:
- dropped the cross-competency overlap guard in `dbCompReanchorApply()` — a
  re-anchored span nesting another competency's quote is a real rule-2
  conflict; `llm_comp_reanchor_run()` re-checks and routes those to statusCode
  6 (resolve), same as extraction does.
- added `reanchor_locate()` — the model reliably copies a span's body but
  keeps the paraphrase's subject word (`She`/`she`, `[name_redact]`/`He`); on
  a miss it strips the leading token, re-locates the remainder, and walks the
  start back over the real subject token. `text_match` is stored as the actual
  evaluation slice, not the model's string.
- prompt tightened: shortest span, evaluation's exact wording, first fragment
  only on `...` stitches.
~1/3 of touched reviews route to statusCode 6 (genuine nested-quote rule-2s);
the rest to 3.

### Batch path — DONE 2026-09-09

| file | added |
|---|---|
| `inst/narrate.sql` | statusCode 8 "Reanchor batch submitted" (+ migrated onto `local/narrate.db`) |
| `R/review.R` | `llm_comp_reanchor_batch_submit()` (5 → 8), `batch_reanchor_process()` (8 → 3 / 6 / 5; re-derives the itemId→ct_id map from current unplaced rows, guards on count mismatch) — same shape as the resolve batch pair |
| `dev/reanchor_batch.R` | full pipeline driver mirroring `dev/reprocess_rule2_conflicts.R`: backup → reanchor batch → resolve loop (≤2 rounds) → score batch → summary. Resumable via STATE_FILE, PushOver per step, `REANCHOR_DB` env override for testing on a copy. |

**End-to-end batch test (6 reviews, scratch copy, 2026-09-09):** ~17 min,
3 batch round-trips (reanchor 21 -> resolve 22 -> score 23). Result:
19 quotes re-anchored, **0 flagged, 0 slice mismatches** (all 50 located rows
in these reviews verified: `substr(plainText, start+1, end) == text_match`).
4 reviews hit a rule-2 conflict from re-anchoring, all resolved in 1 round;
2 competencies dropped where the re-anchored span's evidence was genuinely
shared (same behaviour as the rule-2 reprocess). Utility/sentiment unchanged.
All 6 back to statusCode 5.

### FULL RUN — DONE 2026-09-09

`dev/reanchor_batch.R` on all 132 rubric-3 AI reviews. ~23 min, 3 batches
(reanchor 21 -> resolve 22 [63 reviews, 1 round] -> score 23 [126 reviews]).
Backup: `local/backup/narrate_pre-reanchor_20260909-103618.db`.

| outcome | count |
|---|---|
| quotes re-anchored (stored slice == `text_match` exactly) | **145 / 145** |
| flagged `locate_status = 'unlocated'` for human review | **8** (5 are the model having quoted a `---question header` — clean rejects; 3 paraphrase/fabrication) |
| reviews that hit a rule-2 conflict from re-anchoring | 63 → 62 resolved, **1 → statusCode -4** (review 1798, resolve attempts exhausted, needs human — same escape hatch as review 1650 in the rule-2 work) |
| competency_score rows dropped (shared-evidence resolution) | 17 (807 → 790) |
| new ws-tolerant slice diffs introduced | **0** (28 before, 28 after — all pre-existing) |
| mean utility / sentiment | 2.69 / 4.83 → 2.69 / 4.86 (negligible) |
| final rubric-3 AI status | 2146 @ 5, 2 @ -4 (1650 + 1798) |

**Post-run cleanup (2026-09-09):** a full DB scan for residual rule-2
conflicts found review **1819** at statusCode 5 with an undetected `overlap` —
Phase 0's `dbRelocateCompText()` had filled a NULL position that landed on
another competency's span, and Phase 0 ran no conflict check / re-score
(the plan assumed it "only adds highlights"). Fixed by hand: statusCode 6 →
`llm_comp_resolve_run()` → `llm_comp_score_run()` → 5, no competency lost,
scores unchanged (backup `narrate_pre-1819fix_20260909-111538.db`). It was
the only statusCode-5 review affected. **If `dbRelocateCompText()` is ever
run again at scale, follow it with a conflict scan + resolve/re-score of the
filled reviews.**

**Final rubric-3 AI state:** 2146 @ statusCode 5, 2 @ -4 (1650, 1798).
competency_text: 14501 located, 8 flagged `locate_status='unlocated'` (all in
sc-5 reviews, for human adjudication), 1 NULL+unflagged (in 1650, sc-4). The
only remaining rule-2 conflicts are in 1650 and 1798 — both already at -4 for
human review. No hidden issues.

**Not yet done:** Phase 3 app display of the 8 `locate_status = 'unlocated'`
rows in `inst/review_app.R`. `tokens_in/out` on `review_assignment` get
overwritten with the re-anchor call's counts (matches
`llm_comp_resolve_run()`).

### Phase 3 — handle the unanchorable remainder

For rows Phase 2 returns `null` for (true fabrications) or that stay
non-verbatim after a retry:

- Option D1: delete the `competency_text` row; delete the `competency_score`
  if it is then orphaned (matches `dbCompConflictResolve()` cleanup). Loses
  the competency.
- Option D2: keep the row, set a flag / note, and surface it in the app as
  "AI evidence, source not located" so a human can accept or reject.

Recommendation: **D2** — don't silently drop AI judgements; let a human
adjudicate. Needs a small schema addition (a nullable `note` or status on
`competency_text`, or a review-level flag) — decide during implementation.

### Phase 4 — re-score affected reviews

Any review whose `competency_text` changed in Phases 0/2/3 must be re-scored
(`statusCode 5 → 3 → score`), same as the rule-2 reprocess. Reuse
`dev/reprocess_rule2_conflicts.R` structure (it already does reset → batch →
notify → re-score, resumable, PushOver per step).

---

## 6. Decisions — RESOLVED 2026-09-08

1. **Scope:** rubric 3 only. Older rubrics are retired. (Confirmed by data:
   all 235 `NA` rows are already rubric 3 / `statusCode 5` / reviewer gpt-5.1;
   the only other `NA` row is 1 at `statusCode -4`, an error state — ignore it.)
2. **Phase 1:** **1c** — accept the residual, no structural change. Phase 1 is
   a no-op for this effort; the ws-tolerant locator is the only defence and
   Phases 2–3 mop up. Revisit 1a (with a sample measurement) only if extraction
   is re-run at volume later.
3. **Phase 3:** **D2** — flag for human, do not drop. Requires a schema change:
   add a nullable column to `competency_text` (e.g. `note` for parity with
   `competency_score.note`, or a dedicated `locate_status`). Update
   `inst/narrate.sql` **and** migrate `local/narrate.db` (backup first, per §9).
4. **Re-score:** only the reviews actually touched in Phases 2/3 (a
   `text_match` changed, or a row flagged/removed). Phase 0 only adds
   highlights — no re-score.
5. **Phase 2 model:** write the code first, live-test on 1–2 reviews
   (`gpt-5.1`), then submit the rest as a batch (`gpt-5.1-batch`) later.

### Environment / template notes (verified 2026-09-08)

- Canonical DB: `local/narrate.db`. API key `HMS_AZURE_API` via
  `keyring::key_get()` (present), endpoint `https://azure-ai.hms.edu`.
- `dev/reprocess_rule2_conflicts.R` is the Phase 4 driver template, but it runs
  `5 → 6 → resolve → 3 → score`; paraphrase re-score has no conflict step, so
  the driver must reset `5 → 3 → score` (drop the resolve stage).
- `inst/prompt_comp_resolve.md` is the shape template for the new
  `inst/prompt_comp_reanchor.md`.
- Schema has no `review` table — join
  `competency_text → competency_score → review_assignment` (rubric_id,
  statusCode, reviewer_id live on `review_assignment`).
- The scratchpad sizing script is gone; regenerate as `dev/size_paraphrase_na.R`
  (it doubles as the Phase 0 dry run).

---

## 7. Files in play

- `R/shiny_mod_highlight.R` — `mod_highlight_locate()` (already ws-tolerant),
  `mod_highlight_strip_tags()`
- `R/dbOperations.R` — `dbCompExtraction()` locate call (~L853), new
  `dbRelocateCompText()` / re-anchor apply function
- `R/dbGetEvals` (in `dbOperations.R`, L289) — Phase 1b would change `html`
  rendering here
- `R/review_helper.R` — `db_fetch_review_extract()` (L412), `db_record_batch`,
  new fetch helper for the re-anchor step
- `R/review.R` — new `llm_comp_reanchor_run()` / `_batch_submit()` /
  `batch_reanchor_process()` alongside the resolve equivalents
- `inst/prompt_comp_reanchor.md` — new prompt template
- `inst/review_app.R` — L434-448 `evalText()`, L463-487 `highlightInitVals()`
  (Phase 1b coordinate check; Phase 3 D2 display)
- `inst/narrate.sql` — only if Phase 3 D2 needs a column
- `dev/reprocess_rule2_conflicts.R` — template for the Phase 4 driver

---

## 8. Testing

- Phase 0: run on a 10-review sample, assert every filled row's `text_match`
  equals the substring at its new `[start, end)` (the check used at the end of
  the rule-2 reprocess), and no previously-good offset became `NA`.
- Phase 1a: extract a ~20-review sample both ways, diff competency sets and
  `NA`-row counts.
- Phase 2: dry-run the apply on a handful with hand-written anchors before any
  live call; then live on ~10, eyeball each re-anchored span in context.
- Phase 4: same before/after comparison script used for the rule-2 batches
  (competency counts, specificity/utility/sentiment deltas, position
  integrity, 0 residual conflicts).

---

## 9. Backups

Always `cp local/narrate.db local/backup/narrate_pre-paraphrase-<step>_<ts>.db`
before any write step, as was done for every rule-2 batch.

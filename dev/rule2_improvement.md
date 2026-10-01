# Reducing rule-2 ("one competency per quote") conflicts in AI extraction

Status: investigation only, no code changed yet. Standalone plan for a future
session.

## Background

The AI review pipeline (see `dev/run_rubric3_batch.R`) runs three steps per
review: **extract** competencies + verbatim evidence quotes → **resolve** rule-2
conflicts → **score**.

Rule 2 ("One competency per quote", `rule.id = 2`) says each piece of evidence
text must be assigned to exactly one competency. After extraction,
`dbCompExtractionCheckConflicts()` (`R/dbOperations.R`) flags any review where
the same text span was assigned to two competencies — either an overlapping
character range or an identical `text_match` string. Flagged reviews go to
`statusCode 6` and a separate LLM "resolve" batch
(`llm_comp_resolve_batch_submit()` / `batch_resolve_process()`) picks the single
best competency for each conflicting quote (or discards it).

### The problem

Across the rubric-3 AI runs, a large and growing share of reviews hit rule-2
conflicts and needed the resolve pass:

| run (evals)      | reviews | needed resolve | rate |
|------------------|--------:|---------------:|-----:|
| 1–250            |     247 |             77 |  31% |
| 251–750          |     500 |            189 |  38% |
| 751–1450         |     700 |            311 |  44% |

The resolve pass works (conflicts clear in one round, ~none exhaust to
`statusCode -4`), but it is an extra LLM batch (~200k input tokens, ~10 min for
the 700 run) doing work that better extraction guidance could prevent. Also note
`dbCompExtractionCheckConflicts()` is **exact-match only** (documented in its
roxygen) — paraphrased double-assignment is not detected, so the true
semantic-overlap rate is higher than the numbers above.

## What the conflicts actually are

They are **genuine semantic overlap between adjacent competencies**, not sloppy
verbatim duplication. From the 700-run resolve batch (batch id 11): 311 reviews,
620 conflicting quote-pairs, mean 2 per review (max 9). Median conflicting quote
length 135 chars, p90 229 — i.e. full compound sentences, not tight phrases.

Example: *"strong insight into their patients ... highlighted and reorganized
during oral presentations on rounds"* was tagged as **both** "Provide Effective
Oral and Written Professional Communication" and "Clinical Reasoning and Decision
Making". It genuinely evidences both, and was quoted as one long span.

71% of the 620 conflicts come from just 7 competency pairs:

| competency pair (rubric-3 order #)                          | conflicts | the blur |
|------------------------------------------------------------|----------:|----------|
| 7 Professionalism ↔ 8 Interprofessional / Team-Based Care  |       124 | reliability / accountability vs teamwork |
| 3 Oral & Written Communication ↔ 4 Clinical Reasoning      |        84 | rounds presentations involve both |
| 1 Medical Knowledge ↔ 4 Clinical Reasoning                 |        80 | knowing facts vs applying them |
| 1 Medical Knowledge ↔ 6 Scholarly Inquiry / EBM            |        51 | both about learning / evidence |
| 5 Interpersonal & Communication Skills ↔ 8 Interprof./Team |        41 | patients & families vs colleagues |
| 5 Interpersonal & Communication Skills ↔ 7 Professionalism |        36 | rapport vs respect / integrity |
| 2 History Taking & Physical Exam ↔ 4 Clinical Reasoning    |        27 | data gathering vs synthesis |

Rubric-3 competency order (for reference):

1. Medical Knowledge
2. Medical History Taking and Physical Examination
3. Provide Effective Oral and Written Professional Communication
4. Clinical Reasoning and Decision Making
5. Interpersonal and Communication Skills
6. Scholarly Inquiry and Evidence-Based Medicine Integration
7. Professionalism
8. Interprofessional and Team-Based Care

## How the numbers above were generated

Run from the repo root with the package loadable and `HMS_AZURE_API` in the
keyring. Uses the stored **input** file of a completed resolve batch (the
`# CONFLICTS` section lists each conflicting quote and its candidate
competencies). Change `batch_ids` to aggregate several runs.

```r
suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn("local/narrate.db")

batch_ids <- 11L   # resolve batch(es) to analyse; e.g. c(5L, 8L, 11L)

fetch_input <- function(file_input_id) {
  resp <- httr2::request(
    paste0("https://azure-ai.hms.edu/openai/v1/files/", file_input_id, "/content")
  ) |>
    httr2::req_headers("api-key" = Sys.getenv("HMS_AZURE_API")) |>
    httr2::req_perform()
  lines <- strsplit(httr2::resp_body_string(resp), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

pairs <- c(); nconf_per <- c(); qlen <- c()
for (bid in batch_ids) {
  fi <- tbl(conn, "batch") |> filter(id == local(bid)) |> pull(file_input_id)
  for (r in fetch_input(fi)) {
    txt <- r$body$input
    # one block per "N. Quote:" entry in the # CONFLICTS section
    confs <- strsplit(txt, "(?<=\\n)(?=\\d+\\. Quote:)", perl = TRUE)[[1]]
    confs <- confs[grepl("cIndex", confs)]
    nconf_per <- c(nconf_per, length(confs))
    for (cf in confs) {
      idx <- as.integer(sub("cIndex ", "",
        regmatches(cf, gregexpr("cIndex \\d+", cf))[[1]]))
      q <- sub('.*Quote: "(.*?)".*', "\\1", gsub("\n", " ", cf))
      qlen <- c(qlen, nchar(q))
      if (length(idx) >= 2) pairs <- c(pairs, paste(sort(idx[1:2]), collapse = "-"))
    }
  }
}

cat("reviews:", length(nconf_per), " conflicts:", length(pairs),
    " mean/review:", round(mean(nconf_per), 1), " max:", max(nconf_per), "\n")
cat("conflicting-quote length: median", median(qlen), " p90", quantile(qlen, .9), "\n")
print(sort(table(pairs), decreasing = TRUE))
dbFinish(conn)
```

Extraction output-token distribution (separate check, to confirm quotes are long
not just numerous) was pulled from the extract batches with
`batch_results_preprocess()` / `raw$response$body$usage$output_tokens`.

## Proposed solution

Three levers, in priority order. Levers 1–2 are prompt/rubric changes; lever 3
is the expected downstream effect.

### 1. Populate the disambiguation section (biggest lever, ~71% of conflicts)

`competency_diff` currently has **zero rows** (`SELECT count(*) FROM
competency_diff;` → 0). As a result `prompt_build_competencies()` (`R/prompt.R`)
renders `{disambiguation_section}` as an **empty string** in *both* the
extraction prompt (`inst/prompt_comp_extract.md`) and the resolve prompt
(`inst/prompt_comp_resolve.md`). The model has no tie-breaker rule for any of the
recurring pairs.

Add one short rule per high-frequency pair (start with the 7 in the table). Each
`competency_diff` row: `competency_id1`, `competency_id2` (both resolved from the
`competency` table — note these are `competency.id`, not the rubric order #),
`description` = the decision rule. Draft rules (wording to be reviewed by the
domain experts, TK/AW/KM):

- **3 vs 4** (Communication vs Clinical Reasoning): assign to Communication only
  when the comment is about clarity / structure / concision of the delivery;
  assign to Clinical Reasoning when it is about the content — differential,
  prioritisation, assessment, plan.
- **1 vs 4** (Medical Knowledge vs Clinical Reasoning): Medical Knowledge = recall
  / understanding of facts and principles in the abstract; Clinical Reasoning =
  applying knowledge to *this* patient's data to reach a decision.
- **1 vs 6** (Medical Knowledge vs Scholarly Inquiry/EBM): Scholarly Inquiry only
  when the comment references literature, evidence appraisal, self-directed
  study, or a scholarly product; otherwise Medical Knowledge.
- **7 vs 8** (Professionalism vs Interprofessional/Team): Professionalism =
  individual conduct (accountability, punctuality, integrity, preparedness);
  Interprofessional/Team = coordinating work *with* other team members and
  disciplines.
- **5 vs 8** (Interpersonal vs Interprofessional/Team): Interpersonal = rapport
  and communication with patients / families / caregivers; Interprofessional =
  colleagues and other professions.
- **5 vs 7** (Interpersonal vs Professionalism): Interpersonal = relationship /
  rapport quality; Professionalism = respect, integrity, ethical conduct.
- **2 vs 4** (History & Physical vs Clinical Reasoning): H&P = the gathering and
  accurate reporting of history / exam findings; Clinical Reasoning = what was
  synthesised or concluded from them.

After inserting rows, regenerate the rubric's prompts. Because this changes
scoring inputs, treat it as a **new rubric version**: `rubric_add(conn, prev_id
= 3, info = "add disambiguation rules for top conflict pairs")` then
`rubric_link_prompts(conn, <new id>)`, and run the pipeline against the new
`rubric_id`. Do not silently mutate rubric 3 under already-scored reviews.

### 2. Tighten the quoting instruction in `inst/prompt_comp_extract.md`

Rule 3 ("Verbatim only") permits multiple spans but never says *minimal* span.
Median conflicting quote is a 135-char compound sentence. Add to rule 3 (or as a
new rule):

> Quote the shortest span that evidences the competency. If a single sentence
> contains evidence for two different competencies, split it at the clause
> boundary and quote each part under its own competency — never quote the whole
> sentence under both.

This removes the shared span, so the exact-overlap check no longer fires, and the
stored evidence is cleaner.

### 3. Expected effect on the workflow

With 1 + 2 in the extraction prompt, most conflicts never occur, so the resolve
pass shrinks (keep it as a safety net). Re-run the analysis script above on the
first resolve batch of the next run to measure the new rate; target < 20%.

## Validation plan for the future session

1. Draft `competency_diff` rows (lever 1) + prompt edit (lever 2); get domain
   sign-off on the rule wording.
2. Create a new rubric version, regenerate prompts.
3. Re-run a ~250-eval window through `dev/run_rubric3_batch.R` against the new
   rubric.
4. Run the reproduction script on that run's resolve batch; compare conflict
   rate and per-pair counts to the table above.
5. Spot-check 10–20 resolved-vs-not reviews for whether the new rules pushed
   assignments the way the domain experts expect.

## Key files / functions

- `inst/prompt_comp_extract.md`, `inst/prompt_comp_resolve.md` — prompt templates
- `R/prompt.R` : `prompt_build_competencies()` (renders `{disambiguation_section}`
  from `competency_diff`), `prompt_generate()`, `prompt_generate_resolve()`
- `R/rubric.R` : `rubric_add()`, `rubric_link_prompts()`
- `R/dbOperations.R` : `dbCompExtractionCheckConflicts()` (exact-match only),
  `dbCompConflictResolve()`
- `R/review.R` : `llm_comp_resolve_batch_submit()`, `batch_resolve_process()`
- `dev/run_rubric3_batch.R` — the end-to-end pipeline runner
- DB tables: `rule` (id 2 = rule-2), `competency`, `competency_diff` (empty),
  `rubric_competency`, `competency_score`, `competency_text`

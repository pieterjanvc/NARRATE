# Move the rubric 4 review set to rubric 5 (new utility scale).
#
# Scope: the 60 evaluations assigned under rubric 4.
#   - Rubric 5 is completed first: rules 1-6 (as in rubric 4) are attached,
#     prompts are regenerated and linked, and prev_id / info are set.
#   - A new AI reviewer 'gpt-6-luna' is added.
#   - Human reviewers 2, 3, 4 ('test' reviewer 5 is excluded) and gpt-6-luna
#     get a rubric 5 assignment (status 0) for each evaluation.
#   - Completed (2) / flagged (-1) rubric 4 human reviews are copied over as
#     partial reviews: competency scores + evidence text, sentiment, note and
#     duration are copied, utility is NOT (the scale changed). Their status is
#     set to 1 (In progress).
#   - Unstarted rubric 4 human assignments (status 0) are deleted.
#
# A backup is written to local/backup/ and an id mapping to dev/ai_dev/.
#
# Usage:  Rscript dev/ai_dev/copy_rubric4_to_5.R
#         RUBRIC5_DB=path/to/copy.db Rscript dev/ai_dev/copy_rubric4_to_5.R

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

db_path <- Sys.getenv("RUBRIC5_DB", "local/narrate.db")
old_rubric <- 4L
new_rubric <- 5L
human_reviewers <- c(2L, 3L, 4L)
ai_model <- "gpt-6-luna"
ra_inprogress <- 1L
n_expected <- 60L

conn <- dbGetConn(db_path)

# ── Safety checks ────────────────────────────────────────────────────────────
latest_rubric_id <- tbl(conn, "rubric") |>
  summarise(id = max(id, na.rm = TRUE)) |>
  pull(id)
stopifnot(latest_rubric_id == new_rubric)

n_existing <- tbl(conn, "review_assignment") |>
  filter(rubric_id == new_rubric) |>
  count() |>
  pull(n)
if (n_existing > 0) {
  stop("Rubric ", new_rubric, " already has ", n_existing, " assignments")
}

n_rules <- tbl(conn, "rubric_rule") |>
  filter(rubric_id == new_rubric) |>
  count() |>
  pull(n)
if (n_rules > 0) {
  stop("Rubric ", new_rubric, " already has ", n_rules, " rules")
}

n_ai <- tbl(conn, "reviewer") |>
  filter(model == ai_model | username == ai_model) |>
  count() |>
  pull(n)
if (n_ai > 0) stop("Reviewer '", ai_model, "' already exists")

# ── Scope ────────────────────────────────────────────────────────────────────
src_all <- tbl(conn, "review_assignment") |>
  filter(rubric_id == old_rubric, reviewer_id %in% human_reviewers) |>
  collect()

eval_ids <- sort(unique(src_all$evaluation_id))
stopifnot(
  length(eval_ids) == n_expected,
  nrow(src_all) == length(human_reviewers) * n_expected,
  !anyDuplicated(src_all[, c("evaluation_id", "reviewer_id")]),
  # AI rubric 4 set covers the same evaluations
  setequal(
    eval_ids,
    tbl(conn, "review_assignment") |>
      filter(rubric_id == old_rubric, !reviewer_id %in% human_reviewers) |>
      pull(evaluation_id)
  )
)

src_done <- src_all |> filter(statusCode %in% c(2L, -1L))
src_open <- src_all |> filter(statusCode == 0L)
stopifnot(nrow(src_done) + nrow(src_open) == nrow(src_all))

# ── Lookup maps (rubric 4 id -> rubric 5 id) ─────────────────────────────────
comp_map <- tbl(conn, "rubric_competency") |>
  filter(rubric_id %in% c(old_rubric, new_rubric)) |>
  inner_join(
    tbl(conn, "competency") |> select(competency_id = id, cID),
    by = "competency_id"
  ) |>
  select(rubric_id, competency_id, cID) |>
  collect()
comp_map <- inner_join(
  comp_map |> filter(rubric_id == old_rubric) |> select(old_id = competency_id, cID),
  comp_map |> filter(rubric_id == new_rubric) |> select(new_id = competency_id, cID),
  by = "cID"
)

sent_map <- tbl(conn, "rubric_sentiment") |>
  filter(rubric_id %in% c(old_rubric, new_rubric)) |>
  select(rubric_id, score_id = sentiment_id) |>
  inner_join(tbl(conn, "sentiment") |> select(score_id = id, value), by = "score_id") |>
  collect()
sent_map <- inner_join(
  sent_map |> filter(rubric_id == old_rubric) |> select(old_id = score_id, value),
  sent_map |> filter(rubric_id == new_rubric) |> select(new_id = score_id, value),
  by = "value"
)

# Source competency scores / texts for the reviews to copy
src_cs <- tbl(conn, "competency_score") |>
  filter(review_assignment_id %in% local(src_done$id)) |>
  collect()
src_ct <- tbl(conn, "competency_text") |>
  filter(competency_score_id %in% local(src_cs$id)) |>
  collect()

stopifnot(
  all(src_cs$competency_id %in% comp_map$old_id),
  all(na.omit(src_done$sentiment_score_id) %in% sent_map$old_id),
  !anyDuplicated(src_cs[, c("review_assignment_id", "competency_id")])
)

# Open rubric 4 assignments to delete: must not hold any scores
stopifnot(
  tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(src_open$id)) |>
    count() |>
    pull(n) ==
    0
)

old_rules <- tbl(conn, "rubric_rule") |>
  filter(rubric_id == old_rubric) |>
  arrange(order) |>
  collect()
stopifnot(nrow(old_rules) > 0)

cat(sprintf(
  "Scope: %d evaluations | copy %d reviews (%d scores, %d texts) | delete %d open rubric %d assignments | attach %d rules\n",
  length(eval_ids), nrow(src_done), nrow(src_cs), nrow(src_ct),
  nrow(src_open), old_rubric, nrow(old_rules)
))

# ── Backup ───────────────────────────────────────────────────────────────────
backup_path <- file.path(
  "local/backup",
  sprintf(
    "%s_pre-rubric5-copy_%s.db",
    tools::file_path_sans_ext(basename(db_path)),
    format(Sys.time(), "%Y%m%d-%H%M%S")
  )
)
dir.create(dirname(backup_path), showWarnings = FALSE, recursive = TRUE)
stopifnot(file.copy(db_path, backup_path))
cat("Backup written to", backup_path, "\n")

# ── Transfer (single transaction) ────────────────────────────────────────────
# 1. Complete rubric 5: rules, prompts, provenance
old_rules |>
  transmute(rubric_id = new_rubric, rule_id, order) |>
  tbl_insert(conn, "rubric_rule", commit = FALSE, returnData = FALSE)

prompts <- prompt_generate(conn, rubric_id = new_rubric)
prompt_extract_id <- dbAddPrompt(
  prompts$extract, conn,
  task = "comp_extract", commit = FALSE, showWarning = TRUE
)
prompt_score_id <- dbAddPrompt(
  prompts$score, conn,
  task = "comp_score", commit = FALSE, showWarning = TRUE
)
tbl_update(
  data.frame(
    id = new_rubric,
    prompt_extract_id = prompt_extract_id,
    prompt_score_id = prompt_score_id,
    prev_id = old_rubric,
    info = "4-level utility scale revised (utility 2-4); rest as rubric 4"
  ),
  conn,
  "rubric",
  commit = FALSE,
  returnData = FALSE
)

# 2. New AI reviewer
ai_reviewer <- tbl_insert(
  data.frame(human = 0L, model = ai_model, username = ai_model),
  conn,
  "reviewer",
  commit = FALSE
) |>
  pull(id)

# 3. New rubric 5 assignments (status 0; created/modified default to now)
new_human <- src_all |>
  select(evaluation_id, reviewer_id, include_questions, redacted)
new_ai <- data.frame(
  evaluation_id = eval_ids,
  reviewer_id = ai_reviewer,
  include_questions = 1L,
  redacted = 1L
)
new_ra <- bind_rows(new_human, new_ai) |>
  mutate(rubric_id = new_rubric, statusCode = 0L) |>
  tbl_insert(conn, "review_assignment", commit = FALSE)

# 4. Copy competency scores / texts for completed / flagged reviews
ra_map <- src_done |>
  select(old_ra = id, evaluation_id, reviewer_id) |>
  inner_join(
    new_ra |> select(new_ra = id, evaluation_id, reviewer_id),
    by = c("evaluation_id", "reviewer_id")
  )
stopifnot(nrow(ra_map) == nrow(src_done))

new_cs <- src_cs |>
  inner_join(ra_map |> select(old_ra, new_ra), by = c("review_assignment_id" = "old_ra")) |>
  inner_join(comp_map |> select(old_id, new_id), by = c("competency_id" = "old_id")) |>
  transmute(
    old_cs = id,
    review_assignment_id = new_ra,
    competency_id = new_id,
    specificity,
    note
  )
stopifnot(nrow(new_cs) == nrow(src_cs))

ins_cs <- new_cs |>
  select(-old_cs) |>
  tbl_insert(conn, "competency_score", commit = FALSE)
cs_map <- new_cs |>
  select(old_cs, review_assignment_id, competency_id) |>
  inner_join(
    ins_cs |> select(new_cs = id, review_assignment_id, competency_id),
    by = c("review_assignment_id", "competency_id")
  )
stopifnot(nrow(cs_map) == nrow(src_cs))

if (nrow(src_ct) > 0) {
  src_ct |>
    inner_join(cs_map |> select(old_cs, new_cs), by = c("competency_score_id" = "old_cs")) |>
    transmute(
      competency_score_id = new_cs,
      text_match,
      start,
      end,
      locate_status
    ) |>
    tbl_insert(conn, "competency_text", commit = FALSE, returnData = FALSE)
}

# 5. Mark copied reviews as in progress (sentiment, note, duration; no utility)
src_done |>
  inner_join(ra_map |> select(old_ra, new_ra), by = c("id" = "old_ra")) |>
  left_join(sent_map |> select(old_id, new_sent = new_id), by = c("sentiment_score_id" = "old_id")) |>
  transmute(
    id = new_ra,
    statusCode = ra_inprogress,
    sentiment_score_id = new_sent,
    sentiment_score_value,
    duration,
    note,
    modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
  tbl_update(conn, "review_assignment", commit = FALSE, returnData = FALSE)

# 6. Delete open rubric 4 human assignments (no scores, checked above)
tbl_delete(src_open |> select(id), conn, "review_assignment", commit = FALSE, returnData = FALSE)

# 7. Verification
r5 <- tbl(conn, "review_assignment") |> filter(rubric_id == new_rubric) |> collect()
stopifnot(
  nrow(r5) == (length(human_reviewers) + 1) * n_expected,
  sum(r5$statusCode == ra_inprogress) == nrow(src_done),
  all(r5$statusCode %in% c(0L, ra_inprogress)),
  all(is.na(r5$utility_score_id)),
  all(is.na(r5$utility_score_value)),
  all(r5$sentiment_score_id[!is.na(r5$sentiment_score_id)] %in% sent_map$new_id),
  sum(!is.na(r5$sentiment_score_id)) == sum(!is.na(src_done$sentiment_score_id)),
  setequal(r5$reviewer_id, c(human_reviewers, ai_reviewer))
)
summ <- function(ra_ids) {
  cs <- tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(ra_ids)) |>
    left_join(
      tbl(conn, "competency_text") |>
        count(competency_score_id, name = "n_text"),
      by = c("id" = "competency_score_id")
    ) |>
    inner_join(tbl(conn, "competency") |> select(id, cID), by = c("competency_id" = "id")) |>
    collect()
  cs |>
    mutate(n_text = coalesce(n_text, 0L)) |>
    select(review_assignment_id, cID, specificity, n_text)
}
chk <- full_join(
  summ(ra_map$old_ra) |> inner_join(ra_map, by = c("review_assignment_id" = "old_ra")) |>
    select(new_ra, cID, spec_old = specificity, text_old = n_text),
  summ(ra_map$new_ra) |>
    rename(new_ra = review_assignment_id, spec_new = specificity, text_new = n_text),
  by = c("new_ra", "cID")
)
stopifnot(
  nrow(chk) == nrow(src_cs),
  identical(chk$spec_old, chk$spec_new),
  identical(chk$text_old, chk$text_new)
)
r5_rubric <- tbl(conn, "rubric") |> filter(id == new_rubric) |> collect()
stopifnot(
  tbl(conn, "rubric_rule") |> filter(rubric_id == new_rubric) |> count() |> pull(n) ==
    nrow(old_rules),
  r5_rubric$prev_id == old_rubric,
  tbl(conn, "review_assignment") |>
    filter(id %in% local(src_open$id)) |>
    count() |>
    pull(n) ==
    0
)

# Mapping file for provenance
write.csv(
  ra_map |> rename(rubric4_review_id = old_ra, rubric5_review_id = new_ra),
  "dev/ai_dev/rubric4_to_5_review_map.csv",
  row.names = FALSE
)
dbFinish(conn)

cat(sprintf(
  "Transfer committed | AI reviewer id %d | rubric %d prompts: extract %d, score %d\n",
  ai_reviewer, new_rubric, prompt_extract_id, prompt_score_id
))

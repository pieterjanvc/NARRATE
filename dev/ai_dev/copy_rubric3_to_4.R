# Move the 2026-10-02 human review batch from rubric 3 to rubric 4.
#
# Scope: the 60 non-summary evaluations assigned to human reviewers on
# 2026-10-02 under rubric 3 (the earlier 11 evaluations stay on rubric 3).
#   - Human reviewers 2, 3, 4 ('test' reviewer 5 is excluded) get a rubric 4
#     assignment for each evaluation. Completed (2) / flagged (-1) rubric 3
#     reviews are copied over (scores mapped to rubric 4 ids), others start at 0.
#   - AI (reviewer 1) gets a new rubric 4 assignment (status 0) per evaluation.
#   - Unfinished rubric 3 human assignments (status 0/1) on these evaluations
#     are deleted (all reviewers, incl. 'test').
#
# Copied reviews are inserted as status 0 (created/modified = now), scores are
# copied, and only then the status is set and modified is updated to now.
#
# A backup is written to local/backup/ and an id mapping to dev/ai_dev/.
#
# Usage:  Rscript dev/ai_dev/copy_rubric3_to_4.R
#         RUBRIC4_DB=path/to/copy.db Rscript dev/ai_dev/copy_rubric3_to_4.R

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

db_path <- Sys.getenv("RUBRIC4_DB", "local/narrate.db")
old_rubric <- 3L
new_rubric <- 4L
batch_date <- "2026-10-02"
ai_reviewer <- 1L
human_reviewers <- c(2L, 3L, 4L)
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

# ── Scope ────────────────────────────────────────────────────────────────────
src_all <- tbl(conn, "review_assignment") |>
  filter(rubric_id == old_rubric) |>
  inner_join(
    tbl(conn, "reviewer") |> filter(human == 1) |> select(reviewer_id = id),
    by = "reviewer_id"
  ) |>
  inner_join(
    tbl(conn, "evaluation") |>
      filter(summary_flg == 0) |>
      select(evaluation_id = id),
    by = "evaluation_id"
  ) |>
  collect()

eval_ids <- src_all |>
  group_by(evaluation_id) |>
  summarise(first_created = min(created)) |>
  filter(substr(first_created, 1, 10) == batch_date) |>
  pull(evaluation_id) |>
  sort()
stopifnot(length(eval_ids) == n_expected)

src_all <- src_all |> filter(evaluation_id %in% eval_ids)
stopifnot(!anyDuplicated(src_all[, c("evaluation_id", "reviewer_id")]))

src_done <- src_all |>
  filter(reviewer_id %in% human_reviewers, statusCode %in% c(2L, -1L))
src_open <- src_all |> filter(statusCode %in% c(0L, 1L))
stopifnot(nrow(src_done) + nrow(src_open) == nrow(src_all))

# All in-scope reviewer x evaluation combinations must exist under rubric 3
stopifnot(
  nrow(src_all |> filter(reviewer_id %in% human_reviewers)) ==
    length(human_reviewers) * n_expected
)

# ── Lookup maps (rubric 3 id -> rubric 4 id) ─────────────────────────────────
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

score_map <- function(link_tbl, score_tbl, id_col) {
  m <- tbl(conn, link_tbl) |>
    filter(rubric_id %in% c(old_rubric, new_rubric)) |>
    select(rubric_id, score_id = all_of(id_col)) |>
    inner_join(tbl(conn, score_tbl) |> select(score_id = id, value), by = "score_id") |>
    collect()
  inner_join(
    m |> filter(rubric_id == old_rubric) |> select(old_id = score_id, value),
    m |> filter(rubric_id == new_rubric) |> select(new_id = score_id, value),
    by = "value"
  )
}
util_map <- score_map("rubric_utility", "utility", "utility_id")
sent_map <- score_map("rubric_sentiment", "sentiment", "sentiment_id")

# Source competency scores / texts for the reviews to copy
src_cs <- tbl(conn, "competency_score") |>
  filter(review_assignment_id %in% local(src_done$id)) |>
  collect()
src_ct <- tbl(conn, "competency_text") |>
  filter(competency_score_id %in% local(src_cs$id)) |>
  collect()

stopifnot(
  all(src_cs$competency_id %in% comp_map$old_id),
  all(na.omit(src_done$utility_score_id) %in% util_map$old_id),
  all(na.omit(src_done$sentiment_score_id) %in% sent_map$old_id),
  !anyDuplicated(src_cs[, c("review_assignment_id", "competency_id")])
)

# Open rubric 3 assignments to delete; check for partial scores
del_cs <- tbl(conn, "competency_score") |>
  filter(review_assignment_id %in% local(src_open$id)) |>
  collect()
del_ct <- tbl(conn, "competency_text") |>
  filter(competency_score_id %in% local(del_cs$id)) |>
  collect()

cat(sprintf(
  "Scope: %d evaluations | copy %d reviews (%d scores, %d texts) | delete %d open rubric %d assignments (%d scores, %d texts)\n",
  length(eval_ids), nrow(src_done), nrow(src_cs), nrow(src_ct),
  nrow(src_open), old_rubric, nrow(del_cs), nrow(del_ct)
))

# ── Backup ───────────────────────────────────────────────────────────────────
backup_path <- file.path(
  "local/backup",
  sprintf(
    "%s_pre-rubric4-copy_%s.db",
    tools::file_path_sans_ext(basename(db_path)),
    format(Sys.time(), "%Y%m%d-%H%M%S")
  )
)
dir.create(dirname(backup_path), showWarnings = FALSE, recursive = TRUE)
stopifnot(file.copy(db_path, backup_path))
cat("Backup written to", backup_path, "\n")

# ── Transfer (single transaction) ────────────────────────────────────────────
# 1. New rubric 4 assignments (status 0; created/modified default to now)
new_human <- expand.grid(
  evaluation_id = eval_ids,
  reviewer_id = human_reviewers
) |>
  left_join(
    src_all |>
      select(evaluation_id, reviewer_id, include_questions, redacted),
    by = c("evaluation_id", "reviewer_id")
  )
new_ai <- data.frame(
  evaluation_id = eval_ids,
  reviewer_id = ai_reviewer,
  include_questions = 1L,
  redacted = 1L
)
new_ra <- bind_rows(new_human, new_ai) |>
  mutate(rubric_id = new_rubric, statusCode = 0L) |>
  tbl_insert(conn, "review_assignment", commit = FALSE)

# 2. Copy scores for completed / flagged reviews
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

# 3. Complete the copied reviews (status, overall scores, modified = now)
src_done |>
  inner_join(ra_map |> select(old_ra, new_ra), by = c("id" = "old_ra")) |>
  left_join(util_map |> select(old_id, new_util = new_id), by = c("utility_score_id" = "old_id")) |>
  left_join(sent_map |> select(old_id, new_sent = new_id), by = c("sentiment_score_id" = "old_id")) |>
  transmute(
    id = new_ra,
    statusCode,
    utility_score_id = new_util,
    utility_score_value,
    sentiment_score_id = new_sent,
    sentiment_score_value,
    duration,
    note,
    modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
  tbl_update(conn, "review_assignment", commit = FALSE, returnData = FALSE)

# 4. Delete open rubric 3 assignments (children first)
if (nrow(del_ct) > 0) {
  tbl_delete(del_ct |> select(id), conn, "competency_text", commit = FALSE, returnData = FALSE)
}
if (nrow(del_cs) > 0) {
  tbl_delete(del_cs |> select(id), conn, "competency_score", commit = FALSE, returnData = FALSE)
}
tbl_delete(src_open |> select(id), conn, "review_assignment", commit = FALSE, returnData = FALSE)

# 5. Verification
r4 <- tbl(conn, "review_assignment") |> filter(rubric_id == new_rubric) |> collect()
stopifnot(
  nrow(r4) == (length(human_reviewers) + 1) * n_expected,
  sum(r4$statusCode != 0) == nrow(src_done),
  all(r4$utility_score_id[!is.na(r4$utility_score_id)] %in% util_map$new_id),
  all(r4$sentiment_score_id[!is.na(r4$sentiment_score_id)] %in% sent_map$new_id)
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
stopifnot(
  tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(r4$id)) |>
    filter(!competency_id %in% local(comp_map$new_id)) |>
    count() |>
    pull(n) ==
    0,
  tbl(conn, "review_assignment") |>
    filter(id %in% local(src_open$id)) |>
    count() |>
    pull(n) ==
    0
)

# Mapping file for provenance
write.csv(
  ra_map |> rename(rubric3_review_id = old_ra, rubric4_review_id = new_ra),
  "dev/ai_dev/rubric3_to_4_review_map.csv",
  row.names = FALSE
)
write.csv(
  data.frame(evaluation_id = eval_ids),
  "dev/ai_dev/rubric4_eval_ids.csv",
  row.names = FALSE
)
dbFinish(conn)

cat("Transfer committed\n")

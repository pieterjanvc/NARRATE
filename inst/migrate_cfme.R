# Migration: old narrate.db schema → new schema with rubric / split score tables
# Run from the project root in RStudio (devtools::load_all() first is fine).
#
#
# Assumptions:
#   - All existing review assignments belong to a single rubric (rubric_id = 1).
#   - Prompt references on review_assignment are dropped; the rubric row is
#     created without prompt links per user instruction.
#   - Old `score` table value column is TEXT ("1", "2", …); new tables keep
#     that convention, so character matching is used for ID lookups.

library(RSQLite)
library(dplyr)

src_path <- "local/test.db"
dst_path <- "local/narrate_new.db"
schema_path <- "inst/narrate.sql"

if (!file.exists(src_path)) {
  stop("Source database not found: ", src_path)
}
if (!file.exists(schema_path)) {
  stop("Schema file not found: ", schema_path)
}

# Remove stale output if it exists
if (file.exists(dst_path)) {
  message("Removing existing ", dst_path)
  file.remove(dst_path)
}

# ── 1. Create new database from schema ───────────────────────────────────────

sql_statements <- paste(readLines(schema_path), collapse = "\n") |>
  strsplit(";\n") |>
  unlist() |>
  trimws()
sql_statements <- sql_statements[nchar(sql_statements) > 0]

src <- dbConnect(SQLite(), src_path)
dst <- dbConnect(SQLite(), dst_path)

# SQLite FK enforcement off during bulk load
dbExecute(dst, "PRAGMA foreign_keys = OFF")

for (stmt in sql_statements) {
  if (!grepl("^--", stmt)) {
    tryCatch(dbExecute(dst, stmt), error = function(e) {
      message(
        "Skipping statement (",
        conditionMessage(e),
        "):\n",
        substr(stmt, 1, 80)
      )
    })
  }
}

message("New schema created.")

# ── 2. Copy unchanged tables verbatim ────────────────────────────────────────

unchanged <- c(
  "student",
  "evaluator",
  "clerkship",
  "rotation",
  "evaluation",
  "question",
  "answer",
  "reviewer",
  "prompt",
  "batch",
  "batch_review",
  "competency",
  "competency_diff",
  "competency_score",
  "competency_text"
)

for (tbl in unchanged) {
  if (tbl %in% dbListTables(src)) {
    rows <- dbReadTable(src, tbl)
    if (nrow(rows) > 0) {
      dbWriteTable(dst, tbl, rows, append = TRUE)
      message(sprintf("  %-25s %d rows", tbl, nrow(rows)))
    } else {
      message(sprintf("  %-25s (empty)", tbl))
    }
  }
}

# ── 3. Populate split score tables from old `score` table ────────────────────

old_score <- dbReadTable(src, "score")

copy_scores <- function(category, table_name) {
  rows <- old_score |>
    filter(category == !!category) |>
    arrange(as.integer(value)) |>
    select(value, description, example, timestamp, note)
  dbWriteTable(dst, table_name, rows, append = TRUE)
  message(sprintf(
    "  %-25s %d rows  (from score category '%s')",
    table_name,
    nrow(rows),
    category
  ))
  invisible(rows)
}

copy_scores("Specificity", "specificity")
copy_scores("Utility", "utility")
copy_scores("Sentiment", "sentiment")

# ── 4. Create the single rubric row (no prompt links) ────────────────────────

dbExecute(
  dst,
  "
  INSERT INTO rubric (id, prompt_extract_id, prompt_score_id, info, timestamp)
  VALUES (1, NULL, NULL, 'Original rubric (migrated)', datetime('now','localtime'))
"
)
message("  rubric                    1 row  (id = 1)")

# ── 5. Populate rubric join tables ───────────────────────────────────────────

# rubric_competency — ordered by cID
competencies <- dbReadTable(dst, "competency") |>
  group_by(cID) |>
  filter(id == max(id)) |> # latest version of each competency
  ungroup() |>
  arrange(cID)

rubric_comp <- data.frame(
  rubric_id = 1L,
  competency_id = competencies$id,
  order = seq_len(nrow(competencies))
)
dbWriteTable(dst, "rubric_competency", rubric_comp, append = TRUE)
message(sprintf("  %-25s %d rows", "rubric_competency", nrow(rubric_comp)))

# rubric_specificity / rubric_utility / rubric_sentiment
link_scores <- function(score_table, rubric_table) {
  scores <- dbReadTable(dst, score_table) |> arrange(as.integer(value))
  rows <- data.frame(
    rubric_id = 1L,
    setNames(list(scores$id), paste0(score_table, "_id"))
  )
  dbWriteTable(dst, rubric_table, rows, append = TRUE)
  message(sprintf("  %-25s %d rows", rubric_table, nrow(rows)))
}

link_scores("specificity", "rubric_specificity")
link_scores("utility", "rubric_utility")
link_scores("sentiment", "rubric_sentiment")

# ── 6. Migrate review_assignment ─────────────────────────────────────────────

old_ra <- dbReadTable(src, "review_assignment")

# Build value→id lookup maps from the newly inserted score tables
util_map <- dbReadTable(dst, "utility") |> select(id, value)
sent_map <- dbReadTable(dst, "sentiment") |> select(id, value)

new_ra <- old_ra |>
  mutate(
    rubric_id = 1L,
    utility_score_id = util_map$id[match(
      as.character(utility),
      util_map$value
    )],
    utility_score_value = as.integer(utility),
    sentiment_score_id = sent_map$id[match(
      as.character(sentiment),
      sent_map$value
    )],
    sentiment_score_value = as.integer(sentiment)
  ) |>
  select(
    id,
    created,
    modified,
    evaluation_id,
    reviewer_id,
    rubric_id,
    statusCode,
    include_questions,
    redacted,
    utility_score_id,
    utility_score_value,
    sentiment_score_id,
    sentiment_score_value,
    tokens_in,
    tokens_out,
    duration,
    note
  )

dbWriteTable(dst, "review_assignment", new_ra, append = TRUE)
message(sprintf(
  "  %-25s %d rows  (rubric_id = 1 for all)",
  "review_assignment",
  nrow(new_ra)
))

# ── 7. Re-enable FK enforcement and verify ───────────────────────────────────

dbExecute(dst, "PRAGMA foreign_keys = ON")

cat("\n── Verification ────────────────────────────────────────\n")
for (tbl in dbListTables(dst)) {
  n <- dbGetQuery(dst, paste("SELECT COUNT(*) FROM", tbl))[[1]]
  if (n > 0) cat(sprintf("  %-30s %d rows\n", tbl, n))
}

# Spot-check: no review_assignment should have NULL rubric_id
null_rubric <- dbGetQuery(
  dst,
  "SELECT COUNT(*) FROM review_assignment WHERE rubric_id IS NULL"
)[[1]]
if (null_rubric > 0) {
  warning(null_rubric, " review_assignment rows have NULL rubric_id!")
}

# Spot-check: utility_score_id NULLs where utility was not NA in old data
util_null <- dbGetQuery(
  dst,
  "SELECT COUNT(*) FROM review_assignment WHERE utility_score_id IS NULL AND utility_score_value IS NOT NULL"
)[[1]]
if (util_null > 0) {
  warning(
    util_null,
    " review_assignment rows lost their utility_score_id mapping!"
  )
}

cat("── Done. New database at:", dst_path, "\n")
cat("── Update dbInfo in inst/review_app.R to: '../local/narrate_new.db'\n")

dbDisconnect(src)
dbDisconnect(dst)

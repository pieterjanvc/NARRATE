# Rebuild a database from the current narrate.sql schema, preserving all data.
# Handles any SQLite database already on the new-ish schema (post-rubric migration).
# Run from the project root: source("inst/rebuild_db.R")

library(RSQLite)

src_path <- "local/backup/cfme_2026-06-30.db"
dst_path <- "local/narrate.db"
schema_path <- "inst/narrate.sql"

stopifnot(file.exists(src_path), file.exists(schema_path))
if (file.exists(dst_path)) {
  file.remove(dst_path)
}

src <- dbConnect(SQLite(), src_path)
dst <- dbConnect(SQLite(), dst_path)

# ── 1. Apply new schema (includes status_codes seed data) ────────────────────

sql_stmts <- paste(readLines(schema_path), collapse = "\n") |>
  strsplit(";\n") |>
  unlist() |>
  trimws()
sql_stmts <- sql_stmts[nchar(sql_stmts) > 0]

dbExecute(dst, "PRAGMA foreign_keys = OFF")

for (stmt in sql_stmts) {
  if (!grepl("^--", stmt)) {
    tryCatch(
      dbExecute(dst, stmt),
      error = function(e) {
        message("Skipped (", conditionMessage(e), "): ", substr(stmt, 1, 60))
      }
    )
  }
}
message("Schema applied.")

# ── 2. Copy all user-data tables (status_codes seeded by schema, skip it) ────

copy_tables <- c(
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
  "rubric",
  "competency",
  "competency_diff",
  "rubric_competency",
  "specificity",
  "rubric_specificity",
  "utility",
  "rubric_utility",
  "sentiment",
  "rubric_sentiment",
  "review_assignment",
  "competency_score",
  "competency_text"
)

for (tbl in copy_tables) {
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

# ── 3. Verify ─────────────────────────────────────────────────────────────────

dbExecute(dst, "PRAGMA foreign_keys = ON")
fk_violations <- dbGetQuery(dst, "PRAGMA foreign_key_check")
if (nrow(fk_violations) > 0) {
  warning(
    nrow(fk_violations),
    " FK violation(s) detected — review before swapping files."
  )
  print(fk_violations)
} else {
  message("\nNo FK violations.")
}

dbDisconnect(src)
dbDisconnect(dst)

cat("\nDone. New database at:", dst_path, "\n")
cat("To replace: file.rename('", dst_path, "', '", src_path, "')\n", sep = "")

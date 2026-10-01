# Size the paraphrase / unlocatable-quote problem (dev/paraphrase_fix_plan.md).
#
# Dry-runs dbRelocateCompText() over every AI review that has a NULL-position
# competency_text row and reports how many of those rows are mechanically
# recoverable now (Phase 0) versus genuinely not in the evaluation text
# (Phase 2 LLM re-anchor / Phase 3 flag-for-human).
#
# Usage:  Rscript dev/size_paraphrase_na.R
# Read-only - writes nothing to the database.

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

db_path <- "local/narrate.db"
conn <- dbGetConn(db_path)
on.exit(try(dbFinish(conn), silent = TRUE), add = TRUE)

log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# ── All NULL-position competency_text rows, with review context ──────────────
na_rows <- tbl(conn, "competency_text") |>
  filter(is.na(start)) |>
  inner_join(
    tbl(conn, "competency_score") |>
      select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  inner_join(
    tbl(conn, "review_assignment") |>
      select(review_assignment_id = id, reviewer_id, rubric_id, statusCode),
    by = "review_assignment_id"
  ) |>
  select(review_assignment_id, reviewer_id, rubric_id, statusCode) |>
  collect()

log("NULL-position competency_text rows:", nrow(na_rows))
cat("\nBy reviewer / rubric / statusCode:\n")
print(as.data.frame(count(na_rows, reviewer_id, rubric_id, statusCode)))

# ── Scope: rubric-3 AI reviews (decision 1 in the plan) ─────────────────────
ai_reviews <- na_rows |>
  filter(reviewer_id == 1L, rubric_id == 3L) |>
  distinct(review_assignment_id) |>
  pull(review_assignment_id)
log("\nrubric-3 AI reviews with >=1 NULL-position row:", length(ai_reviews))

# ── Dry-run the relocate pass ──────────────────────────────────────────────
report <- dbRelocateCompText(conn, ai_reviews, dry_run = TRUE)

cat("\n== dry-run relocate report ==\n")
log("reviews processed:                    ", nrow(report))
log("NULL rows recoverable now (Phase 0):  ", sum(report$filled))
log("NULL rows still unplaced (Phase 2/3): ", sum(report$still_na))
log("already-located rows that would move: ", sum(report$moved),
    "  (kept unless apply_moves = TRUE)")

cat("\nPer-review (rows with something to do), worst first:\n")
print(as.data.frame(
  report |>
    filter(filled + moved + still_na > 0) |>
    arrange(desc(still_na), desc(filled))
))

out <- file.path(tempdir(), "size_paraphrase_na_report.rds")
saveRDS(report, out)
log("\nfull report saved to", out)

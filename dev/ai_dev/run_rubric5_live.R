# Run the live (real-time, non-batch) AI review pipeline for the remaining
# rubric-5 AI review assignments (gpt-6-luna reviewer, status 0).
# Steps: extract all -> resolve rule-2 conflicts -> score all.
#
# All extractions run back to back before any scoring, so consecutive scoring
# calls share the same instructions prefix and hit the prompt cache (the
# scoring prompt is ~1.1k tokens; the extraction prompt is ~840 tokens, below
# the 1024-token caching minimum, so extraction calls are never cached).
#
# Usage:  Rscript dev/ai_dev/run_rubric5_live.R
#
# Resumable: each step only picks up reviews at the status it expects (0 for
# extraction, 6 for resolve, 3 for scoring), so re-running after a crash or
# interrupt continues where it left off. A backup of the database is made
# before anything is written.

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# ── Settings ─────────────────────────────────────────────────────────────────
db_path <- "local/narrate.db"
reviewer_id <- 6L # gpt-6-luna
rubric_id <- 5L

Sys.setenv(HUIT_API_NARRATE = keyring::key_get("HUIT_API_NARRATE"))

log <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
}

backup_path <- sprintf(
  "local/backup/narrate_pre_rubric5_live_%s.db",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)
dir.create(dirname(backup_path), showWarnings = FALSE)
stopifnot(file.copy(db_path, backup_path))
log("Backup written to", backup_path)

conn <- dbGetConn(db_path)

ids <- tbl(conn, "review_assignment") |>
  filter(
    reviewer_id == local(reviewer_id),
    rubric_id == local(rubric_id),
    statusCode %in% c(0, 3, 6)
  ) |>
  pull(id)

status_summary <- function() {
  tbl(conn, "review_assignment") |>
    filter(id %in% local(ids)) |>
    count(statusCode) |>
    arrange(statusCode) |>
    collect()
}

log(length(ids), "review assignments to process")
print(status_summary())

# ── Step 1: extraction ───────────────────────────────────────────────────────
t0 <- Sys.time()
to_extract <- tbl(conn, "review_assignment") |>
  filter(id %in% local(ids), statusCode == 0) |>
  pull(id)

if (length(to_extract) > 0) {
  log("Extracting", length(to_extract), "reviews...")
  ext <- llm_comp_extract_run(conn, to_extract, verbose = TRUE)
  log(sprintf(
    "Extraction done in %.1f min | tokens in %d, out %d",
    as.numeric(difftime(Sys.time(), t0, units = "mins")),
    sum(ext$tokens_in, na.rm = TRUE),
    sum(ext$tokens_out, na.rm = TRUE)
  ))
  print(count(ext, statusCode))
}

# ── Step 1b: resolve rule-2 conflicts ────────────────────────────────────────
to_resolve <- tbl(conn, "review_assignment") |>
  filter(id %in% local(ids), statusCode == 6) |>
  pull(id)

if (length(to_resolve) > 0) {
  log("Resolving conflicts for", length(to_resolve), "reviews...")
  res <- llm_comp_resolve_run(conn, to_resolve, verbose = TRUE)
  print(res)
}

# ── Step 2: scoring ──────────────────────────────────────────────────────────
t1 <- Sys.time()
to_score <- tbl(conn, "review_assignment") |>
  filter(id %in% local(ids), statusCode == 3) |>
  pull(id)

if (length(to_score) > 0) {
  log("Scoring", length(to_score), "reviews...")
  sc <- llm_comp_score_run(conn, to_score, verbose = TRUE)
  log(sprintf(
    "Scoring done in %.1f min | tokens in %d, out %d",
    as.numeric(difftime(Sys.time(), t1, units = "mins")),
    sum(sc$tokens_in, na.rm = TRUE),
    sum(sc$tokens_out, na.rm = TRUE)
  ))
  print(count(sc, statusCode))
}

# ── Summary ──────────────────────────────────────────────────────────────────
log(
  sprintf(
    "All done in %.1f min. Final status (5 = scored, -2 = extraction failed,",
    as.numeric(difftime(Sys.time(), t0, units = "mins"))
  ),
  "-3 = scoring failed, -4 = conflict unresolved):"
)
print(status_summary())

dbFinish(conn)

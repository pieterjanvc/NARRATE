# Phase 0 of dev/paraphrase_fix_plan.md - backfill competency_text positions
# for AI evidence that was stored NULL only because it predates the
# whitespace-tolerant locator (committed in b25be1e).
#
# Backs up the database, runs dbRelocateCompText() over every rubric-3 AI
# review with a NULL-position row (apply_moves = FALSE - already-located rows
# are never touched), then validates:
#   * every newly filled [start, end) slices text_match back out of the
#     evaluation (whitespace-tolerant), and
#   * no row that had a position lost it.
#
# Usage:  Rscript dev/relocate_comp_text.R [--dry]

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

dry <- "--dry" %in% commandArgs(trailingOnly = TRUE)
db_path <- "local/narrate.db"
log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# ── Backup ─────────────────────────────────────────────────────────────────
if (!dry) {
  bak <- file.path(
    "local/backup",
    sprintf("narrate_pre-paraphrase-phase0_%s.db", format(Sys.time(), "%Y%m%d-%H%M%S"))
  )
  stopifnot(file.copy(db_path, bak, overwrite = FALSE))
  log("backup ->", bak)
}

conn <- dbGetConn(db_path)
on.exit(try(dbFinish(conn), silent = TRUE), add = TRUE)

# ── Target reviews: rubric-3 AI, >=1 NULL-position competency_text row ──────
targets <- tbl(conn, "competency_text") |>
  filter(is.na(start)) |>
  inner_join(
    tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  inner_join(
    tbl(conn, "review_assignment") |>
      filter(reviewer_id == 1L, rubric_id == 3L) |>
      select(review_assignment_id = id),
    by = "review_assignment_id"
  ) |>
  distinct(review_assignment_id) |>
  pull(review_assignment_id) |>
  sort()
log("target reviews:", length(targets))

# ── Snapshot positions before, for the "nothing lost" check ────────────────
before <- tbl(conn, "competency_text") |>
  inner_join(
    tbl(conn, "competency_score") |>
      filter(review_assignment_id %in% local(targets)) |>
      select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  select(id, review_assignment_id, text_match, start, end) |>
  collect()

# ── Run ────────────────────────────────────────────────────────────────────
report <- dbRelocateCompText(conn, targets, apply_moves = FALSE, dry_run = dry)
log(sprintf("filled=%d  moved(left alone)=%d  still_na=%d  written=%d",
            sum(report$filled), sum(report$moved), sum(report$still_na), sum(report$written)))

if (dry) {
  log("dry run - no writes, no validation")
  quit(save = "no")
}

# ── Validate ───────────────────────────────────────────────────────────────
after <- tbl(conn, "competency_text") |>
  inner_join(
    tbl(conn, "competency_score") |>
      filter(review_assignment_id %in% local(targets)) |>
      select(competency_score_id = id, review_assignment_id),
    by = "competency_score_id"
  ) |>
  select(id, review_assignment_id, text_match, start, end) |>
  collect()

lost <- before |>
  inner_join(after |> select(id, a_start = start), by = "id") |>
  filter(!is.na(start) & is.na(a_start))
log("positions lost (must be 0):", nrow(lost))

filled <- before |>
  select(id, b_start = start) |>
  inner_join(after, by = "id") |>
  filter(is.na(b_start) & !is.na(start))
log("positions filled:", nrow(filled))

ws_ok <- function(needle, hay) {
  if (grepl(needle, hay, fixed = TRUE)) return(TRUE)
  parts <- strsplit(trimws(needle), "[[:space:]]+")[[1]]
  parts <- parts[nzchar(parts)]
  pat <- paste(gsub("(\\W)", "\\\\\\1", parts, perl = TRUE), collapse = "[[:space:]]*")
  grepl(pat, hay, perl = TRUE)
}

bad <- 0L
for (ra_id in unique(filled$review_assignment_id)) {
  pt <- get("db_locate_text")(conn, ra_id)
  fr <- filled[filled$review_assignment_id == ra_id, ]
  for (i in seq_len(nrow(fr))) {
    slice <- substr(pt, fr$start[i] + 1L, fr$end[i])
    if (!ws_ok(fr$text_match[i], slice)) {
      bad <- bad + 1L
      cat(sprintf("  MISMATCH review %d id %d [%d,%d)\n    want: %s\n    got:  %s\n",
                  ra_id, fr$id[i], fr$start[i], fr$end[i],
                  substr(fr$text_match[i], 1, 80), substr(slice, 1, 80)))
    }
  }
}
log("filled rows whose slice != text_match (must be 0):", bad)

if (nrow(lost) == 0 && bad == 0) {
  log("VALIDATION PASSED")
} else {
  log("VALIDATION FAILED - restore from backup")
  quit(save = "no", status = 1)
}

# Phase 2 live smoke test (dev/paraphrase_fix_plan.md) - run llm_comp_reanchor_run()
# against a FEW real rubric-3 AI reviews and print each re-anchored span in
# context so it can be eyeballed before a full run / batch submit.
#
# Runs on a COPY of local/narrate.db (local/backup/narrate_reanchor-livetest_*.db)
# so the live database is never touched.
#
# Usage:  Rscript dev/reanchor_live_test.R [N]     (default N = 2 reviews)

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

# args: [N] [skip]  -> take N reviews starting after the first `skip`
.a  <- as.integer(commandArgs(trailingOnly = TRUE))
N   <- if (length(.a) >= 1 && !is.na(.a[1])) .a[1] else 2L
SKIP <- if (length(.a) >= 2 && !is.na(.a[2])) .a[2] else 0L
src <- "local/narrate.db"
work <- file.path("local/backup", sprintf("narrate_reanchor-livetest_%s.db", format(Sys.time(), "%Y%m%d-%H%M%S")))
stopifnot(file.copy(src, work))
cat("working copy:", work, "\n")

Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
conn <- dbGetConn(work)
on.exit(try(dbFinish(conn), silent = TRUE), add = TRUE)

log <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

targets <- tbl(conn, "competency_text") |>
  filter(is.na(start)) |>
  inner_join(tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
             by = "competency_score_id") |>
  inner_join(tbl(conn, "review_assignment") |>
               filter(reviewer_id == 1L, rubric_id == 3L, statusCode == 5L) |>
               select(review_assignment_id = id),
             by = "review_assignment_id") |>
  distinct(review_assignment_id) |>
  pull(review_assignment_id) |>
  sort()
targets <- head(targets[(SKIP + 1):length(targets)], N)
log("reviews:", paste(targets, collapse = ", "))

before <- tbl(conn, "competency_text") |>
  inner_join(tbl(conn, "competency_score") |>
               filter(review_assignment_id %in% local(targets)) |>
               select(competency_score_id = id, review_assignment_id),
             by = "competency_score_id") |>
  filter(is.na(start)) |>
  select(id, review_assignment_id, text_match) |>
  collect()

res <- llm_comp_reanchor_run(conn, targets, verbose = TRUE)
log("== per-review result ==")
print(as.data.frame(res))

after <- tbl(conn, "competency_text") |>
  filter(id %in% local(before$id)) |>
  select(id, text_match, start, end, locate_status) |>
  collect()

for (ra_id in targets) {
  pt <- get("db_locate_text")(conn, ra_id)
  rows <- before[before$review_assignment_id == ra_id, ]
  cat("\n===== review", ra_id, "=====\n")
  for (i in seq_len(nrow(rows))) {
    a <- after[after$id == rows$id[i], ]
    cat(sprintf("\n[ct %d]\n  paraphrase : %s\n", rows$id[i], rows$text_match[i]))
    if (!is.na(a$start)) {
      ctx0 <- max(1, a$start - 40); ctx1 <- min(nchar(pt), a$end + 40)
      cat(sprintf("  ANCHORED   : %s\n  slice      : %s\n  context    : ...%s...\n",
                  a$text_match, substr(pt, a$start + 1, a$end), substr(pt, ctx0, ctx1)))
    } else {
      cat(sprintf("  FLAGGED    : locate_status = %s\n", a$locate_status))
    }
  }
}

log("\ndone - working copy left at", work, "(delete when finished)")

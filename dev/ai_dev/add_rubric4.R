# Add rubric 4 (from dev/rubric_temp.md) to the database.
#
# Derived from rubric 3 (prev_id = 3):
#   - competencies 3, 6 and 7 have revised descriptions -> new competency rows
#   - utility moves from 3 to 4 levels                   -> new utility rows
#   - sentiment 1, 2, 4, 5 are recapitalised             -> new sentiment rows
#   - specificity (1, 2, 5, 6) and rules (1-6) are unchanged and reused
#
# A timestamped backup of the database is written to local/backup/ first.
# Refuses to run if the latest rubric is not 3 (i.e. already applied).
#
# Usage:  Rscript dev/ai_dev/add_rubric4.R
#         RUBRIC4_DB=path/to/copy.db Rscript dev/ai_dev/add_rubric4.R

suppressMessages(pkgload::load_all(here::here(), quiet = TRUE))
suppressMessages(library(dplyr))

db_path <- Sys.getenv("RUBRIC4_DB", "local/narrate.db")
prev_rubric_id <- 3L

conn <- dbGetConn(db_path)

latest_rubric_id <- tbl(conn, "rubric") |>
  summarise(id = max(id, na.rm = TRUE)) |>
  pull(id)
if (latest_rubric_id != prev_rubric_id) {
  stop(
    "Latest rubric is ",
    latest_rubric_id,
    ", expected ",
    prev_rubric_id,
    ". Has rubric 4 already been added?"
  )
}

# ── Backup ───────────────────────────────────────────────────────────────────
backup_path <- file.path(
  "local/backup",
  sprintf(
    "%s_pre-rubric4_%s.db",
    tools::file_path_sans_ext(basename(db_path)),
    format(Sys.time(), "%Y%m%d-%H%M%S")
  )
)
dir.create(dirname(backup_path), showWarnings = FALSE, recursive = TRUE)
stopifnot(file.copy(db_path, backup_path))
cat("Backup written to", backup_path, "\n")

# ── New competencies (revised versions of 12, 15, 16) ────────────────────────
new_comp <- tbl_insert(
  data.frame(
    cID = c(3L, 6L, 7L),
    name = c(
      "Provide Effective Oral and Written Professional Communication",
      "Scholarly Inquiry and Evidence-Based Medicine Integration",
      "Professionalism"
    ),
    description = c(
      paste(
        "Communicate clinical information effectively, efficiently, and",
        "professionally in oral and written formats, including concise patient",
        "presentations on rounds and well-organized clinical documentation",
        "including progress notes."
      ),
      paste(
        "Evaluate, analyze, and apply new and existing data from literature",
        "across biomedical, clinical, population, and data sciences."
      ),
      paste(
        "Exemplify integrity, social responsibility and respect for all persons.",
        "Demonstrate responsible behaviors including accountability, patient",
        "confidentiality and safety, punctuality, preparedness and situational",
        "awareness. Appropriate and engaged in clinical settings, including being",
        "curious and asking questions. Demonstrate and embody ethical standards",
        "in all professional interactions. Demonstrate desire to learn."
      )
    ),
    note = c(
      "Revised from competency 12",
      "Revised from competency 15",
      "Revised from competency 16"
    )
  ),
  conn,
  "competency"
)

# ── New utility scale (4 levels) ─────────────────────────────────────────────
new_util <- tbl_insert(
  data.frame(
    value = 1:4,
    description = c(
      "Not useful: too vague or general to act upon",
      paste(
        "Minimally useful: includes student-specific recommendations for",
        "maintaining or improving performance but recommendations are hard to",
        "act upon"
      ),
      paste(
        "Useful: 1 competency has a student-specific, actionable",
        "recommendations for maintaining or improving performance"
      ),
      paste(
        "Highly useful: multiple competencies have a student-specific,",
        "actionable recommendations for maintaining or improving performance"
      )
    ),
    example = c(
      "keep reading about important topics",
      "has strong theoretical knowledge, but should practice clinical reasoning",
      paste(
        "to strengthen clinical reasoning, commit to a prioritized differential",
        "and plan for each new patient before presenting on rounds"
      ),
      paste(
        "to strengthen clinical reasoning, commit to a prioritized differential",
        "before presenting on rounds; Make sure to always be on time."
      )
    )
  ),
  conn,
  "utility"
)

# ── Sentiment: recapitalised 1, 2, 4, 5 (examples carried over); 3 reused ───
old_sent <- tbl(conn, "sentiment") |>
  filter(id %in% c(1L, 2L, 4L, 5L)) |>
  arrange(value) |>
  collect()
new_sent <- tbl_insert(
  data.frame(
    value = old_sent$value,
    description = c(
      "Strongly negative (e.g. red flags)",
      "Negative (including coded language indicating potential criticism)",
      "Positive",
      "Strongly positive (e.g. signaling exceptional student)"
    ),
    example = old_sent$example,
    note = paste("Recapitalised from sentiment", old_sent$id)
  ),
  conn,
  "sentiment"
)
sentiment_ids <- c(new_sent$id[1:2], 3L, new_sent$id[3:4])

# ── Rubric ───────────────────────────────────────────────────────────────────
competency_ids <- c(10L, 11L, new_comp$id[1], 13L, 14L, new_comp$id[2:3], 17L)

rubric <- rubric_add(
  conn,
  competency_ids = competency_ids,
  specificity_ids = c(1L, 2L, 5L, 6L),
  utility_ids = new_util$id,
  sentiment_ids = sentiment_ids,
  info = paste(
    "Revised competencies 3, 6, 7; 4-level utility scale;",
    "recapitalised sentiment (from dev/rubric_temp.md)"
  ),
  prev_id = prev_rubric_id
)

cat("Created rubric", rubric$id, "\n")
print(rubric)

dbFinish(conn)

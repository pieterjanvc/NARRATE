#' Parse inst/rubric.md and insert competency, disambiguation, and score data into the database
#'
#' @param conn Database connection
#' @param rubric_path Path to rubric.md (defaults to package inst file)
#' @param commit (Default = TRUE) Commit the results to the database.
#'
#' @import dplyr
#' @importFrom sqlife tbl_insert
#' @importFrom stringr str_trim str_match str_detect
#'
#' @returns Invisibly, a list of inserted data frames: competency, competency_diff, score
#' @export
rubric_parsing <- function(conn, rubric_path = NULL, commit = T) {
  if (is.null(rubric_path)) {
    rubric_path <- system.file("rubric.md", package = "CFME")
    if (rubric_path == "") rubric_path <- "inst/rubric.md"
  }

  lines <- readLines(rubric_path, warn = FALSE)

  collapse_text <- function(ls) {
    ls <- str_trim(ls)
    paste(ls[nchar(ls) > 0], collapse = " ")
  }

  # Parse "- N: text..." bullet lists; continuation lines are indented with 2+ spaces
  parse_bullets <- function(ls) {
    result <- list()
    key <- NULL
    parts <- character(0)
    for (line in ls) {
      m <- str_match(line, "^- (\\S+?):\\s*(.*)")
      if (!is.na(m[1, 1])) {
        if (!is.null(key)) {
          result[[key]] <- paste(str_trim(parts), collapse = " ")
        }
        key <- m[1, 2]
        parts <- m[1, 3]
      } else if (!is.null(key) && str_detect(line, "^  \\S")) {
        parts <- c(parts, str_trim(line))
      }
    }
    if (!is.null(key)) {
      result[[key]] <- paste(str_trim(parts), collapse = " ")
    }
    result
  }

  # Section boundary lines
  scoring_line <- which(lines == "# SCORING")
  definitions_line <- which(lines == "## Definitions")
  disambig_line <- which(lines == "## Disambiguation")

  # --- Competency definitions ---
  def_block <- lines[definitions_line:(disambig_line - 1)]
  header_pos <- which(str_detect(def_block, "^### \\d+\\."))

  comp_numbers <- integer(0)
  comp_names <- character(0)
  comp_descs <- character(0)

  for (i in seq_along(header_pos)) {
    h <- header_pos[i]
    m <- str_match(def_block[h], "^### (\\d+)\\.\\s*(.*)")
    comp_numbers <- c(comp_numbers, as.integer(m[1, 2]))
    comp_names <- c(comp_names, str_trim(m[1, 3]))
    body_end <- if (i < length(header_pos)) {
      header_pos[i + 1] - 1
    } else {
      length(def_block)
    }
    comp_descs <- c(comp_descs, collapse_text(def_block[(h + 1):body_end]))
  }

  competency_df <- data.frame(
    cID = comp_numbers,
    name = comp_names,
    description = comp_descs,
    stringsAsFactors = FALSE
  )

  # --- Disambiguation ---
  disambig_block <- lines[disambig_line:(scoring_line - 1)]
  disambig_headers <- which(str_detect(disambig_block, "^### Comp "))

  diff_num1 <- integer(0)
  diff_num2 <- integer(0)
  diff_descs <- character(0)

  for (i in seq_along(disambig_headers)) {
    h <- disambig_headers[i]
    m <- str_match(disambig_block[h], "Comp (\\d+) vs\\.?\\s*(\\d+|others)")
    diff_num1 <- c(diff_num1, as.integer(m[1, 2]))
    diff_num2 <- c(diff_num2, suppressWarnings(as.integer(m[1, 3]))) # NA for "others"
    body_end <- if (i < length(disambig_headers)) {
      disambig_headers[i + 1] - 1
    } else {
      length(disambig_block)
    }
    diff_descs <- c(diff_descs, collapse_text(disambig_block[(h + 1):body_end]))
  }

  # --- Scoring ---
  scoring_block <- lines[scoring_line:length(lines)]
  cat_pos <- which(str_detect(scoring_block, "^## "))

  score_categories <- character(0)
  score_values <- character(0)
  score_descs <- character(0)
  score_examples <- character(0)

  for (i in seq_along(cat_pos)) {
    h <- cat_pos[i]
    category <- str_trim(sub("^## ", "", scoring_block[h]))
    body_end <- if (i < length(cat_pos)) {
      cat_pos[i + 1] - 1
    } else {
      length(scoring_block)
    }
    cat_block <- scoring_block[h:body_end]

    levels_h <- which(str_detect(cat_block, "^### Levels"))
    examples_h <- which(str_detect(cat_block, "^### Guiding examples"))

    if (length(levels_h) == 0) {
      next
    }

    level_end <- if (length(examples_h) > 0) {
      examples_h[1] - 1
    } else {
      length(cat_block)
    }
    levels <- parse_bullets(cat_block[(levels_h[1] + 1):level_end])
    examples <- if (length(examples_h) > 0) {
      parse_bullets(cat_block[(examples_h[1] + 1):length(cat_block)])
    } else {
      list()
    }

    for (v in names(levels)) {
      score_categories <- c(score_categories, category)
      score_values <- c(score_values, v)
      score_descs <- c(score_descs, levels[[v]])
      score_examples <- c(
        score_examples,
        if (!is.null(examples[[v]])) examples[[v]] else NA_character_
      )
    }
  }

  score_df <- data.frame(
    category = score_categories,
    value = score_values,
    description = score_descs,
    example = score_examples,
    stringsAsFactors = FALSE
  )

  # --- Insert (skip rows whose content has not changed) ---
  existing_comp <- tbl(conn, "competency") |>
    select(id, cID, name, description) |>
    collect()

  to_insert_comp <- anti_join(
    competency_df,
    existing_comp,
    by = c("cID", "name", "description")
  )
  if (nrow(to_insert_comp) > 0) {
    tbl_insert(to_insert_comp, conn, "competency", commit = commit)
  }

  # Build ID map from all DB rows that match the current competency data
  comp_id_map <- tbl(conn, "competency") |>
    select(id, cID, name, description) |>
    collect() |>
    inner_join(competency_df, by = c("cID", "name", "description")) |>
    (\(d) setNames(d$id, d$cID))()

  competency_diff_df <- data.frame(
    competency_id1 = comp_id_map[as.character(diff_num1)],
    competency_id2 = ifelse(
      is.na(diff_num2),
      NA_integer_,
      comp_id_map[as.character(diff_num2)]
    ),
    description = diff_descs,
    stringsAsFactors = FALSE
  )

  existing_diff <- tbl(conn, "competency_diff") |>
    select(competency_id1, competency_id2, description) |>
    collect()
  to_insert_diff <- anti_join(
    competency_diff_df,
    existing_diff,
    by = c("competency_id1", "competency_id2", "description")
  )
  if (nrow(to_insert_diff) > 0) {
    tbl_insert(to_insert_diff, conn, "competency_diff", commit = commit)
  }

  existing_score <- tbl(conn, "score") |>
    select(category, value, description, example) |>
    collect()
  to_insert_score <- anti_join(
    score_df,
    existing_score,
    by = c("category", "value", "description", "example")
  )
  if (nrow(to_insert_score) > 0) {
    tbl_insert(to_insert_score, conn, "score", commit = commit)
  }

  invisible(list(
    competency = to_insert_comp,
    competency_diff = to_insert_diff,
    score = to_insert_score
  ))
}

#' Check the rubric file to any updates and reprocess to generate prompts if needed
#'
#' @param conn Database connection
#'
#' @import dplyr
#'
#' @returns Table with latest set of prompts
#' @export
rubric_process <- function(conn, showWarning = F) {
  rubric_parsing(conn)
  prompts <- prompt_generate(conn)
  prompt_extract_id <- dbAddPrompt(
    prompts$extract,
    conn,
    task = "comp_extract",
    showWarning = showWarning
  )
  prompt_score_id <- dbAddPrompt(
    prompts$score,
    conn,
    task = "comp_score",
    showWarning = showWarning
  )
  tbl(conn, "prompt") |>
    group_by(task) |>
    filter(timestamp == max(timestamp, na.rm = T)) |>
    ungroup() |>
    collect()
}

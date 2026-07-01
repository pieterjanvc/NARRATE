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
    rubric_path <- system.file("rubric.md", package = "NARRATE")
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

  make_score_df <- function(cat) {
    idx <- score_categories == cat
    data.frame(
      value = as.integer(score_values[idx]),
      description = score_descs[idx],
      example = score_examples[idx],
      stringsAsFactors = FALSE
    )
  }
  specificity_df <- make_score_df("Specificity")
  utility_df <- make_score_df("Utility")
  sentiment_df <- make_score_df("Sentiment")

  # --- Insert (skip rows whose content has not changed) ---
  existing_comp <- tbl(conn, "competency") |>
    select(id, name, description) |>
    collect()

  to_insert_comp <- anti_join(
    competency_df,
    existing_comp,
    by = c("name", "description")
  )
  if (nrow(to_insert_comp) > 0) {
    tbl_insert(to_insert_comp, conn, "competency", commit = commit)
  }

  # Build ID map keyed by position number; pick the latest id if duplicates exist
  comp_id_map <- tbl(conn, "competency") |>
    select(id, name, description) |>
    collect() |>
    inner_join(
      competency_df |> select(cID, name, description),
      by = c("name", "description")
    ) |>
    group_by(cID) |>
    filter(id == max(id)) |>
    ungroup() |>
    (\(d) setNames(d$id, as.character(d$cID)))()

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

  insert_score_table <- function(df, table_name) {
    existing <- tbl(conn, table_name) |>
      select(value, description, example) |>
      collect()
    to_insert <- anti_join(
      df,
      existing,
      by = c("value", "description", "example")
    )
    if (nrow(to_insert) > 0) {
      tbl_insert(to_insert, conn, table_name, commit = commit)
    }
    to_insert
  }

  to_insert_specificity <- insert_score_table(specificity_df, "specificity")
  to_insert_utility <- insert_score_table(utility_df, "utility")
  to_insert_sentiment <- insert_score_table(sentiment_df, "sentiment")

  invisible(list(
    competency = to_insert_comp,
    competency_diff = to_insert_diff,
    specificity = to_insert_specificity,
    utility = to_insert_utility,
    sentiment = to_insert_sentiment,
    comp_ids = unname(comp_id_map[as.character(comp_numbers)])
  ))
}

#' Parse the rubric file, sync content tables, and create a versioned rubric row
#'
#' Runs \code{rubric_parsing()} to sync competency and score tables, then
#' creates a new rubric row, populates its join tables, generates both filled
#' prompt templates, stores them via \code{dbAddPrompt()}, and writes the
#' resulting prompt IDs back to the rubric row.
#'
#' If the rubric content has not changed since the last run (no new rows from
#' \code{rubric_parsing()} and the latest rubric already has both prompt IDs
#' set), the existing rubric row is returned without creating a new one.
#'
#' @param conn Database connection
#' @param showWarning Pass through to \code{dbAddPrompt()}. Default = FALSE.
#'
#' @import dplyr
#' @importFrom sqlife tbl_insert tbl_update
#'
#' @returns The rubric table row (data frame) for the active rubric
#' @export
rubric_process <- function(conn, showWarning = FALSE) {
  inserted <- rubric_parsing(conn)
  content_changed <- any(
    sapply(inserted[names(inserted) != "comp_ids"], nrow) > 0
  )

  latest_rubric <- tbl(conn, "rubric") |>
    filter(id == max(id, na.rm = TRUE)) |>
    collect()

  already_linked <- nrow(latest_rubric) > 0 &&
    !is.na(latest_rubric$prompt_extract_id) &&
    !is.na(latest_rubric$prompt_score_id)

  if (!content_changed && already_linked) {
    if (showWarning) {
      message(
        "Rubric content unchanged; using existing rubric ",
        latest_rubric$id
      )
    }
    return(latest_rubric)
  }

  # Gather the latest ID for each entity, ordered as the rubric intends.
  # rubric_parsing() already resolved name/description → competency id in
  # markdown order, so we reuse that result directly.
  latest_comp_ids <- inserted$comp_ids

  latest_spec_ids <- tbl(conn, "specificity") |>
    collect() |>
    arrange(value) |>
    pull(id)
  latest_util_ids <- tbl(conn, "utility") |>
    collect() |>
    arrange(value) |>
    pull(id)
  latest_sent_ids <- tbl(conn, "sentiment") |>
    collect() |>
    arrange(value) |>
    pull(id)

  # Create the new rubric row (prompts filled in below)
  new_rubric <- tbl_insert(
    data.frame(prompt_extract_id = NA_integer_, prompt_score_id = NA_integer_),
    conn,
    "rubric"
  )
  rubric_id <- new_rubric$id

  # Populate rubric join tables
  tbl_insert(
    data.frame(
      rubric_id = rubric_id,
      competency_id = latest_comp_ids,
      order = seq_along(latest_comp_ids)
    ),
    conn,
    "rubric_competency",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, specificity_id = latest_spec_ids),
    conn,
    "rubric_specificity",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, utility_id = latest_util_ids),
    conn,
    "rubric_utility",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, sentiment_id = latest_sent_ids),
    conn,
    "rubric_sentiment",
    returnData = FALSE
  )

  # Generate filled prompts and store them
  prompts <- prompt_generate(conn, rubric_id = rubric_id)
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

  # Write prompt IDs back to the rubric row
  tbl_update(
    data.frame(
      id = rubric_id,
      prompt_extract_id = prompt_extract_id,
      prompt_score_id = prompt_score_id
    ),
    conn,
    "rubric"
  )

  tbl(conn, "rubric") |> filter(id == local(rubric_id)) |> collect()
}

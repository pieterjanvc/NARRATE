#' Generate both prompt templates populated with the latest rubric data
#'
#' Replaces \code{\{competencies\}}, \code{\{disambiguation\}}, and per-category
#' score placeholders (e.g. \code{\{specificity\}}, \code{\{utility\}},
#' \code{\{sentiment\}}) with content queried from the database.
#' Both prompt templates are returned as a named list.
#'
#' Note: placeholder replacement uses \code{gsub(fixed = TRUE)}, not
#' \code{glue}, so JSON braces in the template OUTPUT sections are safe.
#'
#' @param conn Database connection
#' @param competency_ids Integer vector of competency \code{id} values
#'   (omit = most recently inserted set)
#' @param score_ids Integer vector of score \code{id} values
#'   (omit = most recently inserted set)
#'
#' @import dplyr
#'
#' @returns Named list with elements \code{extract} and \code{score}
#' @export
prompt_generate <- function(conn, competency_ids, score_ids) {
  comp_tbl <- tbl(conn, "competency")
  score_tbl <- tbl(conn, "score")

  # Default to the most recently inserted set of each table
  if (missing(competency_ids)) {
    latest_comp_ts <- comp_tbl |>
      summarise(ts = max(timestamp, na.rm = TRUE)) |>
      pull(ts)
    competency_ids <- comp_tbl |>
      filter(timestamp == !!latest_comp_ts) |>
      pull(id)
  }

  if (missing(score_ids)) {
    latest_score_ts <- score_tbl |>
      summarise(ts = max(timestamp, na.rm = TRUE)) |>
      pull(ts)
    score_ids <- score_tbl |> filter(timestamp == !!latest_score_ts) |> pull(id)
  }

  # --- Competencies ---
  comp_data <- comp_tbl |>
    filter(id %in% competency_ids) |>
    arrange(cID) |>
    select(cID, name, description) |>
    collect()

  competencies <- paste(
    sprintf(
      "### %d. %s\n\n%s",
      comp_data$cID,
      comp_data$name,
      comp_data$description
    ),
    collapse = "\n\n"
  )

  # --- Disambiguation (auto-filtered to supplied competency IDs) ---
  diff_data <- tbl(conn, "competency_diff") |>
    inner_join(
      comp_tbl |> select(id, cID1 = cID),
      by = c("competency_id1" = "id")
    ) |>
    left_join(
      comp_tbl |> select(id, cID2 = cID),
      by = c("competency_id2" = "id")
    ) |>
    filter(
      competency_id1 %in% competency_ids,
      is.na(competency_id2) | competency_id2 %in% competency_ids
    ) |>
    arrange(cID1) |>
    select(description, cID1, cID2) |>
    collect()

  diff_headers <- ifelse(
    is.na(diff_data$cID2),
    paste0("Comp ", diff_data$cID1, " vs. others"),
    paste0("Comp ", diff_data$cID1, " vs. ", diff_data$cID2)
  )
  disambiguation <- paste(
    sprintf("- **%s**: %s", diff_headers, diff_data$description),
    collapse = "\n\n"
  )

  # --- Scores ---
  score_data <- score_tbl |>
    filter(id %in% score_ids) |>
    select(category, value, description, example) |>
    collect() |>
    arrange(category, as.integer(value))

  score_sections <- list()
  for (cat in unique(score_data$category)) {
    rows <- score_data[score_data$category == cat, ]
    levels_text <- paste(
      sprintf("- %s: %s", rows$value, rows$description),
      collapse = "\n"
    )
    examples_text <- paste(
      sprintf("- %s: %s", rows$value, rows$example),
      collapse = "\n"
    )
    score_sections[[tolower(cat)]] <- paste0(
      levels_text,
      "\n\n**guiding examples**\n\n",
      examples_text
    )
  }

  # --- Fill templates ---
  replacements <- c(
    list(competencies = competencies, disambiguation = disambiguation),
    score_sections
  )

  fill_template <- function(path) {
    result <- paste(readLines(path, warn = FALSE), collapse = "\n")
    for (key in names(replacements)) {
      result <- gsub(
        paste0("{", key, "}"),
        replacements[[key]],
        result,
        fixed = TRUE
      )
    }
    result
  }

  extract_path <- system.file("prompt_comp_extract.md", package = "CFME")
  score_path <- system.file("prompt_comp_score.md", package = "CFME")
  if (extract_path == "") {
    extract_path <- "inst/prompt_comp_extract.md"
  }
  if (score_path == "") {
    score_path <- "inst/prompt_comp_score.md"
  }

  list(
    extract = fill_template(extract_path),
    score = fill_template(score_path)
  )
}

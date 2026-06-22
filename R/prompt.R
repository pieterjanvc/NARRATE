#' Generate both prompt templates populated with rubric data
#'
#' Replaces \code{\{competencies\}}, \code{\{disambiguation\}}, and per-category
#' score placeholders (\code{\{specificity\}}, \code{\{utility\}},
#' \code{\{sentiment\}}) with content queried from the database for the
#' specified rubric.
#'
#' Competencies and score options are ordered by the \code{order} column in the
#' rubric join tables, so the prompt reflects the rubric's intended sequence.
#'
#' Note: placeholder replacement uses \code{gsub(fixed = TRUE)}, not
#' \code{glue}, so JSON braces in the template OUTPUT sections are safe.
#'
#' @param conn Database connection
#' @param rubric_id Integer rubric ID. Defaults to the most recently created rubric.
#'
#' @import dplyr
#'
#' @returns Named list with elements \code{extract} and \code{score}
#' @export
prompt_generate <- function(conn, rubric_id = NULL) {
  if (is.null(rubric_id)) {
    rubric_id <- tbl(conn, "rubric") |>
      summarise(id = max(id, na.rm = TRUE)) |>
      pull(id)
    if (length(rubric_id) == 0 || is.na(rubric_id)) {
      stop("No rubric found in the database")
    }
  }
  rid <- rubric_id

  # --- Competencies (ordered by rubric_competency.order) ---
  comp_data <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(rid)) |>
    arrange(order) |>
    left_join(
      tbl(conn, "competency") |> select(competency_id = id, name, description),
      by = "competency_id"
    ) |>
    select(competency_id, comp_order = order, name, description) |>
    collect()

  competencies <- paste(
    sprintf(
      "### %d. %s\n\n%s",
      comp_data$comp_order,
      comp_data$name,
      comp_data$description
    ),
    collapse = "\n\n"
  )

  # --- Disambiguation (filtered to this rubric's competency set) ---
  # Use comp_data as an order lookup so no extra DB round-trip is needed
  comp_ids <- comp_data$competency_id
  order_lookup <- setNames(
    comp_data$comp_order,
    as.character(comp_data$competency_id)
  )

  diff_data <- tbl(conn, "competency_diff") |>
    filter(
      competency_id1 %in% local(comp_ids),
      is.na(competency_id2) | competency_id2 %in% local(comp_ids)
    ) |>
    select(description, competency_id1, competency_id2) |>
    collect() |>
    mutate(
      order1 = order_lookup[as.character(competency_id1)],
      order2 = order_lookup[as.character(competency_id2)]
    ) |>
    arrange(order1)

  diff_headers <- ifelse(
    is.na(diff_data$order2),
    paste0("Comp ", diff_data$order1, " vs. others"),
    paste0("Comp ", diff_data$order1, " vs. ", diff_data$order2)
  )
  disambiguation <- paste(
    sprintf("- **%s**: %s", diff_headers, diff_data$description),
    collapse = "\n\n"
  )

  # --- Score sections (ordered by rubric join table order) ---
  score_section <- function(join_table, score_table, id_col) {
    rows <- tbl(conn, join_table) |>
      filter(rubric_id == local(rid)) |>
      arrange(as.integer(value)) |>
      left_join(
        tbl(conn, score_table) |> select(id, value, description, example),
        by = setNames("id", id_col)
      ) |>
      select(value, description, example) |>
      collect()
    paste0(
      paste(sprintf("- %s: %s", rows$value, rows$description), collapse = "\n"),
      "\n\n**guiding examples**\n\n",
      paste(sprintf("- %s: %s", rows$value, rows$example), collapse = "\n")
    )
  }

  # --- Fill templates ---
  replacements <- list(
    competencies = competencies,
    disambiguation = disambiguation,
    specificity = score_section(
      "rubric_specificity",
      "specificity",
      "specificity_id"
    ),
    utility = score_section("rubric_utility", "utility", "utility_id"),
    sentiment = score_section("rubric_sentiment", "sentiment", "sentiment_id")
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

  extract_path <- system.file("prompt_comp_extract.md", package = "NARRATE")
  score_path <- system.file("prompt_comp_score.md", package = "NARRATE")
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

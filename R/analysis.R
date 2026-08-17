#' Compute normalized coverage/specificity/utility scores for review assignments
#'
#' Reproduces the quality score logic used in `inst/ai_review_app.R` as a
#' reusable, exported function. For each review, the raw metrics (n competencies
#' scored, average specificity, utility) are normalized against the range
#' available on that review's rubric, then scaled by the (normalized) weights.
#' Sentiment is intentionally excluded from this score.
#'
#' @param conn NARRATE database connection
#' @param review_ids Integer vector of `review_assignment.id` values to score
#' @param coverage_weight,specificity_weight,utility_weight Relative weights for
#'   coverage, average specificity, and utility. Normalized internally so they
#'   sum to 1, regardless of the scale they're passed in on. Default 0.35 / 0.45 / 0.20
#' @param error_on_incomplete If `TRUE` (default), `stop()` when any `review_id`
#'   is missing or not in a completed status. If `FALSE`, such ids are silently
#'   dropped before scoring
#'
#' @import dplyr
#' @returns Data frame with one row per surviving `review_id`: `review_id`,
#'   `reviewer_id`, `coverage`, `avg_specificity`, `utility`. The three metric
#'   columns are already weight-scaled, rubric-normalized fractions (0-1) such
#'   that `coverage + avg_specificity + utility` equals the total score
#'   fraction for that review
#' @export
review_scores <- function(
  conn,
  review_ids,
  coverage_weight = 0.35,
  specificity_weight = 0.45,
  utility_weight = 0.20,
  error_on_incomplete = TRUE
) {
  review_ids <- unique(review_ids)

  w_sum <- coverage_weight + specificity_weight + utility_weight
  if (w_sum == 0) {
    stop("review_scores(): weights cannot all be zero.")
  }
  w_c <- coverage_weight / w_sum
  w_s <- specificity_weight / w_sum
  w_u <- utility_weight / w_sum

  ra_codes <- status_codes(conn, "review_assignment")
  ra_done_codes <- ra_codes$code[
    ra_codes$description %in%
      c("Completed with flag", "Completed", "Batch scoring complete")
  ]

  ra_all <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(id, reviewer_id, rubric_id, statusCode, utility_score_value) |>
    collect()

  missing_ids <- setdiff(review_ids, ra_all$id)
  incomplete_ids <- union(
    missing_ids,
    ra_all$id[!(ra_all$statusCode %in% ra_done_codes)]
  )

  if (length(incomplete_ids) > 0) {
    if (error_on_incomplete) {
      stop(
        "review_scores(): the following review_id(s) are not in a completed ",
        "status (or do not exist): ",
        paste(sort(incomplete_ids), collapse = ", ")
      )
    }
    ra_all <- ra_all |> filter(!(id %in% incomplete_ids))
  }

  if (nrow(ra_all) == 0) {
    return(data.frame(
      review_id = integer(),
      reviewer_id = integer(),
      coverage = numeric(),
      avg_specificity = numeric(),
      utility = numeric()
    ))
  }

  rubric_ids <- unique(ra_all$rubric_id)

  comp_summary <- tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(ra_all$id), !is.na(specificity)) |>
    group_by(review_assignment_id) |>
    summarise(
      n_competencies = n(),
      avg_specificity = mean(specificity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect()

  rubric_n_competencies <- tbl(conn, "rubric_competency") |>
    filter(rubric_id %in% local(rubric_ids)) |>
    count(rubric_id, name = "n_total_competencies") |>
    collect()

  rubric_specificity_range <- tbl(conn, "rubric_specificity") |>
    filter(rubric_id %in% local(rubric_ids)) |>
    left_join(
      tbl(conn, "specificity") |> select(specificity_id = id, value),
      by = "specificity_id"
    ) |>
    group_by(rubric_id) |>
    summarise(
      spec_min = min(value, na.rm = TRUE),
      spec_max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect()

  rubric_utility_range <- tbl(conn, "rubric_utility") |>
    filter(rubric_id %in% local(rubric_ids)) |>
    left_join(
      tbl(conn, "utility") |> select(utility_id = id, value),
      by = "utility_id"
    ) |>
    group_by(rubric_id) |>
    summarise(
      util_min = min(value, na.rm = TRUE),
      util_max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect()

  ra_all |>
    left_join(comp_summary, by = c("id" = "review_assignment_id")) |>
    left_join(rubric_n_competencies, by = "rubric_id") |>
    left_join(rubric_specificity_range, by = "rubric_id") |>
    left_join(rubric_utility_range, by = "rubric_id") |>
    mutate(
      n_competencies = ifelse(is.na(n_competencies), 0L, n_competencies),
      coverage_norm = n_competencies / n_total_competencies,
      specificity_norm = ifelse(
        n_competencies > 0 & spec_max > spec_min,
        (avg_specificity - spec_min) / (spec_max - spec_min),
        0
      ),
      utility_norm = ifelse(
        !is.na(utility_score_value) & util_max > util_min,
        (utility_score_value - util_min) / (util_max - util_min),
        0
      ),
      # Final columns hold the weight-scaled, normalized contribution (not a
      # raw mean) so that coverage + avg_specificity + utility sums to 1.
      coverage = w_c * coverage_norm,
      avg_specificity = w_s * specificity_norm,
      utility = w_u * utility_norm
    ) |>
    select(review_id = id, reviewer_id, coverage, avg_specificity, utility)
}

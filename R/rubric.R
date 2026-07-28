#' Read the id vectors currently linked to a rubric
#'
#' Internal helper used by \code{rubric_add()} to resolve omitted arguments
#' when building a new rubric from a previous one.
#'
#' @param conn Database connection
#' @param rubric_id Integer rubric ID to read linked ids from
#'
#' @import dplyr
#'
#' @returns Named list with elements \code{competency_ids}, \code{specificity_ids},
#'   \code{utility_ids}, \code{sentiment_ids}, \code{rule_ids}
#' @keywords internal
rubric_current_ids <- function(conn, rubric_id) {
  rid <- rubric_id

  competency_ids <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(rid)) |>
    arrange(order) |>
    pull(competency_id)

  specificity_ids <- tbl(conn, "rubric_specificity") |>
    filter(rubric_id == local(rid)) |>
    left_join(
      tbl(conn, "specificity") |> select(specificity_id = id, value),
      by = "specificity_id"
    ) |>
    arrange(value) |>
    pull(specificity_id)

  utility_ids <- tbl(conn, "rubric_utility") |>
    filter(rubric_id == local(rid)) |>
    left_join(
      tbl(conn, "utility") |> select(utility_id = id, value),
      by = "utility_id"
    ) |>
    arrange(value) |>
    pull(utility_id)

  sentiment_ids <- tbl(conn, "rubric_sentiment") |>
    filter(rubric_id == local(rid)) |>
    left_join(
      tbl(conn, "sentiment") |> select(sentiment_id = id, value),
      by = "sentiment_id"
    ) |>
    arrange(value) |>
    pull(sentiment_id)

  rule_ids <- tbl(conn, "rubric_rule") |>
    filter(rubric_id == local(rid)) |>
    arrange(order) |>
    pull(rule_id)

  list(
    competency_ids = competency_ids,
    specificity_ids = specificity_ids,
    utility_ids = utility_ids,
    sentiment_ids = sentiment_ids,
    rule_ids = rule_ids
  )
}

#' Create a new rubric from content-table ids
#'
#' Builds a new, versioned \code{rubric} row from ids in the \code{competency},
#' \code{specificity}, \code{utility}, \code{sentiment}, and \code{rule} tables,
#' populates the corresponding join tables, generates both filled prompt
#' templates via \code{rubric_link_prompts()}, and returns the resulting rubric
#' row.
#'
#' Disambiguation text is not passed in directly: \code{prompt_generate()}
#' automatically includes any \code{competency_diff} rows whose competencies
#' are both present in \code{competency_ids}.
#'
#' If \code{prev_id} is supplied, any of \code{competency_ids},
#' \code{specificity_ids}, \code{utility_ids}, \code{sentiment_ids}, or
#' \code{rule_ids} may be omitted (\code{NULL}); omitted ones are carried
#' forward unchanged from \code{prev_id}. The new row's \code{prev_id} column
#' records this lineage. If \code{prev_id} is \code{NULL}, all five id
#' arguments are required.
#'
#' @param conn Database connection
#' @param competency_ids Integer vector of \code{competency.id}, in display order
#' @param specificity_ids Integer vector of \code{specificity.id}
#' @param utility_ids Integer vector of \code{utility.id}
#' @param sentiment_ids Integer vector of \code{sentiment.id}
#' @param rule_ids Integer vector of \code{rule.id}, in display order
#' @param info (Optional) Note describing this rubric version
#' @param prev_id (Optional) Rubric ID this version is derived from. When set,
#'   omitted id arguments are copied forward from this rubric.
#' @param showWarning Pass through to \code{dbAddPrompt()}. Default = FALSE.
#'
#' @import dplyr
#' @importFrom sqlife tbl_insert
#'
#' @returns The rubric table row (data frame) for the newly created rubric
#' @export
rubric_add <- function(
  conn,
  competency_ids = NULL,
  specificity_ids = NULL,
  utility_ids = NULL,
  sentiment_ids = NULL,
  rule_ids = NULL,
  info = NULL,
  prev_id = NULL,
  showWarning = FALSE
) {
  if (!is.null(prev_id)) {
    prev_ids <- rubric_current_ids(conn, prev_id)
    if (is.null(competency_ids)) competency_ids <- prev_ids$competency_ids
    if (is.null(specificity_ids)) specificity_ids <- prev_ids$specificity_ids
    if (is.null(utility_ids)) utility_ids <- prev_ids$utility_ids
    if (is.null(sentiment_ids)) sentiment_ids <- prev_ids$sentiment_ids
    if (is.null(rule_ids)) rule_ids <- prev_ids$rule_ids
  }

  missing_args <- c(
    "competency_ids", "specificity_ids", "utility_ids",
    "sentiment_ids", "rule_ids"
  )[
    sapply(
      list(competency_ids, specificity_ids, utility_ids, sentiment_ids, rule_ids),
      is.null
    )
  ]
  if (length(missing_args) > 0) {
    stop(
      "Missing required id(s): ",
      paste(missing_args, collapse = ", "),
      ". Provide them directly or via `prev_id`."
    )
  }

  new_rubric <- tbl_insert(
    data.frame(
      prompt_extract_id = NA_integer_,
      prompt_score_id = NA_integer_,
      prev_id = if (is.null(prev_id)) NA_integer_ else prev_id,
      info = if (is.null(info)) NA_character_ else info
    ),
    conn,
    "rubric"
  )
  rubric_id <- new_rubric$id

  tbl_insert(
    data.frame(
      rubric_id = rubric_id,
      competency_id = competency_ids,
      order = seq_along(competency_ids)
    ),
    conn,
    "rubric_competency",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, specificity_id = specificity_ids),
    conn,
    "rubric_specificity",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, utility_id = utility_ids),
    conn,
    "rubric_utility",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(rubric_id = rubric_id, sentiment_id = sentiment_ids),
    conn,
    "rubric_sentiment",
    returnData = FALSE
  )
  tbl_insert(
    data.frame(
      rubric_id = rubric_id,
      rule_id = rule_ids,
      order = seq_along(rule_ids)
    ),
    conn,
    "rubric_rule",
    returnData = FALSE
  )

  rubric_link_prompts(conn, rubric_id, showWarning = showWarning)
}

#' Generate and store prompts for an existing rubric row
#'
#' Runs \code{prompt_generate()} for \code{rubric_id}, stores both resulting
#' prompts via \code{dbAddPrompt()}, and writes the prompt IDs back onto the
#' rubric row.
#'
#' @param conn Database connection
#' @param rubric_id Integer rubric ID to generate and link prompts for
#' @param showWarning Pass through to \code{dbAddPrompt()}. Default = FALSE.
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#'
#' @returns The rubric table row (data frame) for \code{rubric_id}
#' @export
rubric_link_prompts <- function(conn, rubric_id, showWarning = FALSE) {
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

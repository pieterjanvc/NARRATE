#' Internal: build the competencies list and disambiguation section text for
#' a rubric
#'
#' Shared by prompt_generate() and prompt_generate_resolve() so the
#' competency/disambiguation queries aren't duplicated. disambiguation_section
#' includes its own "# DISAMBIGUATION" heading and lead-in sentence so it can
#' be dropped entirely (rather than left as an empty section) when the rubric
#' has no competency_diff rows.
#'
#' @param conn Database connection
#' @param rubric_id Integer rubric ID
#'
#' @import dplyr
#' @returns List with comp_data (data frame: competency_id, comp_order, name,
#'   description), competencies (formatted text) and disambiguation_section
#'   (formatted text, or "" if the rubric has no disambiguation rows)
prompt_build_competencies <- function(conn, rubric_id) {
  rid <- rubric_id

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

  disambiguation_section <- if (nzchar(disambiguation)) {
    paste0(
      "# DISAMBIGUATION\n",
      "When text could fit more than one competency, apply these rules to ",
      "assign it to\na single one:\n\n",
      disambiguation
    )
  } else {
    ""
  }

  list(
    comp_data = comp_data,
    competencies = competencies,
    disambiguation_section = disambiguation_section
  )
}

#' Generate both prompt templates populated with rubric data
#'
#' Replaces \code{\{competencies\}}, \code{\{disambiguation_section\}},
#' \code{\{rules\}}, and per-category score placeholders (\code{\{specificity\}},
#' \code{\{utility\}}, \code{\{sentiment\}}) with content queried from the
#' database for the specified rubric. disambiguation_section is dropped
#' entirely (not left as an empty section) when the rubric has no
#' competency_diff rows - see prompt_build_competencies().
#'
#' Competencies, rules, and score options are ordered by the \code{order}
#' column in the rubric join tables, so the prompt reflects the rubric's
#' intended sequence.
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

  comp <- prompt_build_competencies(conn, rid)
  competencies <- comp$competencies
  disambiguation_section <- comp$disambiguation_section

  # --- Rules (ordered by rubric_rule.order) ---
  rule_data <- tbl(conn, "rubric_rule") |>
    filter(rubric_id == local(rid)) |>
    arrange(order) |>
    left_join(
      tbl(conn, "rule") |> select(rule_id = id, title, description),
      by = "rule_id"
    ) |>
    select(rule_order = order, title, description) |>
    collect()

  rules <- paste(
    sprintf(
      "%d. **%s**: %s",
      rule_data$rule_order,
      rule_data$title,
      rule_data$description
    ),
    collapse = "\n\n"
  )

  # --- Score sections (ordered by rubric join table order) ---
  score_section <- function(join_table, score_table, id_col) {
    rows <- tbl(conn, join_table) |>
      filter(rubric_id == local(rid)) |>
      left_join(
        tbl(conn, score_table) |> select(id, value, description, example),
        by = setNames("id", id_col)
      ) |>
      select(value, description, example) |>
      collect() |>
      arrange(value)
    paste0(
      paste(sprintf("- %s: %s", rows$value, rows$description), collapse = "\n"),
      "\n\n**guiding examples**\n\n",
      paste(sprintf("- %s: %s", rows$value, rows$example), collapse = "\n")
    )
  }

  # --- Fill templates ---
  replacements <- list(
    competencies = competencies,
    disambiguation_section = disambiguation_section,
    rules = rules,
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

  extract_path <- prompt_template_path("prompt_comp_extract.md")
  score_path <- prompt_template_path("prompt_comp_score.md")

  list(
    extract = fill_template(extract_path),
    score = fill_template(score_path)
  )
}

#' Generate the (static, per-rubric) resolve prompt
#'
#' Companion to prompt_generate() for the conflict-resolution step: fills
#' inst/prompt_comp_resolve.md with the rubric's competencies and
#' disambiguation section. Unlike the extract/score prompts, this is not
#' cached via the prompt/rubric tables - the conflicts themselves (which vary
#' per call) are passed separately as the request's input rather than baked
#' into this instructions string, so the static part can just be regenerated
#' on demand.
#'
#' @param conn Database connection
#' @param rubric_id Integer rubric ID
#'
#' @import dplyr
#' @returns Character string (the filled resolve prompt template)
#' @export
prompt_generate_resolve <- function(conn, rubric_id) {
  comp <- prompt_build_competencies(conn, rubric_id)

  result <- paste(
    readLines(prompt_template_path("prompt_comp_resolve.md"), warn = FALSE),
    collapse = "\n"
  )
  replacements <- list(
    competencies = comp$competencies,
    disambiguation_section = comp$disambiguation_section
  )
  for (key in names(replacements)) {
    result <- gsub(paste0("{", key, "}"), replacements[[key]], result, fixed = TRUE)
  }
  result
}

#' Internal: resolve a package template file path, falling back to the
#' repo-relative inst/ path when the package isn't installed (dev mode)
#'
#' @param file Template file name (e.g. "prompt_comp_resolve.md")
#' @returns File path
prompt_template_path <- function(file) {
  path <- system.file(file, package = "NARRATE")
  if (path == "") path <- file.path("inst", file)
  path
}

#' Build the CONFLICTS section text (and applicable cluster/option mapping)
#' for the resolve prompt
#'
#' Conflicts from dbCompExtractionCheckConflicts() are pairwise (one row per
#' conflicting pair of competency_text rows), but the same quote can appear
#' in more than one pair at once (e.g. a 3-way duplicate across competencies
#' A, B and C shows up as pairs A-B, A-C and B-C). Resolving those
#' independently could give contradictory answers (A-B picks B, but A-C picks
#' A), so this groups pairwise conflicts into connected clusters first and
#' asks the model to make one decision per cluster among all of its options.
#'
#' Each conflict lists every competing quote with its own verbatim text (the
#' quotes are no longer assumed byte-identical - a "substring_duplicate"
#' conflict has genuinely different text on each side). When `conn` is
#' supplied, a `# CURRENT EXTRACTIONS` block is prepended listing every quote
#' currently extracted for the review, grouped by competency, so the model
#' can judge each decision against the whole picture (e.g. leave a contested
#' quote with the competency that would otherwise have no evidence).
#'
#' @param conflicts The conflicts data frame from
#'   dbCompExtractionCheckConflicts()
#' @param comp_data Data frame with competency_id, comp_order, name (as
#'   returned by prompt_build_competencies()$comp_data)
#' @param conn (Optional) NARRATE database connection. When given, the full
#'   current extraction set for the review is included as read-only context.
#'
#' @import dplyr
#'
#' @returns List with text (the formatted prompt body, "" if no conflicts)
#'   and clusters, a data frame (one row per option per conflictId) with
#'   columns conflictId, competency_text_id, competency_score_id,
#'   competency_id, comp_order, text, start, end
#' @export
build_resolve_conflicts <- function(conflicts, comp_data, conn = NULL) {
  empty_clusters <- data.frame(
    conflictId = integer(0), competency_text_id = integer(0),
    competency_score_id = integer(0), competency_id = integer(0),
    comp_order = integer(0), text = character(0),
    start = integer(0), end = integer(0),
    stringsAsFactors = FALSE
  )

  if (nrow(conflicts) == 0) {
    return(list(text = "", clusters = empty_clusters))
  }

  # --- Union-find over competency_text ids to group pairwise conflicts that
  # share a competency_text row into one cluster (transitively)
  all_ids <- unique(c(conflicts$competency_text_id_1, conflicts$competency_text_id_2))
  parent <- setNames(all_ids, as.character(all_ids))

  find <- function(x) {
    x <- as.character(x)
    while (parent[[x]] != as.integer(x)) {
      x <- as.character(parent[[x]])
    }
    as.integer(x)
  }
  union <- function(a, b) {
    ra <- find(a)
    rb <- find(b)
    if (ra != rb) parent[[as.character(ra)]] <<- rb
  }

  for (i in seq_len(nrow(conflicts))) {
    union(conflicts$competency_text_id_1[i], conflicts$competency_text_id_2[i])
  }

  roots <- setNames(vapply(all_ids, find, integer(1)), as.character(all_ids))

  # --- One row per distinct competency_text option, long format
  options <- rbind(
    data.frame(
      competency_text_id = conflicts$competency_text_id_1,
      competency_score_id = conflicts$competency_score_id_1,
      competency_id = conflicts$competency_id_1,
      text = conflicts$text_1,
      start = conflicts$start_1,
      end = conflicts$end_1,
      stringsAsFactors = FALSE
    ),
    data.frame(
      competency_text_id = conflicts$competency_text_id_2,
      competency_score_id = conflicts$competency_score_id_2,
      competency_id = conflicts$competency_id_2,
      text = conflicts$text_2,
      start = conflicts$start_2,
      end = conflicts$end_2,
      stringsAsFactors = FALSE
    )
  )
  options <- options[!duplicated(options$competency_text_id), ]
  options$root <- roots[as.character(options$competency_text_id)]

  cluster_roots <- unique(options$root)
  conflictId_map <- setNames(seq_along(cluster_roots), cluster_roots)
  options$conflictId <- conflictId_map[as.character(options$root)]

  options <- merge(
    options,
    comp_data[, c("competency_id", "comp_order", "name")],
    by = "competency_id"
  )
  options <- options[order(options$conflictId, options$comp_order), ]

  lines <- vapply(cluster_roots, function(root) {
    id <- conflictId_map[as.character(root)]
    rows <- options[options$conflictId == id, ]
    opts <- paste(
      sprintf("   - cIndex %d (%s): \"%s\"", rows$comp_order, rows$name, rows$text),
      collapse = "\n"
    )
    sprintf(
      paste0(
        "%d. These quotes were assigned to different competencies but ",
        "overlap in the evaluation text. Keep the shared evidence under ",
        "exactly one competency (its cIndex), or 0 to discard it.\n%s"
      ),
      id, opts
    )
  }, character(1))

  conflicts_block <- paste0(
    "# CONFLICTS\n\n", paste(lines, collapse = "\n\n")
  )

  # Optional read-only context: everything currently extracted for the review
  context_block <- ""
  if (!is.null(conn) && nrow(options) > 0) {
    ra_id <- tbl(conn, "competency_score") |>
      filter(id == local(options$competency_score_id[1])) |>
      pull(review_assignment_id)

    all_ext <- tbl(conn, "competency_score") |>
      filter(review_assignment_id == local(ra_id)) |>
      select(competency_score_id = id, competency_id) |>
      inner_join(
        tbl(conn, "competency_text") |>
          select(competency_score_id, text_match),
        by = "competency_score_id"
      ) |>
      collect() |>
      merge(
        comp_data[, c("competency_id", "comp_order", "name")],
        by = "competency_id"
      )

    if (nrow(all_ext) > 0) {
      all_ext <- all_ext[order(all_ext$comp_order), ]
      ctx_lines <- vapply(
        split(all_ext, all_ext$comp_order),
        function(g) {
          quotes <- paste(sprintf("   - \"%s\"", g$text_match), collapse = "\n")
          sprintf("cIndex %d (%s):\n%s", g$comp_order[1], g$name[1], quotes)
        },
        character(1)
      )
      context_block <- paste0(
        "# CURRENT EXTRACTIONS\n\n",
        "All quotes currently extracted for this review, for context only ",
        "(resolve only the numbered conflicts below):\n\n",
        paste(ctx_lines, collapse = "\n\n"),
        "\n\n"
      )
    }
  }

  list(
    text = paste0(context_block, conflicts_block),
    clusters = options[, c(
      "conflictId", "competency_text_id", "competency_score_id",
      "competency_id", "comp_order", "text", "start", "end"
    )]
  )
}

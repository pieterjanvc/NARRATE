# ─── Intermediate helpers for the review pipeline ────────────────────────────
# Internal functions shared between batch and real-time review workflows.
# Most of these are not exported — use the functions in review.R instead.

# ─── Request body builders ────────────────────────────────────────────────────

#' Build a responses API request body for competency extraction
#'
#' @param evaluation_text Evaluation text string
#' @param prompt System prompt (extraction instructions)
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_extract_body <- function(evaluation_text, prompt) {
  list(
    instructions = prompt,
    input = paste0(evaluation_text, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    # Real extraction output tops out around 1.2k tokens (p99 over ~750 reviews);
    # 2500 leaves 2x headroom while failing a runaway repetition loop fast
    # instead of burning to a 10k ceiling.
    max_output_tokens = 2500L
  )
}

#' Build a responses API request body for conflict resolution
#'
#' @param conflicts_text Formatted prompt body from build_resolve_conflicts()$text
#'   (already includes its own section headers)
#' @param prompt System prompt (resolve instructions, from prompt_generate_resolve())
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_resolve_body <- function(conflicts_text, prompt) {
  list(
    instructions = prompt,
    input = paste0(
      conflicts_text,
      "\n\nRespond with JSON as instructed."
    ),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 2000L
  )
}

#' Build a responses API request body for competency-evidence re-anchoring
#'
#' @param reanchor_text Formatted input body from build_reanchor_items()$text
#'   (the evaluation text followed by the numbered item list)
#' @param prompt System prompt (re-anchor instructions, i.e. the contents of
#'   inst/prompt_comp_reanchor.md)
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_reanchor_body <- function(reanchor_text, prompt) {
  list(
    instructions = prompt,
    input = paste0(reanchor_text, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 3000L
  )
}

#' Build a responses API request body for competency scoring
#'
#' @param extractions List of extraction items; each has cIndex (integer order
#'   position within the rubric) and text (character vector)
#' @param prompt System prompt (scoring instructions)
#' @returns Named list for use as a responses API body (model field excluded)
llm_build_score_body <- function(extractions, prompt) {
  user_msg <- jsonlite::toJSON(
    list(extractions = lapply(extractions, function(item) {
      list(cIndex = item$cIndex, text = as.list(item$text))
    })),
    auto_unbox = TRUE
  )
  list(
    instructions = prompt,
    input = paste0(user_msg, "\n\nRespond with JSON as instructed."),
    text = list(format = list(type = "json_object")),
    max_output_tokens = 4000L
  )
}

# ─── LLM call wrappers ───────────────────────────────────────────────────────

#' Extract competencies and verbatim text from a clerkship evaluation
#'
#' Calls the Azure responses API and parses the JSON output into a structured
#' extraction list. Shared by llm_comp_extract_run() and used as the real-time
#' counterpart of the batch extraction workflow.
#'
#' @param evaluation_text Character string with the evaluation text
#' @param prompt System prompt (extraction instructions)
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON
#' @returns List with:
#'   - statusCode: run status_codes(conn, "llm_comp_extract") for code details
#'   - data: list of extraction items on success (each has cIndex and text), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_extract <- function(
  evaluation_text,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_extract_body(evaluation_text, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("extractions" %in% names(parsed))) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  data <- parsed$extractions
  for (i in seq_along(data)) data[[i]]$text <- unlist(data[[i]]$text)

  list(
    statusCode = 2, data = data,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

#' Score extracted competencies from a clerkship evaluation
#'
#' Takes the extracted competency texts and asks the LLM to assign a
#' specificity score to each, plus overall utility and sentiment ratings.
#' Shared by llm_comp_score_run() and used as the real-time counterpart of
#' the batch scoring workflow.
#'
#' @param extractions List of extraction items from llm_comp_extract()$data;
#'   each element has cIndex (integer order position within the rubric) and text (character vector)
#' @param prompt System prompt (scoring instructions)
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON toJSON
#' @returns List with:
#'   - statusCode: run status_codes(conn, "llm_comp_score") for code details
#'   - data: list with competencies, utility and sentiment on success, NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_score <- function(
  extractions,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_score_body(extractions, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (
    is.null(parsed) ||
      !all(c("competencies", "utility", "sentiment") %in% names(parsed))
  ) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  list(
    statusCode = 2, data = parsed,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

#' Resolve rule-2 conflicts flagged by dbCompExtractionCheckConflicts()
#'
#' Calls the Azure responses API and parses the JSON output into a
#' resolutions list. Shared by the live and (future) batch resolve
#' workflows, mirroring llm_comp_extract()'s structure.
#'
#' @param conflicts_text Formatted CONFLICTS section text (build_resolve_conflicts()$text)
#' @param prompt System prompt (resolve instructions, from prompt_generate_resolve())
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON
#' @returns List with:
#'   - statusCode: run status_codes(conn, "llm_comp_extract") for code details (shared codes)
#'   - data: list with resolutions (each has conflictId and cIndex) on success, NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_resolve <- function(
  conflicts_text,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_resolve_body(conflicts_text, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("resolutions" %in% names(parsed))) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  data <- parsed$resolutions

  list(
    statusCode = 2, data = data,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

#' Re-anchor paraphrased competency evidence to a verbatim span
#'
#' Calls the Azure responses API asking, for each non-verbatim quote, for the
#' exact span of the evaluation it refers to (or `null`). Parses the JSON
#' output into an anchors list. Shared by the live and batch re-anchor
#' workflows, mirroring llm_comp_resolve()'s structure.
#'
#' @param reanchor_text Formatted input body (build_reanchor_items()$text)
#' @param prompt System prompt (inst/prompt_comp_reanchor.md contents)
#' @param model Azure deployment name. Default = "gpt-5.1"
#' @param endpoint Azure endpoint base URL
#' @param debug Return raw model output text as well. Default = FALSE
#'
#' @import httr2
#' @importFrom jsonlite fromJSON
#' @returns List with:
#'   - statusCode: 0 API error, 1 parse error, 2 success (shared with
#'     llm_comp_extract())
#'   - data: list of anchor items on success (each has itemId and anchor,
#'     anchor being a string or NULL), NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
#'   - raw: raw response text if debug = TRUE, otherwise NULL
#' @export
llm_comp_reanchor <- function(
  reanchor_text,
  prompt,
  model = "gpt-5.1",
  endpoint = "https://azure-ai.hms.edu",
  debug = FALSE
) {
  body <- llm_build_reanchor_body(reanchor_text, prompt)
  body$model <- model

  req <- request(paste0(endpoint, "/openai/v1/responses")) |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = Sys.getenv("HMS_AZURE_API")
    ) |>
    req_body_json(body) |>
    req_error(is_error = ~FALSE) |>
    req_perform()

  if (resp_status(req) != 200) {
    return(list(
      statusCode = 0, data = NULL, tokens_in = NA, tokens_out = NA,
      raw = if (debug) resp_body_string(req) else NULL
    ))
  }

  resp <- resp_body_json(req)
  raw_text <- resp$output[[1]]$content[[1]]$text
  tokens_in <- resp$usage$input_tokens
  tokens_out <- resp$usage$output_tokens

  parsed <- tryCatch(
    fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed) || !("anchors" %in% names(parsed))) {
    return(list(
      statusCode = 1, data = NULL,
      tokens_in = tokens_in, tokens_out = tokens_out,
      raw = if (debug) raw_text else NULL
    ))
  }

  list(
    statusCode = 2, data = parsed$anchors,
    tokens_in = tokens_in, tokens_out = tokens_out,
    raw = if (debug) raw_text else NULL
  )
}

# ─── DB fetch helpers ─────────────────────────────────────────────────────────

#' Blank out verbatim-repeated paragraphs within a single evaluation's text
#'
#' Some evaluations (e.g. a "Clerkship Director's Summative Comments" answer)
#' legitimately quote earlier free-text answers of the same evaluation
#' verbatim as supporting evidence - a normal, expected authoring pattern in
#' this dataset, not a data error (confirmed against the raw source
#' spreadsheet for a real flagged case). Left as-is, the AI extraction prompt
#' sees the same content twice and can end up assigning each physical copy to
#' a different competency, which violates rule 2 ("one competency per quote")
#' even though every individual quote it returns is technically real text.
#'
#' This collapses exact repeats before the text reaches the LLM: splits the
#' text into paragraphs (blank-line separated), normalizes each one (case,
#' whitespace, surrounding quote marks), and blanks out every occurrence
#' after the first of any paragraph at least min_chars long that repeats
#' elsewhere in the same text. Only the LLM's input copy is affected -
#' dbCompExtraction() re-fetches the untouched original text separately via
#' dbGetEvals() to locate/highlight whatever the model ends up quoting, so
#' the surviving (first) occurrence's real position is unaffected.
#'
#' This is a purely mechanical, exact-match pass - it does not catch
#' paraphrased restatements of the same content (different wording, same
#' underlying observation). That gap is deliberately out of scope for now;
#' dbCompExtractionCheckConflicts() remains the post-hoc safety net for
#' whatever still slips through.
#'
#' @param text Evaluation text (plain, as returned by dbGetEvals(html = FALSE))
#' @param min_chars Minimum normalized paragraph length (characters) to
#'   consider for deduplication (default 40) - shorter paragraphs are left
#'   alone even if repeated, since brief generic phrases aren't a real
#'   conflict risk and dropping them could remove real signal
#'
#' @returns The input text with second-and-later occurrences of any
#'   sufficiently long repeated paragraph removed (paragraph breaks
#'   otherwise preserved)
#' @export
dedupe_repeated_paragraphs <- function(text, min_chars = 40) {
  if (is.na(text) || !nzchar(text)) {
    return(text)
  }

  paragraphs <- strsplit(text, "\n\\s*\n")[[1]]
  if (length(paragraphs) <= 1) {
    return(text)
  }

  # dbGetEvals() glues each question's "---question text\n" header onto the
  # first paragraph of its answer (only paragraph breaks within the answer's
  # own free text get a full blank line) - strip that header before
  # comparing, purely for the comparison key, so a later bare re-quote of
  # that first paragraph elsewhere still matches it
  no_header <- sub("^---[^\n]*\n", "", paragraphs)

  norm <- tolower(trimws(gsub(
    "\\s+", " ",
    gsub("[\"'‘’“”]", "", no_header)
  )))

  seen <- character(0)
  keep <- rep(TRUE, length(paragraphs))
  for (i in seq_along(paragraphs)) {
    if (nchar(norm[i]) < min_chars) next
    if (norm[i] %in% seen) {
      keep[i] <- FALSE
    } else {
      seen <- c(seen, norm[i])
    }
  }

  paste(paragraphs[keep], collapse = "\n\n")
}

#' Fetch review info and evaluation text for the extraction step
#'
#' Joins review_assignment with the extraction prompt and full evaluation text.
#' Filters to statusCode == 0 unless force = TRUE.
#' Used by both llm_comp_extract_run() and llm_comp_extract_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with columns review_id, evaluation_id, evaluation, prompt,
#'   or NULL if there is nothing to process. evaluation has verbatim-repeated
#'   paragraphs collapsed via dedupe_repeated_paragraphs() before being sent
#'   to the LLM (see its docs) - this is the prompt input only, not what's
#'   stored/displayed elsewhere.
db_fetch_review_extract <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, evaluation_id, rubric_id) |>
    left_join(
      tbl(conn, "rubric") |> select(rubric_id = id, prompt_extract_id),
      by = "rubric_id"
    ) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_extract_id = id, prompt),
      by = "prompt_extract_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 0)) == 0) {
      warning(
        "No new review assignments to process (statusCode == 0). ",
        "Use force = TRUE to reprocess."
      )
      return(NULL)
    }
    not_new <- review_info$review_id[review_info$statusCode != 0]
    if (length(not_new) > 0) {
      warning(
        length(not_new), " review_assignment(s) skipped (statusCode != 0): ",
        paste(not_new, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 0)
    }
  }

  review_info <- select(review_info, -statusCode)

  dbGetEvals(review_info$evaluation_id, conn) |>
    select(evaluation_id, evaluation) |>
    mutate(evaluation = vapply(evaluation, dedupe_repeated_paragraphs, character(1))) |>
    left_join(review_info, by = "evaluation_id")
}

#' Fetch review info for the scoring step
#'
#' Joins review_assignment with the scoring prompt. Filters to statusCode == 3
#' (extraction complete) unless force = TRUE.
#' Used by both llm_comp_score_run() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with columns review_id, prompt, or NULL if nothing to process
db_fetch_review_score <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, rubric_id) |>
    left_join(
      tbl(conn, "rubric") |> select(rubric_id = id, prompt_score_id),
      by = "rubric_id"
    ) |>
    left_join(
      tbl(conn, "prompt") |> select(prompt_score_id = id, prompt),
      by = "prompt_score_id"
    ) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 3)) == 0) {
      warning(
        "No review assignments ready to score (statusCode == 3). ",
        "Run extraction first, or use force = TRUE."
      )
      return(NULL)
    }
    not_ready <- review_info$review_id[review_info$statusCode != 3]
    if (length(not_ready) > 0) {
      warning(
        length(not_ready), " review_assignment(s) skipped (statusCode != 3): ",
        paste(not_ready, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 3)
    }
  }

  select(review_info, -statusCode)
}

#' Fetch review info and current conflicts for the resolve step
#'
#' Joins review_assignment with rubric_id and, for each review, re-runs
#' dbCompExtractionCheckConflicts() to get its current rule-2 conflicts.
#' Filters to statusCode == 6 (Extraction conflict pending) unless force = TRUE.
#' Used by llm_comp_resolve_batch_submit() (the live path keeps this logic
#' inline in llm_comp_resolve_run()).
#'
#' The conflict check is a per-review DB read (no LLM call), so a plain
#' lapply() over review_ids is fine here rather than a single set-based query.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with one row per review: review_id, rubric_id, and a
#'   list-column `conflicts` (the data frame from
#'   dbCompExtractionCheckConflicts()$conflicts). Reviews with no current
#'   conflicts are dropped. NULL if there is nothing to process.
db_fetch_review_resolve <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, rubric_id) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 6)) == 0) {
      warning(
        "No review assignments with a pending extraction conflict ",
        "(statusCode == 6). Use force = TRUE to reprocess."
      )
      return(NULL)
    }
    not_pending <- review_info$review_id[review_info$statusCode != 6]
    if (length(not_pending) > 0) {
      warning(
        length(not_pending), " review_assignment(s) skipped (statusCode != 6): ",
        paste(not_pending, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 6)
    }
  }

  review_info <- select(review_info, -statusCode)

  review_info$conflicts <- lapply(
    review_info$review_id,
    function(rid) dbCompExtractionCheckConflicts(conn, rid)$conflicts
  )

  has_conflicts <- vapply(review_info$conflicts, function(x) nrow(x) > 0, logical(1))
  if (any(!has_conflicts)) {
    warning(
      sum(!has_conflicts), " review_assignment(s) skipped (no current conflicts): ",
      paste(review_info$review_id[!has_conflicts], collapse = ", ")
    )
    review_info <- review_info[has_conflicts, , drop = FALSE]
  }

  if (nrow(review_info) == 0) return(NULL)

  review_info
}

#' Fetch review info and unplaced competency evidence for the re-anchor step
#'
#' For each review, collects every `competency_text` row that has no located
#' position (`start IS NULL`) - the paraphrased / unlocatable AI quotes - plus
#' the evaluation text rendered the same way the extraction step saw it
#' (`dbGetEvals(html = FALSE)` then `dedupe_repeated_paragraphs()`). Filters to
#' `statusCode == 5` (Batch scoring complete) unless `force = TRUE`.
#'
#' Rows already flagged `locate_status = 'unlocated'` are still returned so a
#' later run can retry them; `dbCompReanchorApply()` clears or re-sets the
#' flag based on the new answer.
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#' @param force Skip the statusCode filter. Default = FALSE
#'
#' @import dplyr
#' @returns Data frame with one row per review: `review_id`, `rubric_id`,
#'   `evaluation` (character), and a list-column `items` (data frame:
#'   `itemId`, `competency_text_id`, `competency_name`, `text`). Reviews with
#'   no unplaced rows are dropped. NULL if there is nothing to process.
db_fetch_review_reanchor <- function(conn, review_ids, force = FALSE) {
  review_info <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    select(review_id = id, statusCode, rubric_id) |>
    collect()

  if (!force) {
    if (nrow(filter(review_info, statusCode == 5)) == 0) {
      warning(
        "No review assignments at statusCode 5 (Batch scoring complete) ",
        "among the given IDs. Use force = TRUE to reprocess."
      )
      return(NULL)
    }
    not_ready <- review_info$review_id[review_info$statusCode != 5]
    if (length(not_ready) > 0) {
      warning(
        length(not_ready), " review_assignment(s) skipped (statusCode != 5): ",
        paste(not_ready, collapse = ", ")
      )
      review_info <- filter(review_info, statusCode == 5)
    }
  }

  review_info <- select(review_info, -statusCode)

  na_rows <- tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(review_info$review_id)) |>
    select(competency_score_id = id, review_id = review_assignment_id, competency_id) |>
    inner_join(
      tbl(conn, "competency_text") |>
        filter(is.na(start)) |>
        select(competency_text_id = id, competency_score_id, text_match),
      by = "competency_score_id"
    ) |>
    inner_join(
      tbl(conn, "competency") |> select(competency_id = id, competency_name = name),
      by = "competency_id"
    ) |>
    select(review_id, competency_text_id, competency_name, text = text_match) |>
    collect() |>
    arrange(review_id, competency_text_id)

  if (nrow(na_rows) == 0) {
    warning("No unplaced competency_text rows among the given reviews.")
    return(NULL)
  }

  review_info <- review_info[review_info$review_id %in% na_rows$review_id, , drop = FALSE]

  # Evaluation text as the extraction step saw it (readable rendering, deduped)
  ra_eval <- tbl(conn, "review_assignment") |>
    filter(id %in% local(review_info$review_id)) |>
    select(review_id = id, evaluation_id) |>
    collect()

  evals <- dbGetEvals(unique(ra_eval$evaluation_id), conn) |>
    transmute(
      evaluation_id,
      evaluation = vapply(evaluation, dedupe_repeated_paragraphs, character(1))
    )

  review_info <- review_info |>
    left_join(ra_eval, by = "review_id") |>
    left_join(evals, by = "evaluation_id") |>
    select(-evaluation_id)

  review_info$items <- lapply(review_info$review_id, function(rid) {
    rows <- na_rows[na_rows$review_id == rid, ]
    data.frame(
      itemId = seq_len(nrow(rows)),
      competency_text_id = rows$competency_text_id,
      competency_name = rows$competency_name,
      text = rows$text,
      stringsAsFactors = FALSE
    )
  })

  review_info
}

#' Format the re-anchor request body for one review
#'
#' @param evaluation_text The evaluation text (readable rendering)
#' @param items Data frame with `itemId`, `competency_name`, `text` (the
#'   `items` list-column entry from db_fetch_review_reanchor())
#' @returns Single character string: the evaluation text followed by the
#'   numbered item list, ready to pass as llm_comp_reanchor()'s `reanchor_text`
build_reanchor_items <- function(evaluation_text, items) {
  item_lines <- sprintf(
    "%d. [%s] %s",
    items$itemId, items$competency_name, items$text
  )
  paste0(
    "# EVALUATION TEXT\n\n", evaluation_text,
    "\n\n# ITEMS TO RE-ANCHOR\n\n", paste(item_lines, collapse = "\n\n")
  )
}

#' Fetch extracted competency texts for a set of review assignments
#'
#' Also joins rubric_competency to include comp_order (the cIndex position used
#' when sending extractions to the scoring LLM).
#'
#' Used by both llm_comp_score_run() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param review_ids Integer vector of review_assignment IDs
#'
#' @import dplyr
#' @returns Data frame with columns review_assignment_id, competency_id,
#'   comp_order, competency_score_id, text_match
db_fetch_extractions <- function(conn, review_ids) {
  tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(review_ids)) |>
    select(review_assignment_id, competency_id, competency_score_id = id) |>
    left_join(
      tbl(conn, "review_assignment") |> select(review_assignment_id = id, rubric_id),
      by = "review_assignment_id"
    ) |>
    left_join(
      tbl(conn, "rubric_competency") |> select(rubric_id, competency_id, comp_order = order),
      by = c("rubric_id", "competency_id")
    ) |>
    left_join(
      tbl(conn, "competency_text") |> select(competency_score_id, text_match),
      by = "competency_score_id"
    ) |>
    collect()
}

#' Fetch utility and sentiment ID lookup maps for a set of rubric IDs
#'
#' Returns a named list (keyed by rubric_id as character). Each element has
#' \code{util} (columns: utility_id, value) and \code{sent}
#' (columns: sentiment_id, value). Used by the scoring pipeline to translate
#' the integer scores returned by the LLM into FK IDs for review_assignment.
#'
#' @param conn DB connection
#' @param rubric_ids Integer vector of rubric IDs
#'
#' @import dplyr
db_fetch_score_maps <- function(conn, rubric_ids) {
  lapply(setNames(rubric_ids, as.character(rubric_ids)), function(rid) {
    list(
      util = tbl(conn, "rubric_utility") |>
        filter(rubric_id == local(rid)) |>
        left_join(
          tbl(conn, "utility") |> select(utility_id = id, value),
          by = "utility_id"
        ) |>
        select(utility_id, value) |>
        collect(),
      sent = tbl(conn, "rubric_sentiment") |>
        filter(rubric_id == local(rid)) |>
        left_join(
          tbl(conn, "sentiment") |> select(sentiment_id = id, value),
          by = "sentiment_id"
        ) |>
        select(sentiment_id, value) |>
        collect()
    )
  })
}

# ─── DB write helpers ─────────────────────────────────────────────────────────

#' Update competency_score.specificity for a single review assignment
#'
#' Used by both llm_comp_score_run() and batch_score_process().
#'
#' @param conn DB connection
#' @param rid review_assignment ID
#' @param competencies List of items with cIndex (order position) and specificity fields
#' @param commit Commit the transaction. Default = FALSE
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
db_write_score_specificity <- function(conn, rid, competencies, commit = FALSE) {
  if (is.null(competencies) || length(competencies) == 0) {
    return(invisible(NULL))
  }

  rubric_id <- tbl(conn, "review_assignment") |>
    filter(id == local(rid)) |>
    pull(rubric_id)

  order_map <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(rubric_id)) |>
    select(comp_order = order, competency_id) |>
    collect()

  existing <- tbl(conn, "competency_score") |>
    filter(review_assignment_id == local(rid)) |>
    collect()

  updates <- data.frame(
    comp_order  = sapply(competencies, "[[", "cIndex"),
    specificity = sapply(competencies, function(x) x[["specificity"]] %||% NA_integer_),
    stringsAsFactors = FALSE
  ) |>
    left_join(order_map, by = "comp_order") |>
    left_join(existing |> select(id, competency_id), by = "competency_id") |>
    select(id, specificity)

  # The scoring model sometimes returns a specificity for a competency that
  # wasn't in the extraction set (an out-of-range cIndex, or one it invented) -
  # there's no competency_score row to update, so drop it rather than let
  # tbl_update choke on a NULL / duplicated primary key. Also drop rows with no
  # usable specificity, and collapse any competency scored more than once.
  dropped <- sum(is.na(updates$id) | is.na(updates$specificity))
  updates <- updates |>
    filter(!is.na(id), !is.na(specificity)) |>
    distinct(id, .keep_all = TRUE)
  if (dropped > 0) {
    warning(sprintf(
      "db_write_score_specificity(): review %s - dropped %d unmatched/empty specificity score(s)",
      rid, dropped
    ))
  }

  if (nrow(updates) > 0) {
    tbl_update(updates, conn, "competency_score", returnData = FALSE, commit = commit)
  }
}

#' Record a submitted batch job and link review assignments
#'
#' Inserts a batch record, creates batch_review links, and updates
#' review_assignment.statusCode for all submitted IDs.
#' Used by both llm_comp_extract_batch_submit() and llm_comp_score_batch_submit().
#'
#' @param conn DB connection
#' @param file_input_id File ID from llm_batch_upload()
#' @param batch_id Batch ID from llm_batch_create()
#' @param review_ids Integer vector of review_assignment IDs in this batch
#' @param review_status statusCode to set on linked review_assignments
#'
#' @importFrom sqlife tbl_insert tbl_update
#' @returns Inserted batch record data frame
db_record_batch <- function(conn, file_input_id, batch_id, review_ids, review_status) {
  batch_info <- tbl_insert(
    data.frame(
      file_input_id = file_input_id,
      batch_id = batch_id,
      statusCode = 1L,
      n_requests = length(review_ids)
    ),
    conn, "batch"
  )

  tbl_insert(
    data.frame(batch_id = batch_info$id, review_assignment_id = review_ids),
    conn, "batch_review", returnData = FALSE
  )

  tbl_update(
    data.frame(
      id = review_ids,
      statusCode = review_status,
      modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    conn, "review_assignment", returnData = FALSE, commit = TRUE
  )

  batch_info
}

# ─── Batch status and result parsing ─────────────────────────────────────────

#' Check and update the status of a batch job
#'
#' Queries the Azure batch API and updates the batch table in the database.
#'
#' @param batch_id Internal batch ID (row id in the batch table)
#' @param conn DB connection
#' @param check Query the API for an updated status. Default = TRUE
#' @param endpoint Azure endpoint base URL
#' @param api_key API key. Default = HMS_AZURE_API env var
#'
#' @import httr2
#' @importFrom sqlife tbl_update
#' @returns Updated batch info data frame
#' @export
llm_batch_status <- function(
  batch_id,
  conn,
  check = TRUE,
  endpoint = "https://azure-ai.hms.edu",
  api_key = Sys.getenv("HMS_AZURE_API")
) {
  batch_info <- tbl(conn, "batch") |> filter(id == local(batch_id)) |> collect()

  if (!batch_info$statusCode %in% c(1, 2) || !check) return(batch_info)

  status <- c(
    "validating" = 2, "in_progress" = 2, "finalizing" = 2,
    "completed" = 3, "failed" = -1, "expired" = -2,
    "cancelling" = -3, "cancelled" = -3
  )

  resp <- request(paste0(endpoint, "/openai/v1/batches/", batch_info$batch_id)) |>
    req_headers("api-key" = api_key) |>
    req_error(is_error = ~FALSE) |>
    req_perform() |>
    resp_body_json()

  statusCode <- as.integer(status[names(status) == resp$status])

  batch_update <- data.frame(
    id = batch_id,
    file_output_id = resp$output_file_id,
    checked = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    statusCode = statusCode
  )

  if (statusCode == 3 && !is.null(resp$completed_at)) {
    batch_update$finished <- format(
      as.POSIXct(resp$completed_at, origin = "1970-01-01"),
      "%Y-%m-%d %H:%M:%S"
    )
  }

  tbl_update(batch_update, conn, "batch", commit = TRUE)
}

#' Parse raw batch API output into a structured list
#'
#' Converts the raw JSONL output from llm_batch_results() into a named list
#' keyed by custom_id, with review_id, statusCode, parsed data, and token counts.
#' Used by both batch_extract_process() and batch_score_process().
#'
#' @param file_output_id output_file_id from the completed batch status object
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_extract
#' @returns Named list (one element per custom_id) with:
#'   - review_id: integer extracted from the custom_id
#'   - statusCode: run status_codes(conn, "batch_results_preprocess") for code details
#'   - data: parsed JSON object on success, NULL otherwise
#'   - tokens_in, tokens_out: integer token counts
batch_results_preprocess <- function(file_output_id) {
  raw <- llm_batch_results(file_output_id)

  lapply(names(raw), function(id) {
    r <- raw[[id]]
    review_id <- str_extract(id, "\\d+$") |> as.integer()

    if (is.null(r)) {
      return(list(
        review_id = review_id, statusCode = -1,
        data = NULL, tokens_in = NA, tokens_out = NA
      ))
    }

    rb <- r$response$body
    raw_text <- rb$output[[1]]$content[[1]]$text
    tokens_in <- rb$usage$input_tokens
    tokens_out <- rb$usage$output_tokens

    parsed <- tryCatch(
      fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) NULL
    )

    if (is.null(parsed)) {
      return(list(
        review_id = review_id, statusCode = -2,
        data = NULL, tokens_in = tokens_in, tokens_out = tokens_out
      ))
    }

    list(
      review_id = review_id, statusCode = 2,
      data = parsed, tokens_in = tokens_in, tokens_out = tokens_out
    )
  }) |>
    setNames(names(raw))
}

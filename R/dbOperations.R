#' Parse data from a combined format and insert it into the database
#'
#' @param combined_data Dataframe of the original data
#' @param dbPath Path to a (new) NARRATE database
#' @param redactedOnly (Default = FALSE) If TRUE, only redacted evlaluations are
#' put into the database the version with identifiers is omitted
#'
#' @import dplyr
#' @import RSQLite
#' @import sqlife
#' @importFrom stringr str_replace_all
#'
#' @returns TRUE if success
#' @export
#'
dbAddEvaluations <- function(combined_data, dbPath, redactedOnly = F) {
  # Lowercase for all columnnames
  colnames(combined_data) <- str_replace_all(
    tolower(colnames(combined_data)),
    "\\s+",
    "_"
  )
  # Edit data types
  combined_data <- lapply(combined_data, function(x) {
    if (class(x)[1] == "numeric") {
      as.integer(x)
    } else {
      as.character(x)
    }
  }) |>
    as.data.frame() |>
    rename(original_evaluator_id = evaluator_id)

  data <- combined_data

  # Create / access the database
  schema <- system.file("extdata", "narrate.sql", package = "NARRATE")

  if (schema == "") {
    schema <- "inst/narrate.sql"
  }

  result <- dbSetup(dbPath, schema, validateSchema = T)
  conn <- dbGetConn(dbPath)

  # --- Insert student data
  student <- data |>
    select(
      learner_anon_id,
      pce_assign,
      society,
      acad_prog,
      acad_prog_trk,
      gender,
      urim_flg,
      age
    ) |>
    distinct()

  student <- tbl_insert(student, conn, "student", commit = F)

  # Add the new student ID to the data
  data <- data |>
    left_join(
      student |> select(learner_anon_id, student_id = id),
      by = "learner_anon_id"
    )

  # --- Insert evaluator data
  #  Given their title can change over time, the evaluator_id is NOT unique
  evaluator <- data |>
    select(
      original_evaluator_id,
      evaluator,
      acad_title
    ) |>
    distinct()

  evaluator <- tbl_insert(evaluator, conn, "evaluator", commit = F)

  # Add the new evaluator ID to the data
  data <- data |>
    left_join(
      evaluator |> select(original_evaluator_id, acad_title, evaluator_id = id),
      by = c("original_evaluator_id", "acad_title")
    )

  # --- Insert clerkship data
  clerkship <- data |>
    select(
      clerkship,
      location
    ) |>
    distinct()

  clerkship <- tbl_insert(clerkship, conn, "clerkship", commit = F)

  # Add the new clerkship ID to the data
  data <- data |>
    left_join(
      clerkship |> select(clerkship, clerkship_id = id),
      by = c("clerkship")
    )

  # --- Insert rotation data
  rotation <- data |>
    select(
      student_id,
      clerkship_id,
      rotation_date,
      first_nbme_score,
    ) |>
    distinct() |>
    mutate(rotation_date = format(as.Date(rotation_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")), "%Y-%m-%d"))

  check <- rotation |> group_by(student_id, clerkship_id) |> filter(n() > 1)
  if (nrow(check) > 0) {
    head(check)
    stop("Rotations are not unique")
  }

  rotation <- tbl_insert(rotation, conn, "rotation", commit = F)

  # Add the new rotation ID to the data
  data <- data |>
    left_join(
      rotation |> select(student_id, clerkship_id, rotation_id = id),
      by = c("student_id", "clerkship_id")
    )

  # --- Insert evaluation data
  evaluation <- data |>
    group_by(
      rotation_id,
      evaluator_id,
      summary_flg,
      acad_yr
    ) |>
    mutate(
      complete = case_when(
        summary_flg[1] == "Y" & n() > 3 ~ 1,
        summary_flg[1] == "N" & n() > 2 ~ 1,
        TRUE ~ 0
      )
    ) |>
    ungroup() |>
    select(rotation_id, evaluator_id, summary_flg, acad_yr, complete) |>
    distinct() |>
    mutate(summary_flg = ifelse(summary_flg == "Y", 1, 0))

  check <- evaluation |>
    group_by(rotation_id, evaluator_id, summary_flg) |>
    filter(n() > 1)
  if (nrow(check) > 0) {
    head(check)
    stop("Evaluations are not unique")
  }

  evaluation <- tbl_insert(evaluation, conn, "evaluation", commit = F)

  # Add the new evaluation ID to the data
  data <- data |>
    left_join(
      evaluation |>
        select(rotation_id, evaluator_id, evaluation_id = id, summary_flg) |>
        mutate(summary_flg = ifelse(summary_flg == 1, "Y", "N")),
      by = c("rotation_id", "evaluator_id", "summary_flg")
    )

  # --- Insert question data
  question <- data |>
    select(
      question
    ) |>
    distinct()

  question <- tbl_insert(question, conn, "question", commit = F)

  # Add the new question ID to the data
  data <- data |>
    left_join(
      question |> select(question, question_id = id),
      by = c("question")
    )

  # --- Insert answer data
  answer <- data |>
    select(
      question_id,
      evaluation_id,
      submission_date,
      if (redactedOnly) {
        NULL
      } else {
        "answer_txt"
      },
      answer_txt_redacted,
      rowid
    ) |>
    distinct()

  answer <- tbl_insert(answer, conn, "answer", commit = F)

  # --- SANITY CHECK

  # Rejoin all data
  check <- answer |>
    left_join(question, by = c("question_id" = "id")) |>
    left_join(evaluation, by = c("evaluation_id" = "id")) |>
    left_join(rotation, by = c("rotation_id" = "id")) |>
    left_join(evaluator, by = c("evaluator_id" = "id")) |>
    left_join(clerkship, by = c("clerkship_id" = "id")) |>
    left_join(student, by = c("student_id" = "id")) |>
    mutate(summary_flg = ifelse(summary_flg == 1, "Y", "N"))

  # Get the same columns as the original
  colIdx <- sapply(
    colnames(combined_data),
    function(x) {
      which(x == colnames(check))
    },
    USE.NAMES = F
  ) |>
    unlist()

  check <- check[, colIdx] |> arrange(rowid)

  # Check number of rows
  if (nrow(check) != nrow(combined_data)) {
    stop(
      "Something went wrong and the processed data ",
      "does not have the same number of rows as the original"
    )
  }

  # rotation_date is reformatted to "%Y-%m-%d" when inserted into the
  # rotation table, so normalize it the same way before comparing
  combined_data_check <- combined_data |>
    mutate(
      rotation_date = format(
        as.Date(rotation_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
        "%Y-%m-%d"
      )
    )

  #Check if data matches
  if (!all(check == combined_data_check, na.rm = T)) {
    stop(
      "Something went wrong and the processed data does not match the original"
    )
  }

  if (redactedOnly) {
    check <- check |> select(-answer_txt)
  }

  missingVals <- check[!complete.cases(check), ]

  if (nrow(missingVals) > 0) {
    warning(
      "The following rowid have missing values: ",
      paste(missingVals$rowid, collapse = ", ")
    )
  }

  dbFinish(conn)

  return(T)
}

#' Get the evaluation text from the database
#'
#' @param ids A vector of evaluation IDs to retrieve text for
#' @param conn NARRATE database connection
#' @param redacted (Default = TRUE) Show redacted text.
#' Can also be a vector of length ids
#' @param includeQuestions (Default = TRUE) Add the questions to the text.
#' Can also be a vector of length ids
#' @param html (Default = FALSE) Output HTML instead of plain text.
#' Can also be a vector of length ids
#' @param subtitleTag (Default = "h3") In case of HTML = T which tag to use for
#' questions (i.e. subtitle)
#'
#' @import dplyr
#' @importFrom stringr str_trim
#'
#' @returns A data frame with a text summary for each evaluation
#' @export
dbGetEvals <- function(
  ids,
  conn,
  redacted = T,
  includeQuestions = T,
  html = F,
  subtitleTag = "h3"
) {
  toFilter <- ids
  evals <- tbl(conn, "answer") |>
    inner_join(
      tbl(conn, "evaluation") |>
        filter(id %in% toFilter) |>
        select(id, rotation_id, summary_flg, complete),
      by = c("evaluation_id" = "id")
    ) |>
    left_join(tbl(conn, "question"), by = c("question_id" = "id")) |>
    left_join(
      tbl(conn, "rotation") |> select(id, clerkship_id),
      by = c("rotation_id" = "id")
    ) |>
    left_join(tbl(conn, "clerkship"), by = c("clerkship_id" = "id")) |>
    collect() |>
    left_join(
      data.frame(
        evaluation_id = ids,
        redacted = redacted,
        includeQuestions = includeQuestions,
        html = html,
        subtitleTag = subtitleTag
      ),
      by = "evaluation_id"
    ) |>
    mutate(
      # Choose redacted or full
      text = ifelse(redacted, answer_txt_redacted, answer_txt),
      # Clean up whitespace
      text = ifelse(
        html,
        str_replace_all(str_trim(text), "\n", "<br>"),
        str_trim(text)
      ),
      # Add questions if needed
      text = ifelse(
        includeQuestions,
        paste0(
          ifelse(html, sprintf("<%s>", subtitleTag), "---"),
          question,
          ifelse(html, sprintf("</%s><br>", subtitleTag), "\n"),
          text
        ),
        text
      )
    )

  evals <- evals |>
    group_by(evaluation_id) |>
    arrange(question_id) |>
    summarise(
      summary = summary_flg[1] == 1,
      complete = complete[1] == 1,
      clerkship = clerkship[1],
      evaluation = paste(
        text,
        sep = "",
        collapse = ifelse(html, "<br><br>", "\n\n")
      ),
      .groups = "drop"
    )
  return(evals)
}

#' Add a new prompt to the database
#'
#' @param prompt Single string of system prompt text
#' @param conn NARRATE database connection
#' @param note (Optional) Note about this prompt
#' @param commit (Default = TRUE) Commit the transaction
#' @param showWarning (Default = TRUE) Show warning if prompt already exists
#'
#' @import dplyr
#' @importFrom rlang hash
#'
#' @returns Prompt ID
#' @export
#'
dbAddPrompt <- function(
  prompt,
  conn,
  note,
  task = NULL,
  commit = T,
  showWarning = T
) {
  # Check if the prompt already exists
  prompt_hash <- hash(prompt)
  promptID <- tbl(conn, "prompt") |>
    filter(hash == local(prompt_hash)) |>
    pull(id)

  # Add new prompt if needed
  if (length(promptID) == 0) {
    # parsed <- parsePrompt(prompt)
    # if (!parsed$success) {
    #   stop(parsed$msg)
    # }

    toInsert <- data.frame(
      hash = prompt_hash,
      prompt = prompt
    )

    if (!missing(note)) {
      toInsert$note <- note
    }

    if (!is.null(task)) {
      toInsert$task <- task
    }

    promptID <- tbl_insert(toInsert, conn, "prompt", commit = commit) |>
      pull(id)
  } else if (showWarning) {
    warning("The provided prompt already is in the database")
  }

  return(promptID)
}

#' Insert or update into review score table
#'
#' @param conn NARRATE database connection
#' @param statusCode Set the review_assignment status; run status_codes(conn, "review_assignment") for code details
#' @param overallScores Data frame matching review_assignment table which
#' contains the overall scores
#' @param compScores Data frame matching competency_scores table (new IDs will be generated)
#' @param compText Data frame matching competency_text table (new IDs will be generated)
#' @param commit (Default = TRUE) Commit the transaction
#'
#' @import sqlife dplyr
#'
#' @returns A list with the updated results from the database
#' @export
#'
dbReviewUpdate <- function(
  conn,
  statusCode,
  overallScores,
  compScores,
  compText,
  removeNotListed = F,
  commit = T
) {
  # Update the review_assignment table
  if (!missing(overallScores)) {
    # New overall scores
    overallScores$statusCode <- statusCode
  } else {
    # No new overall scores
    overallScores <- compScores |>
      select(id = review_assignment_id) |>
      distinct() |>
      mutate(statusCode = statusCode)
  }

  # Add the modification timestamp
  overallScores$modified <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  overallScores <- tbl_update(
    overallScores,
    conn,
    "review_assignment",
    commit = F
  )

  # End if only overallScores were provided
  if (missing(compScores)) {
    if (commit) {
      dbCommit(conn)
    }

    compScores <- tbl(conn, "competency_score") |>
      filter(review_assignment_id %in% local(overallScores$id)) |>
      collect()

    compText <- tbl(conn, "competency_text") |>
      filter(competency_score_id %in% local(compScores$id)) |>
      collect()

    return(list(
      overallScores = overallScores,
      compScores = compScores,
      compText = compText
    ))
  }

  # Delete existing competency results
  #  competency_text has a cascading delete so will clean up automatically
  toDelete <- tbl(conn, "competency_score") |>
    filter(
      review_assignment_id %in% local(overallScores$id)
    ) |>
    select(id, review_assignment_id, competency_id) |>
    collect()

  # In the app removeNotListed = F as competencies can be updated one by one
  if (!removeNotListed) {
    toDelete <- toDelete |>
      inner_join(
        compScores |> select(review_assignment_id, competency_id),
        by = c("review_assignment_id", "competency_id")
      )
  }

  tbl_delete(toDelete, conn, "competency_score", commit = F, returnData = F)

  # Add new results
  newScores <- tbl_insert(compScores, conn, "competency_score", commit = F)
  compText <- compText |>
    left_join(
      newScores |>
        select(competency_score_id = id, review_assignment_id, competency_id),
      by = c("review_assignment_id", "competency_id")
    ) |>
    select(competency_score_id, text_match, start, end)
  tbl_insert(compText, conn, "competency_text", commit = commit)

  # Return all scores for this review, not just the newly inserted row
  compScores <- tbl(conn, "competency_score") |>
    filter(review_assignment_id %in% local(overallScores$id)) |>
    collect()
  compText <- tbl(conn, "competency_text") |>
    filter(competency_score_id %in% local(compScores$id)) |>
    collect()

  return(list(
    overallScores = overallScores,
    compScores = compScores,
    compText = compText
  ))
}

#' Internal function to insert or update into reviewer table
#'
#' @param conn SQLite connection
#' @param data Data frame with table columns
#' @param commit (Default = TRUE) Commit the transaction
#'
#' @importFrom sqlife tbl_update tbl_insert
#'
#' @returns Inserted / Updated data frame
dbReviewer <- function(conn, data, commit = T) {
  if ("id" %in% colnames(data)) {
    # Update existing
    return(tbl_update(data, conn, "reviewer", commit = commit))
  } else {
    # Create new
    return(tbl_insert(data, conn, "reviewer", commit = commit))
  }
}

#' Insert or Update human reviewer info into the database
#'
#' @param conn NARRATE database connection
#' @param id (Optional) Reviewer id. If provided this means updating existing.
#' If not, a new reviewer will be created
#' @param username Username. Required if new reviewer
#' @param first (Optional) first name
#' @param last (Optional) last name
#' @param note (Optional) note
#' @param commit (Default = T) Commit the changes to the database
#'
#' @import sqlife dplyr
#'
#' @returns Data frame with inserted / updated reviewer info
#'
#' @export
dbReviewerHuman <- function(
  conn,
  id,
  username,
  first,
  last,
  note,
  commit = T
) {
  if (!missing(id)) {
    check <- id
    x <- id
    id <- tbl(conn, "reviewer") |>
      filter(id %in% x, human == 1) |>
      pull(id)
    # Check if exists
    if (length(id) == 0) {
      stop(
        "No human reviewer exists with id ",
        check,
        ". Omit id to create new reviewer"
      )
    }
  } else if (missing(username)) {
    stop("A new human reviewer needs at least a username")
  } else {
    x <- username
    check <- tbl(conn, "reviewer") |>
      filter(username %in% x) |>
      pull(username)
    if (length(check) > 0) {
      stop(sprintf(
        "Reviewers with username %s already exist",
        paste(check, collapse = ", ")
      ))
    }
  }

  # Create the data frame needed for insertion into reviewer table
  reviewer <- data.frame(
    id = missingVal(id),
    human = T,
    username = missingVal(username),
    first_name = missingVal(first),
    last_name = missingVal(last),
    note = missingVal(note)
  )
  # Only keep columns with any new info
  reviewer <- reviewer[, apply(reviewer, 2, function(x) !all(is.na(x)))]

  result <- dbReviewer(conn, reviewer, commit = commit)
  return(result)
}

#' Insert or Update AI reviewer info into the database
#'
#' @param conn NARRATE database connection
#' @param id (Optional) Reviewer id. If provided this means updating existing.
#' If not, a new reviewer will be created
#' @param model AI model name. Required if new reviewer
#' @param note (Optional) Text note
#' @param commit (Default = T) Commit the changes to the database
#'
#' @import sqlife dplyr
#'
#' @returns Data frame with inserted / updated reviewer info
#'
#' @export
dbReviewerAI <- function(
  conn,
  id,
  model,
  note,
  commit = T
) {
  if (!missing(id)) {
    check <- id
    id <- tbl(conn, "reviewer") |>
      filter(id %in% {{ id }}, human == 0) |>
      pull(id)
    # Check if exists
    if (length(id) == 0) {
      stop(
        "No AI reviewer exists with id ",
        check,
        ". Omit id to create new AI reviewer"
      )
    }
  } else if (missing(model)) {
    stop("A new AI reviewer needs model name")
  } else {
    x <- model
    check <- tbl(conn, "reviewer") |>
      filter(model == x) |>
      pull(id)
    if (length(check) > 0) {
      stop(sprintf("A reviewer with model name %s already exists", model))
    }
  }

  # Create the data frame needed for insertion into reviewer table
  reviewer <- data.frame(
    id = missingVal(id),
    human = F,
    username = missingVal(model),
    model = missingVal(model),
    note = missingVal(note)
  )
  # Only keep columns with any new info
  reviewer <- reviewer[, apply(reviewer, 2, function(x) !all(is.na(x)))]

  result <- dbReviewer(conn, reviewer, commit = commit)

  return(result)
}

#' Insert or update a review assignment
#'
#' @param conn NARRATE database connection
#' @param id (Optional) Review assignment ID. If not set, new entry is created
#' @param reviewer_id (Required if id not set)
#' @param evaluation_id (Required if id not set)
#' @param rubric_id (Optional) Rubric ID. Defaults to the most recently created rubric.
#' @param include_questions (Optional value)
#' @param redacted (Optional value)
#' @param duration (Optional value)
#' @param statusCode (Optional value)
#' @param tokens_in (Optional value)
#' @param tokens_out (Optional value)
#' @param note (Optional value)
#' @param timestamp (Optional value)
#' @param commit (Default = T)
#'
#' @returns A data frame with inserted / updated database records in review_assignment table
#' @export
dbReviewAssignment <- function(
  conn,
  id,
  reviewer_id,
  evaluation_id,
  rubric_id,
  include_questions,
  redacted,
  duration,
  statusCode,
  tokens_in,
  tokens_out,
  note,
  timestamp,
  commit = T
) {
  data <- getFunArgs(c("conn", "commit")) |> as.data.frame()

  if (missing(id)) {
    # New
    data$statusCode <- 0
    if (missing(redacted)) {
      data$redacted <- T
    } else {
      redactedOnly <- tbl(conn, "answer") |>
        slice_sample(n = 5) |>
        pull(answer_txt) |>
        is.na() |>
        sum() ==
        5
      if (redactedOnly & redacted == F) {
        stop("This database only contains redacted evaluations")
      }
    }

    if (missing(rubric_id)) {
      data$rubric_id <- tbl(conn, "rubric") |>
        summarise(id = max(id, na.rm = TRUE)) |>
        pull(id)
      if (length(data$rubric_id) == 0 || is.na(data$rubric_id)) {
        stop("No rubric found. Run rubric_add() first.")
      }
    }

    result <- tbl_insert(data, conn, "review_assignment", commit = commit)
  } else {
    # Existing
    result <- tbl_update(data, conn, "review_assignment", commit = commit)
  }

  return(result)
}

#' Insert or update a list of extracted competencies
#'
#' @param conn NARRATE database connection
#' @param review_assignment_id Review assignment ID
#' @param comp_extraction List as generated by llm_comp_extract() (i.e. result$data),
#' where each element has a cIndex (integer order position within the rubric) and
#' text (character vector)
#' @param return_tables (Default = F) If TRUE, returns competency_score and
#' competency_text as dataframes in the result list
#' @param commit (Default = T)
#'
#' @import dplyr
#' @importFrom sqlife tbl_insert tbl_update tbl_delete
#'
#' @returns A list with success (T/F) and optionally competency_score and
#' competency_text dataframes if return_tables = TRUE. success is FALSE
#' without writing anything if comp_extraction contains duplicate cIndex
#' values (the LLM should never emit the same cIndex twice).
#' @export
dbCompExtraction <- function(
  conn,
  review_assignment_id,
  comp_extraction,
  return_tables = F,
  commit = T
) {
  ra_id <- review_assignment_id

  new_indexes <- sapply(comp_extraction, "[[", "cIndex")
  if (anyDuplicated(new_indexes) > 0) {
    return(list(success = FALSE))
  }

  # Resolve cIndex (order position) → competency_id via rubric_competency
  ra_info <- tbl(conn, "review_assignment") |>
    filter(id == local(ra_id)) |>
    select(rubric_id, evaluation_id, redacted) |>
    collect()
  rubric_id <- ra_info$rubric_id

  order_map <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(rubric_id)) |>
    select(comp_order = order, competency_id) |>
    collect()

  new_comp_ids <- order_map$competency_id[match(
    new_indexes,
    order_map$comp_order
  )]

  # A cIndex the LLM returned may not exist in the rubric's current
  # competency set (e.g. it was removed after the prompt was generated, or
  # the LLM hallucinated an out-of-range index) - reject rather than insert
  # a NULL competency_id.
  if (anyNA(new_comp_ids)) {
    return(list(success = FALSE))
  }

  # Get existing competency_score entries for this review_assignment_id
  existing_scores <- tbl(conn, "competency_score") |>
    filter(review_assignment_id == local(ra_id)) |>
    collect()

  # --- competency_score: update existing (reset specificity) or insert new
  to_update <- existing_scores |>
    filter(competency_id %in% new_comp_ids)

  if (nrow(to_update) > 0) {
    tbl_update(
      to_update |> select(id) |> mutate(specificity = NA),
      conn,
      "competency_score",
      commit = commit
    )
  }

  to_insert_ids <- new_comp_ids[
    !new_comp_ids %in% existing_scores$competency_id
  ]

  if (length(to_insert_ids) > 0) {
    tbl_insert(
      data.frame(
        review_assignment_id = review_assignment_id,
        competency_id = unname(to_insert_ids)
      ),
      conn,
      "competency_score",
      commit = commit
    )
  }

  # Refresh scores to get IDs for newly inserted rows
  all_scores <- tbl(conn, "competency_score") |>
    filter(review_assignment_id == local(ra_id)) |>
    collect()

  # Plain text (tag-stripped) the extracted quotes are matched against, in the
  # same coordinate space mod_highlight_server renders highlights against
  plainText <- db_locate_text(
    conn,
    review_assignment_id = ra_id,
    redacted = ra_info$redacted
  )

  # Locate every extracted quote up front (not per-competency) so matches
  # claim non-overlapping ranges across the whole review, not just within
  # their own competency
  all_texts <- unlist(lapply(comp_extraction, "[[", "text"), use.names = FALSE)
  all_pos <- if (length(all_texts) > 0) {
    mod_highlight_locate(plainText, all_texts)
  } else {
    data.frame(start = integer(0), end = integer(0))
  }

  # --- competency_text: delete existing and insert new for each extracted item
  all_text_new <- list()
  posCursor <- 0L

  for (item in comp_extraction) {
    comp_id <- order_map$competency_id[order_map$comp_order == item$cIndex]
    texts <- item$text
    item_pos <- all_pos[posCursor + seq_along(texts), , drop = FALSE]
    posCursor <- posCursor + length(texts)

    score_id <- all_scores |>
      filter(competency_id == local(comp_id)) |>
      pull(id)

    existing_text <- tbl(conn, "competency_text") |>
      filter(competency_score_id == local(score_id)) |>
      collect()

    if (nrow(existing_text) > 0) {
      tbl_delete(
        existing_text |> select(id),
        conn,
        "competency_text",
        commit = commit
      )
    }

    if (length(texts) > 0) {
      new_text <- tbl_insert(
        data.frame(
          competency_score_id = score_id,
          text_match = texts,
          start = item_pos$start,
          end = item_pos$end
        ),
        conn,
        "competency_text",
        commit = commit
      )
      all_text_new[[length(all_text_new) + 1]] <- new_text
    }
  }

  result <- list(success = T)

  if (return_tables) {
    result$competency_score <- all_scores |>
      filter(competency_id %in% new_comp_ids)
    result$competency_text <- bind_rows(all_text_new)
  }

  return(result)
}

#' Rebuild the tag-stripped evaluation text competency_text offsets live in
#'
#' `competency_text.start/end` are measured against the evaluation rendered
#' with `dbGetEvals(html = TRUE, subtitleTag = "b")` and then tag-stripped -
#' the exact string `mod_highlight_server` highlights against in
#' `inst/review_app.R` (`evalText()`). Both `dbCompExtraction()` (at
#' extraction time) and `dbRelocateCompText()` (for backfills) need that
#' string; this keeps the two in lockstep.
#'
#' @param conn NARRATE database connection
#' @param review_assignment_id Review assignment ID
#' @param redacted (Optional) The review_assignment's `redacted` flag, if
#'   already loaded; looked up when missing. `NA` is treated as `TRUE`.
#'
#' @import dplyr
#'
#' @returns A single tag-stripped character string.
#' @keywords internal
db_locate_text <- function(conn, review_assignment_id, redacted = NULL) {
  ra_id <- review_assignment_id

  ra_info <- tbl(conn, "review_assignment") |>
    filter(id == local(ra_id)) |>
    select(evaluation_id, redacted) |>
    collect()

  if (!is.null(redacted)) {
    ra_info$redacted <- redacted
  }

  dbGetEvals(
    ids = ra_info$evaluation_id,
    conn = conn,
    redacted = if (is.na(ra_info$redacted)) TRUE else as.logical(ra_info$redacted),
    includeQuestions = TRUE,
    html = TRUE,
    subtitleTag = "b"
  ) |>
    pull(evaluation) |>
    mod_highlight_strip_tags()
}

#' Backfill competency_text positions for AI evidence stored without one
#'
#' `mod_highlight_locate()` records `start/end = NA` when a quote can't be
#' placed. Some of those are genuine paraphrases (the text is not in the
#' evaluation), but others are verbatim - or whitespace-equivalent - quotes
#' that were extracted before `mod_highlight_locate()` gained its
#' whitespace-tolerant fallback, so a position was simply never written. This
#' function recovers the second group. No LLM, no change to `text_match`,
#' fully reversible from a database backup.
#'
#' For each affected review the review's **entire** `competency_text` set is
#' re-located in insertion (`id`) order, reproducing the non-overlapping
#' claim order `dbCompExtraction()` established, so a backfilled row claims a
#' span consistent with the rows around it. By default only rows that were
#' `NA` are written back; a row that already has a position is never moved
#' and never reset to `NA` (pass `apply_moves = TRUE` to also rewrite
#' already-located rows whose recomputed span differs - inspect the `moved`
#' count from a `dry_run` first).
#'
#' @param conn NARRATE database connection
#' @param review_assignment_ids (Optional) Restrict to these review
#'   assignments. Default: every AI (`reviewer_id` 1) review with at least one
#'   `competency_text` row whose `start` is `NULL`.
#' @param apply_moves (Default = FALSE) Also write back already-located rows
#'   whose recomputed position differs. When FALSE such rows are reported in
#'   `moved` but left untouched.
#' @param dry_run (Default = FALSE) Compute and return the report without
#'   writing anything.
#' @param commit (Default = TRUE) Commit the writes.
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#'
#' @returns A data frame, one row per processed review assignment:
#'   `review_assignment_id`, `n_text` (rows considered), `filled`
#'   (`NA` -> located), `moved` (located -> different span), `still_na`
#'   (`NA` before and still unplaceable), `written` (rows actually updated).
#' @export
dbRelocateCompText <- function(
  conn,
  review_assignment_ids = NULL,
  apply_moves = FALSE,
  dry_run = FALSE,
  commit = TRUE
) {
  if (is.null(review_assignment_ids)) {
    review_assignment_ids <- tbl(conn, "competency_text") |>
      filter(is.na(start)) |>
      inner_join(
        tbl(conn, "competency_score") |> select(competency_score_id = id, review_assignment_id),
        by = "competency_score_id"
      ) |>
      inner_join(
        tbl(conn, "review_assignment") |>
          filter(reviewer_id == 1L) |>
          select(review_assignment_id = id),
        by = "review_assignment_id"
      ) |>
      distinct(review_assignment_id) |>
      pull(review_assignment_id)
  }
  review_assignment_ids <- sort(unique(as.integer(review_assignment_ids)))

  report <- vector("list", length(review_assignment_ids))

  for (k in seq_along(review_assignment_ids)) {
    ra_id <- review_assignment_ids[k]

    rows <- tbl(conn, "competency_text") |>
      inner_join(
        tbl(conn, "competency_score") |>
          filter(review_assignment_id == local(ra_id)) |>
          select(competency_score_id = id),
        by = "competency_score_id"
      ) |>
      select(id, text_match, start, end) |>
      collect() |>
      arrange(id)

    if (nrow(rows) == 0) {
      report[[k]] <- data.frame(
        review_assignment_id = ra_id, n_text = 0L,
        filled = 0L, moved = 0L, still_na = 0L, written = 0L
      )
      next
    }

    plainText <- db_locate_text(conn, ra_id)
    pos <- mod_highlight_locate(plainText, rows$text_match)

    was_na <- is.na(rows$start)
    now_na <- is.na(pos$start)
    same_pos <- !was_na & !now_na & rows$start == pos$start & rows$end == pos$end

    filled_idx <- which(was_na & !now_na)
    moved_idx <- which(!was_na & !now_na & !same_pos)
    still_na_idx <- which(was_na & now_na)

    write_idx <- filled_idx
    if (apply_moves) {
      write_idx <- sort(c(write_idx, moved_idx))
    }

    report[[k]] <- data.frame(
      review_assignment_id = ra_id,
      n_text = nrow(rows),
      filled = length(filled_idx),
      moved = length(moved_idx),
      still_na = length(still_na_idx),
      written = if (dry_run) 0L else length(write_idx)
    )

    if (!dry_run && length(write_idx) > 0) {
      tbl_update(
        data.frame(
          id = rows$id[write_idx],
          start = pos$start[write_idx],
          end = pos$end[write_idx]
        ),
        conn,
        "competency_text",
        commit = commit
      )
    }
  }

  bind_rows(report)
}

#' Check a review's extracted competencies for rule-2 ("one competency per
#' quote") violations
#'
#' Looks at the already-inserted competency_score/competency_text rows for
#' one review_assignment_id (as written by dbCompExtraction()) and flags
#' cases where the same underlying text ended up assigned to more than one
#' competency. Three shapes are detected:
#'   - "overlap": two quotes from different competencies that both have a
#'     located character range and those ranges overlap.
#'   - "unlocated_duplicate": the same verbatim text_match string under two
#'     competencies where at least one copy could not be located (see
#'     mod_highlight_locate()), leaving it with a NULL start/end.
#'   - "substring_duplicate": one competency's quote is a verbatim substring
#'     of another competency's quote (not byte-identical - that's the case
#'     above) and at least one of the pair is unlocated. This is the common
#'     shape mod_highlight_locate() hides: the model quoted a sentence for
#'     one competency and a longer passage containing it for another, so the
#'     nested copy got start/end NA because the containing copy already
#'     claimed that span - invisible to both checks above.
#'
#' Paraphrased duplicates that share neither a character range nor a
#' verbatim substring are still out of scope (would need a similarity
#' threshold calibrated against real examples).
#'
#' @param conn NARRATE database connection
#' @param review_assignment_id Review assignment ID to check
#'
#' @import dplyr
#'
#' @returns A list with has_conflicts (T/F) and conflicts, a data frame (one
#' row per conflicting pair) with columns conflict_type ("overlap",
#' "unlocated_duplicate" or "substring_duplicate"), competency_score_id_1/2,
#' competency_text_id_1/2 (the specific competency_text row on each side, for
#' a resolution step to act on), competency_id_1/2, text_1/2 and start_1/2,
#' end_1/2 (NA where mod_highlight_locate() couldn't place that occurrence)
#' @export
dbCompExtractionCheckConflicts <- function(conn, review_assignment_id) {
  ra_id <- review_assignment_id

  texts <- tbl(conn, "competency_score") |>
    filter(review_assignment_id == local(ra_id)) |>
    select(competency_score_id = id, competency_id) |>
    inner_join(
      tbl(conn, "competency_text") |>
        select(competency_text_id = id, competency_score_id, text_match, start, end),
      by = "competency_score_id"
    ) |>
    collect()

  empty_conflicts <- data.frame(
    conflict_type = character(0),
    competency_score_id_1 = integer(0), competency_text_id_1 = integer(0),
    competency_id_1 = integer(0), text_1 = character(0),
    start_1 = integer(0), end_1 = integer(0),
    competency_score_id_2 = integer(0), competency_text_id_2 = integer(0),
    competency_id_2 = integer(0), text_2 = character(0),
    start_2 = integer(0), end_2 = integer(0),
    stringsAsFactors = FALSE
  )

  if (nrow(texts) < 2) {
    return(list(has_conflicts = FALSE, conflicts = empty_conflicts))
  }

  conflicts <- list()
  addConflict <- function(type, i, j) {
    conflicts[[length(conflicts) + 1]] <<- data.frame(
      conflict_type = type,
      competency_score_id_1 = texts$competency_score_id[i],
      competency_text_id_1 = texts$competency_text_id[i],
      competency_id_1 = texts$competency_id[i],
      text_1 = texts$text_match[i],
      start_1 = texts$start[i],
      end_1 = texts$end[i],
      competency_score_id_2 = texts$competency_score_id[j],
      competency_text_id_2 = texts$competency_text_id[j],
      competency_id_2 = texts$competency_id[j],
      text_2 = texts$text_match[j],
      start_2 = texts$start[j],
      end_2 = texts$end[j],
      stringsAsFactors = FALSE
    )
  }

  # (a) Range-overlap: quotes from different competencies claiming
  # overlapping [start, end) character ranges
  texts$orig_idx <- seq_len(nrow(texts))
  located <- texts[!is.na(texts$start) & !is.na(texts$end), ]
  located <- located[order(located$start), ]

  if (nrow(located) > 1) {
    for (i in seq_len(nrow(located) - 1)) {
      for (j in seq(i + 1, nrow(located))) {
        # Sorted by start - once a later quote starts after this one ends,
        # no further quote can overlap it either
        if (located$start[j] >= located$end[i]) break
        if (located$competency_id[i] != located$competency_id[j]) {
          addConflict("overlap", located$orig_idx[i], located$orig_idx[j])
        }
      }
    }
  }

  # (b) Unlocated duplicates: identical (normalized) text_match under
  # different competencies where at least one copy has no position, because
  # mod_highlight_locate() could only claim one occurrence of it. Compared
  # pairwise (i < j) rather than only starting from unlocated rows, since a
  # located copy (which claimed the one available occurrence first) never
  # gets its own turn as the starting point otherwise.
  norm <- tolower(trimws(gsub("\\s+", " ", texts$text_match)))
  n <- nrow(texts)

  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      if (texts$competency_id[i] == texts$competency_id[j]) next
      if (norm[i] != norm[j]) next
      if (is.na(texts$start[i]) || is.na(texts$start[j])) {
        addConflict("unlocated_duplicate", i, j)
      }
    }
  }

  # (c) Substring duplicates: one quote is a verbatim substring of another
  # under a different competency (byte-identical pairs are already covered
  # by (b)), and at least one side is unlocated - meaning
  # mod_highlight_locate() dropped the nested copy because the containing
  # copy claimed that span. Neither (a) (needs both located) nor (b) (needs
  # identical text) sees this. A minimum shared length avoids flagging
  # incidental short phrases ("team player") that nest by coincidence.
  MIN_SUBSTRING_CHARS <- 20L

  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      if (texts$competency_id[i] == texts$competency_id[j]) next
      if (norm[i] == norm[j]) next # (b)
      shorter <- if (nchar(norm[i]) <= nchar(norm[j])) norm[i] else norm[j]
      longer <- if (nchar(norm[i]) <= nchar(norm[j])) norm[j] else norm[i]
      if (nchar(shorter) < MIN_SUBSTRING_CHARS) next
      if (!grepl(shorter, longer, fixed = TRUE)) next
      if (anyNA(c(
        texts$start[i], texts$end[i], texts$start[j], texts$end[j]
      ))) {
        addConflict("substring_duplicate", i, j)
      }
    }
  }

  conflicts <- if (length(conflicts) > 0) bind_rows(conflicts) else empty_conflicts
  list(has_conflicts = nrow(conflicts) > 0, conflicts = conflicts)
}

# Minimum length (characters, after edge cleanup) for a trimmed loser
# fragment to be worth keeping as its own competency_text row.
CONFLICT_MIN_KEEP_CHARS <- 15L

# Given a located loser quote [l_start, l_end) with text l_text, and the
# winning quote (span w_start/w_end and/or verbatim w_text), return a data
# frame of the loser's surviving fragments (columns text, start, end) after
# removing the region that overlaps the winner. Returns an empty data frame
# when nothing substantial remains, or NULL when the loser can't be trimmed
# (not located, or no identifiable overlap region) and should be deleted
# whole.
conflict_trim_loser <- function(l_text, l_start, l_end, w_text, w_start, w_end) {
  if (is.na(l_start) || is.na(l_end)) return(NULL)
  L <- l_end - l_start
  if (is.na(l_text) || nchar(l_text) != L) return(NULL)

  rel_lo <- NA_integer_
  rel_hi <- NA_integer_

  if (!is.na(w_start) && !is.na(w_end) && w_start < l_end && w_end > l_start) {
    rel_lo <- max(0L, as.integer(w_start - l_start))
    rel_hi <- min(L, as.integer(w_end - l_start))
  } else if (!is.na(w_text) && nzchar(w_text)) {
    p <- regexpr(w_text, l_text, fixed = TRUE)
    if (p[1] != -1) {
      rel_lo <- p[1] - 1L
      rel_hi <- rel_lo + attr(p, "match.length")
    }
  }

  if (is.na(rel_lo) || rel_hi <= rel_lo) return(NULL)
  if (rel_lo <= 0 && rel_hi >= L) {
    return(data.frame(
      text = character(0), start = integer(0), end = integer(0)
    ))
  }

  spans <- list()
  if (rel_lo > 0) spans <- c(spans, list(c(0L, rel_lo)))
  if (rel_hi < L) spans <- c(spans, list(c(rel_hi, L)))

  # Stray separators left dangling by the cut - whitespace and sentence
  # punctuation only, so bracketed tokens like "[name_redact]" survive
  edge <- "[[:space:],;:.!?-]*"
  frags <- lapply(spans, function(s) {
    piece <- substr(l_text, s[1] + 1L, s[2])
    lead <- attr(regexpr(paste0("^", edge), piece), "match.length")
    piece <- substr(piece, lead + 1L, nchar(piece))
    trail <- attr(regexpr(paste0(edge, "$"), piece), "match.length")
    piece <- substr(piece, 1L, nchar(piece) - trail)
    if (nchar(piece) < CONFLICT_MIN_KEEP_CHARS) return(NULL)
    new_start <- l_start + s[1] + lead
    data.frame(
      text = piece, start = as.integer(new_start),
      end = as.integer(new_start + nchar(piece)),
      stringsAsFactors = FALSE
    )
  })
  frags <- frags[!vapply(frags, is.null, logical(1))]
  if (length(frags) == 0) {
    return(data.frame(
      text = character(0), start = integer(0), end = integer(0)
    ))
  }
  do.call(rbind, frags)
}

#' Apply resolve-prompt decisions to conflicting competency_text rows
#'
#' Takes the cluster/option mapping from build_resolve_conflicts() and the
#' model's parsed resolutions (llm_comp_resolve()$data) and applies them: for
#' each conflictId the winning option keeps the overlapping evidence and the
#' losing option(s) give it up (cIndex 0 discards it from all of them). A
#' competency_score row left with no competency_text children afterward is
#' deleted too, since a competency with no supporting evidence shouldn't be
#' scored.
#'
#' Unlike dbCompExtraction(), this only ever touches the specific
#' competency_text rows identified by the conflict - it never touches quotes
#' that weren't part of one.
#'
#' A losing quote is not always deleted whole: when it only partially
#' overlaps the winning quote (or contains it), just the overlapping span is
#' removed and the loser keeps its non-overlapping remainder(s) under its
#' original competency - so a long quote that shares only a clause with the
#' winner still contributes its other clause (a quote split in two by an
#' interior overlap becomes two rows). The row is deleted only when nothing
#' substantial is left (every remainder shorter than CONFLICT_MIN_KEEP_CHARS
#' after trimming, or the loser was never located).
#'
#' @param conn NARRATE database connection
#' @param clusters Cluster/option data frame from build_resolve_conflicts()
#'   (columns conflictId, competency_text_id, competency_score_id,
#'   competency_id, comp_order, text, start, end)
#' @param resolutions List as returned by llm_comp_resolve()$data - each
#'   element has conflictId and cIndex (0 = discard)
#' @param commit (Default = TRUE)
#'
#' @import dplyr
#' @importFrom sqlife tbl_delete tbl_update tbl_insert
#'
#' @returns A list with resolved (integer vector of conflictIds successfully
#'   applied) and unresolved (integer vector of conflictIds the model didn't
#'   answer, or answered with a cIndex not among that conflict's options -
#'   left untouched for a future retry)
#' @export
dbCompConflictResolve <- function(conn, clusters, resolutions, commit = TRUE) {
  resolved <- integer(0)
  unresolved <- integer(0)
  to_delete_text <- integer(0)
  affected_scores <- integer(0)
  to_reposition <- list()
  to_trim <- list()
  to_insert_text <- list()

  all_conflict_ids <- unique(clusters$conflictId)
  chosen <- if (length(resolutions) > 0) {
    setNames(
      sapply(resolutions, "[[", "cIndex"),
      sapply(resolutions, "[[", "conflictId")
    )
  } else {
    setNames(numeric(0), character(0))
  }

  for (cid in all_conflict_ids) {
    opts <- clusters[clusters$conflictId == cid, ]
    cIndex <- chosen[as.character(cid)]

    if (is.na(cIndex)) {
      unresolved <- c(unresolved, cid) # model didn't answer this one
      next
    }

    if (cIndex == 0) {
      # Discard entirely - none of the options is correct
      to_delete_text <- c(to_delete_text, opts$competency_text_id)
      affected_scores <- c(affected_scores, opts$competency_score_id)
      resolved <- c(resolved, cid)
      next
    }

    winner <- opts[opts$comp_order == cIndex, ]
    if (nrow(winner) == 0) {
      unresolved <- c(unresolved, cid) # model picked an option not offered
      next
    }

    losers <- opts[opts$comp_order != cIndex, ]

    win_start <- winner$start[1]
    win_end <- winner$end[1]
    win_text <- winner$text[1]

    # The winning option may be the copy mod_highlight_locate() couldn't
    # place (start/end NA) while a losing option for the same underlying
    # quote does have a real position. Derive the winner's real span from a
    # positioned loser whose text is nested with the winner's (either
    # direction - the winner's quote inside the loser's, or vice versa), so
    # the span always matches nchar(win_text). If none of the positioned
    # losers is nested with the winner, leave the winner NA rather than
    # writing a span that doesn't fit its text.
    if (is.na(win_start) && !is.na(win_text)) {
      positioned <- losers[
        !is.na(losers$start) & !is.na(losers$end) &
          nchar(losers$text) == (losers$end - losers$start), ,
        drop = FALSE
      ]
      for (k in seq_len(nrow(positioned))) {
        pl <- positioned[k, ]
        p <- regexpr(win_text, pl$text, fixed = TRUE) # winner inside loser
        q <- regexpr(pl$text, win_text, fixed = TRUE) # loser inside winner
        if (p[1] != -1) {
          win_start <- as.integer(pl$start + (p[1] - 1L))
        } else if (q[1] != -1) {
          win_start <- as.integer(pl$start - (q[1] - 1L))
        } else {
          next
        }
        if (win_start < 0) { win_start <- NA_integer_; next }
        win_end <- as.integer(win_start + nchar(win_text))
        to_reposition[[length(to_reposition) + 1]] <- data.frame(
          id = winner$competency_text_id[1],
          start = win_start,
          end = win_end
        )
        break
      }
    }

    for (li in seq_len(nrow(losers))) {
      lr <- losers[li, ]
      frags <- conflict_trim_loser(
        lr$text, lr$start, lr$end, win_text, win_start, win_end
      )

      if (is.null(frags) || nrow(frags) == 0) {
        # Nothing worth keeping - remove the whole row
        to_delete_text <- c(to_delete_text, lr$competency_text_id)
        affected_scores <- c(affected_scores, lr$competency_score_id)
        next
      }

      # First fragment updates the existing row; any further fragment
      # (a quote split in two by an interior overlap) becomes a new row
      to_trim[[length(to_trim) + 1]] <- data.frame(
        id = lr$competency_text_id,
        text_match = frags$text[1],
        start = frags$start[1],
        end = frags$end[1]
      )
      if (nrow(frags) > 1) {
        to_insert_text[[length(to_insert_text) + 1]] <- data.frame(
          competency_score_id = lr$competency_score_id,
          text_match = frags$text[-1],
          start = frags$start[-1],
          end = frags$end[-1]
        )
      }
    }

    resolved <- c(resolved, cid)
  }

  if (length(to_reposition) > 0) {
    tbl_update(bind_rows(to_reposition), conn, "competency_text", commit = commit)
  }

  if (length(to_trim) > 0) {
    tbl_update(bind_rows(to_trim), conn, "competency_text", commit = commit)
  }

  if (length(to_insert_text) > 0) {
    tbl_insert(bind_rows(to_insert_text), conn, "competency_text", commit = commit)
  }

  if (length(to_delete_text) > 0) {
    tbl_delete(
      data.frame(id = unique(to_delete_text)),
      conn, "competency_text",
      commit = commit
    )
  }

  # Clean up competency_score rows left with no competency_text children -
  # a competency with no remaining evidence shouldn't be scored
  if (length(affected_scores) > 0) {
    orphaned <- tbl(conn, "competency_score") |>
      filter(id %in% local(unique(affected_scores))) |>
      anti_join(
        tbl(conn, "competency_text") |> select(id = competency_score_id),
        by = "id"
      ) |>
      select(id) |>
      collect()

    if (nrow(orphaned) > 0) {
      tbl_delete(orphaned, conn, "competency_score", commit = commit)
    }
  }

  list(resolved = resolved, unresolved = unresolved)
}

#' Internal: locate a re-anchor span, tolerating a differing leading subject
#'
#' The re-anchor model reliably copies the body of a span but often keeps the
#' paraphrase's subject word instead of the evaluation's - `She` for `she`,
#' `[name_redact]` for `He`, and vice versa. When an exact (whitespace-
#' tolerant) `mod_highlight_locate()` fails, this strips the anchor's first
#' token, re-locates the remainder, and walks the match start left over
#' whatever subject token actually precedes it in `plainText`.
#'
#' @param plainText Tag-stripped evaluation text (`db_locate_text()`)
#' @param anchor The model's returned span
#'
#' @returns Length-2 integer vector `c(start, end)` (0-indexed, half-open) or
#'   `NULL` if the span (or its subject-stripped remainder) isn't present.
reanchor_locate <- function(plainText, anchor) {
  p <- mod_highlight_locate(plainText, anchor)
  if (!is.na(p$start[1])) return(c(p$start[1], p$end[1]))

  rest <- sub(
    "^\\s*(\\[[A-Za-z]*_redact\\]|[A-Za-z]+)[[:space:],]+", "", anchor,
    perl = TRUE
  )
  if (identical(rest, anchor) || nchar(rest) < 15L) return(NULL)

  p2 <- mod_highlight_locate(plainText, rest)
  if (is.na(p2$start[1])) return(NULL)

  s <- p2$start[1]
  left <- substr(plainText, 1L, s)
  m <- regexpr("(\\[[A-Za-z]*_redact\\]|[A-Za-z]+)[[:space:],]+$", left, perl = TRUE)
  if (m > 0) s <- as.integer(m) - 1L
  c(s, p2$end[1])
}

#' Apply LLM re-anchor answers to a review's unplaced competency evidence
#'
#' Phase 2 of the paraphrased-quote fix (dev/paraphrase_fix_plan.md). Given
#' the model's answer - one verbatim span, or `null`, per unplaced
#' `competency_text` row - this, per target row:
#'   - locates the returned span in the review's locate text
#'     (`db_locate_text()`, whitespace-tolerant via `mod_highlight_locate()`).
#'     The answered anchors are located together in `itemId` order so two
#'     paraphrases of the same quote don't claim the same span; already-located
#'     rows are *not* seeded as prior claims and are never touched;
#'   - on a hit: rewrites `text_match` to the anchor, sets `start`/`end`,
#'     clears `locate_status`;
#'   - on a miss (no answer, or the span isn't in the text): sets
#'     `locate_status = 'unlocated'` and leaves the row unplaced for a human.
#'
#' Overlap with an existing quote from another competency is *not* prevented
#' here - that is a legitimate rule-2 conflict, and `llm_comp_reanchor_run()`
#' re-checks `dbCompExtractionCheckConflicts()` after applying and routes the
#' review to the resolve step (statusCode 6) when one results.
#'
#' @param conn NARRATE database connection
#' @param review_assignment_id Review assignment ID
#' @param items Data frame mapping `itemId` -> `competency_text_id` (the
#'   `items` list-column entry from `db_fetch_review_reanchor()`)
#' @param anchors Parsed model answer (`llm_comp_reanchor()$data`): a list of
#'   entries each with `itemId` and `anchor` (a string or `NULL`)
#' @param commit (Default = TRUE)
#'
#' @import dplyr
#' @importFrom sqlife tbl_update
#'
#' @returns A list with `summary` (named integer: `n_reanchored`,
#'   `n_flagged`, `n_unanswered`) and `rows` (data frame: `competency_text_id`,
#'   `itemId`, `outcome`, `start`, `end`).
#' @export
dbCompReanchorApply <- function(
  conn,
  review_assignment_id,
  items,
  anchors,
  commit = TRUE
) {
  ra_id <- review_assignment_id

  # itemId -> anchor string (NA for null / missing / blank)
  ans <- setNames(
    lapply(anchors, function(a) a$anchor),
    vapply(anchors, function(a) as.character(a$itemId), character(1))
  )
  anchor_for <- function(itemId) {
    v <- ans[[as.character(itemId)]]
    if (is.null(v) || length(v) != 1 || is.na(v) || !nzchar(trimws(v))) {
      return(NA_character_)
    }
    as.character(v)
  }

  plainText <- db_locate_text(conn, ra_id)

  tgt <- data.frame(
    itemId = items$itemId,
    ct_id = items$competency_text_id,
    stringsAsFactors = FALSE
  )
  tgt$anchor <- vapply(tgt$itemId, anchor_for, character(1))
  tgt$answered <- !is.na(tgt$anchor)

  # Each answered anchor is located independently (first verbatim occurrence,
  # whitespace- and leading-subject-tolerant via reanchor_locate()). Already-
  # located rows are NOT seeded as prior claims: a re-anchored quote whose
  # span nests (or is nested by) another competency's existing quote is a
  # legitimate rule-2 conflict, and llm_comp_reanchor_run() re-checks
  # dbCompExtractionCheckConflicts() afterwards and routes such reviews to the
  # resolve step (statusCode 6).
  tgt$new_start <- NA_integer_
  tgt$new_end <- NA_integer_
  tgt$new_text <- NA_character_
  for (i in which(tgt$answered)) {
    hit <- reanchor_locate(plainText, tgt$anchor[i])
    if (!is.null(hit)) {
      tgt$new_start[i] <- hit[1]
      tgt$new_end[i] <- hit[2]
      tgt$new_text[i] <- substr(plainText, hit[1] + 1L, hit[2])
    }
  }

  updates <- list()
  flags <- integer(0)
  out_rows <- vector("list", nrow(tgt))

  for (i in seq_len(nrow(tgt))) {
    ct_id <- tgt$ct_id[i]
    win <- tgt$answered[i] && !is.na(tgt$new_start[i])

    if (win) {
      updates[[length(updates) + 1]] <- data.frame(
        id = ct_id, text_match = tgt$new_text[i],
        start = tgt$new_start[i], end = tgt$new_end[i],
        locate_status = NA_character_
      )
      outcome <- "reanchored"
    } else {
      flags <- c(flags, ct_id)
      outcome <- if (!tgt$answered[i]) "unanswered" else "not_verbatim"
    }

    out_rows[[i]] <- data.frame(
      competency_text_id = ct_id,
      itemId = tgt$itemId[i],
      outcome = outcome,
      start = if (win) tgt$new_start[i] else NA_integer_,
      end = if (win) tgt$new_end[i] else NA_integer_
    )
  }

  if (length(updates) > 0) {
    tbl_update(
      bind_rows(updates), conn, "competency_text",
      returnData = FALSE, commit = FALSE
    )
  }
  if (length(flags) > 0) {
    tbl_update(
      data.frame(id = unique(flags), locate_status = "unlocated"),
      conn, "competency_text",
      returnData = FALSE, commit = FALSE
    )
  }
  if (commit) dbCommit(conn)

  out_rows <- bind_rows(out_rows)
  list(
    summary = c(
      n_reanchored = sum(out_rows$outcome == "reanchored"),
      n_flagged = length(unique(flags)),
      n_unanswered = sum(out_rows$outcome == "unanswered")
    ),
    rows = out_rows
  )
}

#' Internal: filter a collected data frame to rows whose id is not already
#' present in a target table
#'
#' @param data Data frame with an id column, already collected from the
#'   source database
#' @param conn Target database connection
#' @param table Name of the target table to check for existing ids
#'
#' @import dplyr
dbRowsMissing <- function(data, conn, table) {
  if (nrow(data) == 0) {
    return(data)
  }
  existing <- tbl(conn, table) |>
    filter(id %in% local(data$id)) |>
    pull(id)
  filter(data, !id %in% existing)
}

#' Merge completed reviews from a local database into a target database
#'
#' For a given set of \code{review_assignment} IDs in a local NARRATE
#' database, copies the "completed" ones into a target NARRATE database,
#' along with everything new they depend on (reviewer, rubric + its prompt /
#' composition rows, batch provenance). IDs are preserved as-is: both
#' databases are expected to share the same ID space (i.e. \code{targetDbPath}
#' is an ancestor/descendant of \code{localDbPath} via a prior pin sync), so
#' no re-keying is attempted. Any row that already exists in the target (by
#' id) is left untouched.
#'
#' Reference/seed tables (evaluation, competency, specificity, utility,
#' sentiment, rule, student, clerkship, rotation, answer, question) are
#' assumed already identical between the two databases and are not copied.
#'
#' @param localDbPath Path to the local NARRATE database containing the new review results
#' @param targetDbPath Path to the target NARRATE database to merge into (modified in place)
#' @param review_ids Integer vector of review_assignment IDs (from the local db) to merge
#' @param statusCodes review_assignment statusCode values considered "completed"
#'   (default \code{c(2, -1, 5)}: completed, completed with flag, batch scoring complete)
#' @param show_warnings (Default = TRUE) Emit a \code{warning()} for review_ids
#'   that are skipped because they don't exist locally, aren't completed, or
#'   already exist in the target
#'
#' @import dplyr
#' @importFrom sqlife dbGetConn dbFinish tbl_insert
#' @importFrom stats na.omit
#'
#' @returns A list with \code{inserted} (named integer vector of rows
#'   inserted per table, zero-count tables omitted), \code{skipped}
#'   (review_ids that already existed in the target), and \code{excluded}
#'   (a list with \code{not_found} and \code{not_completed} review_ids)
#' @export
dbMergeReviews <- function(
  localDbPath,
  targetDbPath,
  review_ids,
  statusCodes = c(2, -1, 5),
  show_warnings = TRUE
) {
  localConn <- dbGetConn(localDbPath)
  targetConn <- dbGetConn(targetDbPath)

  ra <- tbl(localConn, "review_assignment") |>
    filter(id %in% local(review_ids)) |>
    collect()

  not_found <- setdiff(review_ids, ra$id)
  if (show_warnings && length(not_found) > 0) {
    warning(
      length(not_found), " review_id(s) not found in the local database: ",
      paste(not_found, collapse = ", ")
    )
  }

  not_completed <- ra$id[!ra$statusCode %in% statusCodes]
  if (show_warnings && length(not_completed) > 0) {
    warning(
      length(not_completed), " review_id(s) skipped (not completed): ",
      paste(not_completed, collapse = ", ")
    )
  }
  ra <- filter(ra, statusCode %in% statusCodes)

  skipped <- tbl(targetConn, "review_assignment") |>
    filter(id %in% local(ra$id)) |>
    pull(id)
  if (show_warnings && length(skipped) > 0) {
    warning(
      length(skipped), " review_id(s) skipped (already present in target): ",
      paste(skipped, collapse = ", ")
    )
  }
  ra <- filter(ra, !id %in% skipped)

  inserted <- c(
    prompt = 0L,
    rubric = 0L,
    rubric_competency = 0L,
    rubric_specificity = 0L,
    rubric_utility = 0L,
    rubric_sentiment = 0L,
    rubric_rule = 0L,
    reviewer = 0L,
    batch = 0L,
    review_assignment = 0L,
    competency_score = 0L,
    competency_text = 0L,
    batch_review = 0L
  )

  if (nrow(ra) > 0) {
    # --- reviewer
    reviewer <- tbl(localConn, "reviewer") |>
      filter(id %in% local(unique(ra$reviewer_id))) |>
      collect() |>
      dbRowsMissing(targetConn, "reviewer")
    if (nrow(reviewer) > 0) {
      tbl_insert(reviewer, targetConn, "reviewer", commit = F)
      inserted["reviewer"] <- nrow(reviewer)
    }

    # --- rubric (+ prompt + composition, only for rubrics new to target)
    rubric <- tbl(localConn, "rubric") |>
      filter(id %in% local(unique(ra$rubric_id))) |>
      collect() |>
      dbRowsMissing(targetConn, "rubric")

    if (nrow(rubric) > 0) {
      prompt_ids <- unique(na.omit(c(
        rubric$prompt_extract_id,
        rubric$prompt_score_id
      )))
      prompt <- tbl(localConn, "prompt") |>
        filter(id %in% local(prompt_ids)) |>
        collect() |>
        dbRowsMissing(targetConn, "prompt")
      if (nrow(prompt) > 0) {
        tbl_insert(prompt, targetConn, "prompt", commit = F)
        inserted["prompt"] <- inserted["prompt"] + nrow(prompt)
      }

      tbl_insert(rubric, targetConn, "rubric", commit = F)
      inserted["rubric"] <- nrow(rubric)

      for (rubricTbl in c(
        "rubric_competency",
        "rubric_specificity",
        "rubric_utility",
        "rubric_sentiment",
        "rubric_rule"
      )) {
        rows <- tbl(localConn, rubricTbl) |>
          filter(rubric_id %in% local(rubric$id)) |>
          collect() |>
          dbRowsMissing(targetConn, rubricTbl)
        if (nrow(rows) > 0) {
          tbl_insert(rows, targetConn, rubricTbl, commit = F)
          inserted[rubricTbl] <- nrow(rows)
        }
      }
    }

    # --- review_assignment
    tbl_insert(ra, targetConn, "review_assignment", commit = F)
    inserted["review_assignment"] <- nrow(ra)

    # --- competency_score / competency_text
    cs <- tbl(localConn, "competency_score") |>
      filter(review_assignment_id %in% local(ra$id)) |>
      collect()
    if (nrow(cs) > 0) {
      tbl_insert(cs, targetConn, "competency_score", commit = F)
      inserted["competency_score"] <- nrow(cs)

      ct <- tbl(localConn, "competency_text") |>
        filter(competency_score_id %in% local(cs$id)) |>
        collect()
      if (nrow(ct) > 0) {
        tbl_insert(ct, targetConn, "competency_text", commit = F)
        inserted["competency_text"] <- nrow(ct)
      }
    }

    # --- batch / batch_review provenance
    br <- tbl(localConn, "batch_review") |>
      filter(review_assignment_id %in% local(ra$id)) |>
      collect()
    if (nrow(br) > 0) {
      batch <- tbl(localConn, "batch") |>
        filter(id %in% local(unique(br$batch_id))) |>
        collect() |>
        dbRowsMissing(targetConn, "batch")

      if (nrow(batch) > 0) {
        batchPromptIds <- unique(na.omit(batch$prompt_id))
        batchPrompt <- tbl(localConn, "prompt") |>
          filter(id %in% local(batchPromptIds)) |>
          collect() |>
          dbRowsMissing(targetConn, "prompt")
        if (nrow(batchPrompt) > 0) {
          tbl_insert(batchPrompt, targetConn, "prompt", commit = F)
          inserted["prompt"] <- inserted["prompt"] + nrow(batchPrompt)
        }

        tbl_insert(batch, targetConn, "batch", commit = F)
        inserted["batch"] <- nrow(batch)
      }

      tbl_insert(br, targetConn, "batch_review", commit = F)
      inserted["batch_review"] <- nrow(br)
    }
  }

  result <- list(
    inserted = inserted[inserted > 0],
    skipped = skipped,
    excluded = list(not_found = not_found, not_completed = not_completed)
  )

  dbFinish(targetConn, commit = TRUE)
  dbFinish(localConn, commit = FALSE)

  result
}

#' Add core faculty start dates and update core faculty status in evaluations
#'
#' @param file Path to a CSV file with columns original_evaluator_id and core_faculty_start
#' @param conn A database connection (schema inst/narrate.sql)
#'
#' @import dplyr
#' @import RSQLite
#' @importFrom sqlife tbl_update
#' @importFrom utils read.csv
#'
#' @returns TRUE invisibly on success
#' @export
#'
dbAddCoreFaculty <- function(file, conn) {
  csv <- read.csv(file, stringsAsFactors = FALSE)

  # --- Step 1: update core_faculty_start in evaluator
  evaluators <- tbl(conn, "evaluator") |>
    filter(original_evaluator_id %in% local(csv$original_evaluator_id)) |>
    collect()

  to_update <- evaluators |>
    inner_join(
      csv |> select(original_evaluator_id, core_faculty_start),
      by = "original_evaluator_id"
    ) |>
    mutate(core_faculty_start = core_faculty_start.y) |>
    select(id, core_faculty_start)

  if (nrow(to_update) > 0) {
    tbl_update(to_update, conn, "evaluator", commit = FALSE)
  }

  # --- Step 2: update core_faculty in evaluation
  # Re-read evaluators so we use the freshly updated core_faculty_start values
  all_evaluators <- tbl(conn, "evaluator") |>
    filter(!is.na(core_faculty_start)) |>
    collect()

  evaluations <- tbl(conn, "evaluation") |>
    inner_join(tbl(conn, "rotation"), by = c("rotation_id" = "id")) |>
    collect() |>
    left_join(
      all_evaluators |> select(id, core_faculty_start),
      by = c("evaluator_id" = "id")
    ) |>
    mutate(
      core_faculty = as.integer(
        !is.na(core_faculty_start) & rotation_date >= core_faculty_start
      )
    ) |>
    select(id, core_faculty)

  if (nrow(evaluations) > 0) {
    tbl_update(evaluations, conn, "evaluation", commit = FALSE)
  }

  dbCommit(conn)
  invisible(TRUE)
}

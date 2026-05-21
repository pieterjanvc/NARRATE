#' Parse data from a combined format and insert it into the database
#'
#' @param combined_data Dataframe of the original data
#' @param dbPath Path to a (new) CFME database
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
  schema <- system.file("extdata", "cfme.sql", package = "CFME")

  if (schema == "") {
    schema <- "inst/cfme.sql"
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
    distinct()

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

  #Check if data matches
  if (!all(check == combined_data, na.rm = T)) {
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
#' @param conn CFME database connection
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
#' @param conn CFME database connection
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
      toInsert$note = note
    }

    if (!is.null(task)) {
      toInsert$task = task
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
#' @param conn CFME database connection
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
    select(competency_score_id, text_match)
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
#' @param conn CFME database connection
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
    id <- tbl(conn, "reviewer") |>
      filter(id %in% {{ id }}, human == 1) |>
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
    check <- tbl(conn, "reviewer") |>
      filter(username %in% {{ username }}) |>
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
#' @param conn CFME database connection
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
#' @param conn CFME database connection
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
    data$statusCode = 0
    if (missing(redacted)) {
      data$redacted = T
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
        stop("No rubric found. Run rubric_process() first.")
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
#' @param conn CFME database connection
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
#' competency_text dataframes if return_tables = TRUE
#' @export
dbCompExtraction <- function(
  conn,
  review_assignment_id,
  comp_extraction,
  return_tables = F,
  commit = T
) {
  ra_id <- review_assignment_id

  # Resolve cIndex (order position) → competency_id via rubric_competency
  rubric_id <- tbl(conn, "review_assignment") |>
    filter(id == local(ra_id)) |>
    pull(rubric_id)

  order_map <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(rubric_id)) |>
    select(comp_order = order, competency_id) |>
    collect()

  new_indexes   <- sapply(comp_extraction, "[[", "cIndex")
  new_comp_ids  <- order_map$competency_id[match(new_indexes, order_map$comp_order)]

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

  to_insert_ids <- new_comp_ids[!new_comp_ids %in% existing_scores$competency_id]

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

  # --- competency_text: delete existing and insert new for each extracted item
  all_text_new <- list()

  for (item in comp_extraction) {
    comp_id <- order_map$competency_id[order_map$comp_order == item$cIndex]
    texts   <- item$text

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
          text_match = texts
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

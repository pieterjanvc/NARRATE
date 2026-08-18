# https://rstudio.github.io/bslib/articles/cards/index.html
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(sqlife)


dbInfo <- "../local/narrate.db"
# dbInfo <- "../local/test.db"
# dbInfo <- "~/Downloads/narrate.db"

# This is the db used during deployment, see deployShinyApp()
if (!file.exists(dbInfo)) {
  dbInfo <- "narrate.db"
  library(NARRATE)
  # These are the libraries that the app needs when deployed
} else {
  devtools::load_all()
  Sys.setenv(HMS_AZURE_API = keyring::key_get("HMS_AZURE_API"))
}

tabStatusIcon <- function(name, status, session) {
  removeUI(sprintf("#%sIcon", name), session = session)
  insertUI(
    sprintf("#%sTitle", name),
    "afterBegin",
    ui = icon(
      ifelse(status == 1, "circle-half-stroke", "circle"),
      class = ifelse(status == 0, "fa-regular", "fa-solid"),
      style = "color: #3ebc00;",
      id = paste0(name, "Icon")
    ),
    session = session
  )
}

ui <- page_fluid(
  useShinyjs(),
  theme = bs_theme(preset = "journal"),
  tags$style(HTML(
    "
    .control-label {
      font-weight: bold;
    }
    /* Make sure the dropdowns are not clipped by parent container */
    .html-fill-item {
      overflow: visible !important;
    }
    /* DT selected row colour */
    :root {
    --dt-row-selected: 224, 168, 78;
    }
    /* mod_overlap_ui_filters() has no width= param, so its selects fall
       back to Shiny's default 300px .shiny-input-container width */
    .analysis-overlap-filters .shiny-input-container {
      width: 100%;
      max-width: 100%;
    }
    .tooltip .tooltip-inner {
      max-width: 300px;
    }
  "
  )),
  # Add the DB download link
  div(
    # mod_dbSetup_ui("dbMod", "link"),
    # tags$br(),
    actionLink("refreshDB", "Refresh Database"),
    style = "position:absolute;right:20px;top=0;z-index:9999;"
  ),
  navset_card_tab(
    nav_panel(
      title = "REVIEW",
      # TAB 1 - Layout
      layout_columns(
        card(
          card_header("1. Select Reviewer"),

          selectInput("reviewerID", "Reviewer", choices = c()),
          HTML(
            "<i>You are the <b>reviewer</b> assessing the quality of an evaluation. ",
            "The <b>evaluator</b> is the person who wrote the student evaluation below</i>"
          )
        ),
        card(
          card_header("2. Pick an evaluation"),
          selectInput(
            "reviewID",
            "0 to start - 0 in progress - 0 competed",
            choices = c(),
            width = "100%"
          ),
          # checkboxInput("includeCompeted", "List completed", value = F),
          uiOutput("aiReviewUI")
        )
      ),

      layout_columns(
        card(
          card_header("Student evaluation"),
          div(
            mod_highlight_ui_text("highlights"),
            style = "max-height: 70vh; overflow-y: auto;"
          )
        ),
        navset_card_tab(
          nav_panel(
            title = div(" Competencies", id = "compTitle"),
            mod_highlight_ui_controls(
              "highlights",
              NS("highlights", "textDisplay"),
              "1. Highlight text evidence"
            ),
            radioButtons(
              "cID",
              "2. Pick best fitting competency",
              choices = c("Loading ..."),
              width = "100%"
            ),
            uiOutput("compDescr"),
            radioButtons(
              "specificity",
              "3. Select specificity score",
              choices = c(1:3),
              inline = T
            ),
            textAreaInput(
              "competencyComment",
              "Optional competency comment",
              placeholder = "Not part of the rubric, used internally",
              width = "100%"
            ),
            actionButton("addComp", "Save competency review"),
            value = "compTab"
          ),
          nav_panel(
            title = div(" Overall Scores", id = "overallTitle"),
            radioButtons(
              "util",
              "Utility score",
              choices = c(1:3),
              inline = F,
              width = "100%"
            ),
            radioButtons(
              "sent",
              "Sentiment score",
              choices = c(1:5),
              inline = F,
              width = "100%"
            ),
            textAreaInput(
              "reviewComment",
              "Optional review comment",
              placeholder = "Not part of the rubric, used internally",
              width = "100%"
            ),
            actionButton("addOverall", "Add overall review"),
            id = "overallTab"
          ),
          nav_panel(
            title = div(" Submit", id = "submitTitle"),
            uiOutput("summary"),
            tags$b("OPTIONAL"),
            checkboxInput("flag", "Add issue flag"),
            actionButton("complete", "Mark as complete"),
            id = "submitTab"
          ),
          id = "testTabs"
        )
      )
    ),
    nav_panel(
      "ANALYSIS",
      layout_columns(
        card(
          card_header("Evaluation"),
          div(DTOutput("analysis_review_table")),
          div(uiOutput("analysis_score_summary"), class = "mt-2"),
          div(
            class = "analysis-overlap-filters",
            mod_overlap_ui_filters(
              "analysis_overlap",
              group_label = "Competency",
              user_label = "Reviewer"
            )
          ),
          div(
            mod_overlap_ui_text("analysis_overlap"),
            style = "max-height: 40vh; overflow-y: auto;"
          )
        )
      ),
      uiOutput("textMatches"),
      layout_columns(
        card(
          card_header("Comparison"),
          div(DTOutput("analysis_table"))
        )
      )
    ),
    nav_panel(
      "RUBRIC",
      layout_columns(
        card(
          card_header("Competencies"),
          selectInput(
            "rubricID",
            "Rubric version",
            choices = c(),
            width = "100%"
          ),
          div(DTOutput("rubricCompTable")),
          actionButton("addCompetency", "Add competency")
        ),
        card(
          card_header("Specificity scores"),
          div(DTOutput("rubricSpecTable"))
        ),
        card(
          card_header("Utility scores"),
          div(DTOutput("rubricUtilTable"))
        ),
        card(
          card_header("Sentiment scores"),
          div(DTOutput("rubricSentTable"))
        ),
        card(
          layout_columns(
            actionButton("updateRubric", "Save as new rubric version"),
            actionButton("updateRubricInPlace", "Update existing rubric"),
            col_widths = c(6, 6)
          )
        ),
        col_widths = 12
      )
    ),
    nav_panel(
      "ASSIGNMENT",
      card(
        card_header("Select Evaluation"),
        div(DTOutput("assignment_eval_table")),
        checkboxInput(
          "includeOtherRubric",
          "Include previously reviewed with different rubric",
          value = FALSE,
          width = "auto"
        ),
        actionButton("assignToAll", "Assign to all")
      ),
      card(
        card_header("Student evaluation"),
        uiOutput("assignment_evaluation")
      )
    )
  )
)

server <- function(input, output, session) {
  # DB Setup module (only needed for download)
  . <- mod_dbSetup_server(
    "dbMod",
    localFolder = "../local",
    tempFolder = "../local",
    schema = "../inst/narrate.sql",
    useDB = dbInfo
  )
  # DB Connection
  conn <- dbGetConn(dbInfo, session = session)

  # review_assignment status codes loaded once; named vector + semantic aliases
  ra_codes <- status_codes(conn, "review_assignment") |>
    mutate(code = as.integer(code))
  ra_code_desc <- setNames(ra_codes$description, ra_codes$code)
  ra_new <- ra_codes$code[ra_codes$description == "New"]
  ra_inprogress <- ra_codes$code[ra_codes$description == "In progress"]
  ra_flagged <- ra_codes$code[ra_codes$description == "Completed with flag"]
  ra_completed <- ra_codes$code[ra_codes$description == "Completed"]
  ra_completed_label <- if (length(ra_completed) > 0L) {
    unname(ra_code_desc[as.character(ra_completed[1L])])
  } else {
    "Completed"
  }
  ra_ai_done <- ra_codes$code[ra_codes$description == "Batch scoring complete"]
  ra_batch_ext_done <- ra_codes$code[
    ra_codes$description == "Batch extraction complete"
  ]
  ra_done_codes <- c(ra_flagged, ra_completed, ra_ai_done)
  ra_display_codes <- c(
    ra_flagged,
    ra_new,
    ra_inprogress,
    ra_completed,
    ra_ai_done
  )

  # ── Rubric data ──────────────────────────────────────────────────────────────
  # Load once per session. Competency order comes from rubric_competency.order
  # for the latest rubric (replacing the old competency.cID approach).

  latest_rubric_id <- tbl(conn, "rubric") |>
    summarise(id = max(id, na.rm = TRUE)) |>
    pull(id)

  competencies_db <- tbl(conn, "rubric_competency") |>
    filter(rubric_id == local(latest_rubric_id)) |>
    inner_join(
      tbl(conn, "competency") |>
        select(competency_id = id, name, description, note),
      by = "competency_id"
    ) |>
    arrange(order) |>
    select(competency_id, comp_order = order, name, description, note) |>
    collect()

  # Global id -> name lookups for mod_overlap on the ANALYSIS tab. Built from
  # the full tables (not scoped to the latest rubric) since an evaluation may
  # have been reviewed under an older rubric version, and mod_overlap's
  # group_names/user_names are captured once, non-reactively.
  all_competency_names <- tbl(conn, "competency") |>
    select(id, name) |>
    collect() |>
    (\(x) setNames(x$name, as.character(x$id)))()

  all_reviewer_names <- tbl(conn, "reviewer") |>
    select(id, username) |>
    collect() |>
    mutate(username = ifelse(is.na(username), "AI Model", username)) |>
    (\(x) setNames(x$username, as.character(x$id)))()

  # mod_overlap's user_names is captured once, non-reactively - so an
  # incomplete reviewer (varies per review round) is flagged via a distinct
  # "<id>|incomplete" user_id in overlapHighlights instead, resolved here
  all_reviewer_names_overlap <- c(
    all_reviewer_names,
    setNames(
      paste0(all_reviewer_names, " (incomplete)"),
      paste0(names(all_reviewer_names), "|incomplete")
    )
  )

  specificity_opts <- tbl(conn, "specificity") |>
    collect() |>
    arrange(value)
  utility_opts <- tbl(conn, "utility") |>
    collect() |>
    arrange(value)
  sentiment_opts <- tbl(conn, "sentiment") |>
    collect() |>
    arrange(value)

  # Per-review rubric score options — reloaded whenever the selected review changes.
  # Used in the REVIEW tab; global opts above are kept for the ANALYSIS tab.
  review_rubric_opts <- reactive({
    req(input$reviewID)
    rid <- tbl(conn, "review_assignment") |>
      filter(id == as.integer(input$reviewID)) |>
      pull(rubric_id)
    list(
      specificity = tbl(conn, "rubric_specificity") |>
        filter(rubric_id == local(rid)) |>
        left_join(
          tbl(conn, "specificity") |>
            select(specificity_id = id, value, description, example, note),
          by = "specificity_id"
        ) |>
        select(id = specificity_id, value, description, example, note) |>
        collect() |>
        arrange(value),
      utility = tbl(conn, "rubric_utility") |>
        filter(rubric_id == local(rid)) |>
        left_join(
          tbl(conn, "utility") |>
            select(utility_id = id, value, description, example, note),
          by = "utility_id"
        ) |>
        select(id = utility_id, value, description, example, note) |>
        collect() |>
        arrange(value),
      sentiment = tbl(conn, "rubric_sentiment") |>
        filter(rubric_id == local(rid)) |>
        left_join(
          tbl(conn, "sentiment") |>
            select(sentiment_id = id, value, description, example, note),
          by = "sentiment_id"
        ) |>
        select(id = sentiment_id, value, description, example, note) |>
        collect() |>
        arrange(value)
    )
  })

  # Helper: named vector of integer value → "N - description" label
  score_choices <- function(opts) {
    setNames(as.integer(opts$value), paste(opts$value, "-", opts$description))
  }

  # Helper: radio button label with a hover tooltip showing guiding examples
  examples_label <- function(label_text, opts) {
    ex <- opts[!is.na(opts$example) & nzchar(opts$example), ]
    if (nrow(ex) == 0) {
      return(label_text)
    }
    content <- HTML(paste0(
      "<div style='text-align:left'><strong>Guiding Examples</strong><br>",
      paste(
        paste0("- Score of ", ex$value, ": ", ex$example),
        collapse = "<br>"
      ),
      "</div>"
    ))
    tagList(
      label_text,
      tooltip(
        icon("circle-info", class = "fa-solid ms-1"),
        content,
        placement = "right"
      )
    )
  }

  reviewScores <- reactiveVal()

  # The evaluation text the highlight module renders and matches offsets
  # against - questions are always included so start/end stay meaningful
  evalText <- reactive({
    req(input$reviewID)
    evalID <- tbl(conn, "review_assignment") |>
      filter(id == as.integer(input$reviewID)) |>
      pull(evaluation_id)

    dbGetEvals(
      ids = evalID,
      conn = conn,
      redacted = T,
      includeQuestions = T,
      html = T,
      subtitleTag = "b"
    ) |>
      pull(evaluation)
  })

  # Bumped after a successful competency commit to force the highlight module
  # to fully reseed from `highlightInitVals()` (i.e. from the database),
  # so a just-committed staged highlight is reflected as committed and isn't
  # accidentally re-committed under a different competency later. Kept
  # separate from `forceRefresh()` below, which also fires for unrelated
  # updates (overall scores, AI review) that shouldn't wipe staged highlights.
  highlightRefreshTrigger <- reactiveVal(0)

  # Previously saved highlights (all competencies) for the current review,
  # used to seed the highlight module whenever evalText() changes. Also
  # depends on highlightRefreshTrigger() so a post-commit reseed re-queries
  # the database instead of reusing a stale cached value.
  highlightInitVals <- reactive({
    highlightRefreshTrigger()
    req(input$reviewID)
    reviewID <- as.integer(input$reviewID)

    compScores <- tbl(conn, "competency_score") |>
      filter(review_assignment_id == reviewID) |>
      select(id, competency_id) |>
      collect()

    tbl(conn, "competency_text") |>
      filter(competency_score_id %in% local(compScores$id)) |>
      collect() |>
      left_join(
        compScores |> select(competency_score_id = id, competency_id),
        by = "competency_score_id"
      ) |>
      filter(!is.na(start), !is.na(end)) |>
      transmute(
        group_id = as.character(competency_id),
        start = as.integer(start),
        end = as.integer(end),
        text = text_match
      )
  })

  # Highlight selection module
  txtEvidence <- mod_highlight_server(
    "highlights",
    text = reactive({
      highlightRefreshTrigger()
      evalText()
    }),
    cur_group_id = reactive(input$cID),
    init_vals = highlightInitVals
  )

  # Populate reviewers
  x <- tbl(conn, "reviewer") |>
    # filter(human == 1) |>
    select(id, username) |>
    collect()

  updateSelectInput(session, "reviewerID", choices = setNames(x$id, x$username))

  # Populate review selection dropdown
  updateReviewID <- function(selected) {
    reviews <- tbl(conn, "review_assignment") |>
      filter(reviewer_id == as.integer(input$reviewerID)) |>
      left_join(
        tbl(conn, "evaluation") |>
          select(evaluation_id = id, complete, summary_flg),
        by = "evaluation_id"
      ) |>
      collect() |>
      arrange(
        match(
          statusCode,
          c(
            ra_inprogress,
            ra_new,
            ra_flagged,
            ra_completed,
            ra_ai_done,
            setdiff(
              ra_codes$code,
              c(ra_inprogress, ra_new, ra_flagged, ra_completed, ra_ai_done)
            )
          )
        ),
        desc(modified)
      ) |>
      mutate(
        descr = sprintf(
          "%s (%s %s, rubric %s) - %s",
          evaluation_id,
          ifelse(complete == 1, "complete", "incomplete"),
          ifelse(summary_flg == 1, "summative", "formative"),
          rubric_id,
          case_when(
            statusCode %in% c(ra_completed, ra_ai_done) ~ ra_completed_label,
            TRUE ~ coalesce(ra_code_desc[as.character(statusCode)], "Error")
          )
        )
      )

    lblInfo <- reviews |>
      filter(statusCode %in% ra_display_codes) |>
      group_by(statusCode) |>
      summarise(n = n())

    lblInfo <- data.frame(statusCode = ra_display_codes) |>
      left_join(lblInfo, by = "statusCode") |>
      mutate(n = ifelse(is.na(n), 0, n)) |>
      pull(n)
    # lblInfo order matches ra_display_codes: flagged, new, in-progress, completed, ai-done

    updateSelectInput(
      session,
      "reviewID",
      label = sprintf(
        "%i to start - %i in progress - %i completed",
        lblInfo[2],
        lblInfo[3],
        lblInfo[4] + lblInfo[5] + lblInfo[1]
      ),
      choices = setNames(reviews$id, reviews$descr),
      selected = ifelse(missing(selected), reviews$id[1], selected)
    )
  }

  observeEvent(input$reviewerID, {
    updateReviewID()
  })

  # Add the AI review button when the assigned reviewer is an AI model
  output$aiReviewUI <- renderUI({
    reviewID <- as.integer(input$reviewID)

    check <- tbl(conn, "review_assignment") |>
      filter(id == reviewID, statusCode == ra_new) |>
      inner_join(
        tbl(conn, "reviewer") |>
          select(reviewer_id = id, human) |>
          filter(human == 0),
        by = "reviewer_id"
      ) |>
      collect()

    if (nrow(check) == 0) {
      tagList()
    } else {
      actionButton("aiReview", "Start AI review")
    }
  })

  observeEvent(input$aiReview, {
    showModal(modalDialog(
      title = "AI Review in progress",
      "Please be patient ...",
      div(class = "spinner-border spinner-border-sm"),
      easyClose = F,
      footer = NULL
    ))

    rid <- as.integer(input$reviewID)

    # Step 1: extraction (force = TRUE since statusCode may not be 0 yet)
    extract_res <- llm_comp_extract_run(conn, review_ids = rid, force = TRUE)

    # Step 2: scoring (only if extraction succeeded)
    if (
      !is.null(extract_res) &&
        isTRUE(extract_res$statusCode == ra_batch_ext_done)
    ) {
      llm_comp_score_run(conn, review_ids = rid, force = TRUE)
    }

    final_status <- tbl(conn, "review_assignment") |>
      filter(id == rid) |>
      pull(statusCode)

    removeModal()
    updateReviewID(input$reviewID)
    forceRefresh(forceRefresh() + 1)
    showNotification(
      if (isTRUE(final_status == ra_ai_done)) {
        "AI review complete"
      } else {
        "AI review failed"
      },
      type = if (isTRUE(final_status == ra_ai_done)) "message" else "error"
    )
  })

  # Reload UI when review selection changes or after a forced refresh
  forceRefresh <- reactiveVal(0)
  # Tracks the last reviewID this block ran for, so it can tell a genuine
  # review switch (reset cID to the first competency) apart from a
  # same-review forceRefresh (e.g. after addComp - keep cID as-is).
  lastReviewID <- reactiveVal(NA_integer_)
  observeEvent(
    c(input$reviewID, forceRefresh()),
    {
      reviewID <- as.integer(input$reviewID)
      isNewReview <- !identical(reviewID, lastReviewID())
      lastReviewID(reviewID)

      # Get any previous scores
      review_assingment <- tbl(conn, "review_assignment") |>
        filter(id == reviewID) |>
        collect()

      compScores <- tbl(conn, "competency_score") |>
        filter(review_assignment_id == reviewID) |>
        collect()

      compText <- tbl(conn, "competency_text") |>
        filter(competency_score_id %in% local(compScores$id)) |>
        collect()

      req(nrow(review_assingment) == 1)

      reviewStatus <- review_assingment$statusCode %in% ra_done_codes

      if (reviewStatus) {
        compStatus <- 2
        overallStatus <- 2
        submitStatus <- 2
      } else {
        compStatus <- nrow(compScores) > 0 + reviewStatus
        overallStatus <- !is.na(review_assingment$utility_score_value) +
          reviewStatus
        submitStatus <- compStatus & overallStatus + reviewStatus
      }

      tabStatusIcon("comp", compStatus, session = session)
      tabStatusIcon("overall", overallStatus, session = session)
      tabStatusIcon("submit", submitStatus, session = session)

      # Competency list (from competency table). On a genuine review switch,
      # reset to the first competency as before; on a same-review
      # forceRefresh (e.g. after addComp) keep whatever was selected.
      updateRadioButtons(
        inputId = "cID",
        choiceNames = competencies_db$name,
        choiceValues = competencies_db$competency_id,
        selected = if (
          !isNewReview &&
            !is.null(input$cID) &&
            input$cID %in% as.character(competencies_db$competency_id)
        ) {
          input$cID
        } else {
          NULL
        }
      )

      # Specificity score options — loaded from the rubric attached to this review
      rr_opts <- review_rubric_opts()
      curCompScore <- compScores |>
        filter(competency_id == as.integer(input$cID))
      updateRadioButtons(
        inputId = "specificity",
        label = examples_label("Specificity score", rr_opts$specificity),
        choices = score_choices(rr_opts$specificity),
        selected = if (nrow(curCompScore) == 1) {
          curCompScore$specificity
        } else {
          character(0)
        }
      )

      # Overall scores
      updateRadioButtons(
        inputId = "util",
        label = examples_label("Utility score", rr_opts$utility),
        choices = score_choices(rr_opts$utility),
        selected = if (is.na(review_assingment$utility_score_value)) {
          character(0)
        } else {
          review_assingment$utility_score_value
        }
      )
      updateRadioButtons(
        inputId = "sent",
        label = examples_label("Sentiment score", rr_opts$sentiment),
        choices = score_choices(rr_opts$sentiment),
        selected = if (is.na(review_assingment$sentiment_score_value)) {
          character(0)
        } else {
          review_assingment$sentiment_score_value
        }
      )

      updateTextAreaInput(
        input = "reviewComment",
        value = review_assingment$note
      )

      # Submission tab
      updateCheckboxInput(
        inputId = "flag",
        value = review_assingment$statusCode == ra_flagged
      )

      updateActionButton(
        inputId = "complete",
        label = ifelse(
          review_assingment$statusCode %in% ra_done_codes,
          "Resubmit",
          "Mark as complete"
        )
      )

      # Update the competency review var
      reviewScores(list(
        overallScores = review_assingment,
        compScores = compScores,
        compText = compText
      ))
    },
    ignoreInit = T
  )

  # Update the rubric on competency change
  observeEvent(c(input$cID, input$reviewID), {
    req(reviewScores())
    # Get the previous values (if any)
    compScores <- reviewScores()$compScores |>
      filter(competency_id == as.integer(input$cID))

    updateTextAreaInput(
      input = "competencyComment",
      value = ifelse(nrow(compScores) == 0, "", compScores$note)
    )

    updateActionButton(
      inputId = "addComp",
      label = ifelse(
        nrow(compScores) == 0,
        "Add competency review",
        "Update competency review"
      )
    )
  })

  # Competency description from the competency table
  output$compDescr <- renderUI({
    req(input$cID, nrow(competencies_db) > 0)
    desc <- competencies_db$description[
      competencies_db$competency_id == as.integer(input$cID)
    ]
    tagList(tags$i(if (length(desc) == 1) desc else ""))
  })

  # Add or update a competency review
  observeEvent(input$addComp, {
    if (is.null(input$cID)) {
      showNotification(
        "Please select a competency before saving.",
        type = "warning"
      )
      return(invisible())
    }

    # Staged highlights are committed to the currently selected competency;
    # already-committed, non-deleted highlights for this competency are
    # carried over as-is. Rows marked "deleted" are deliberately excluded -
    # dbReviewUpdate()'s delete-then-reinsert below removes them from the DB
    # by simply never reinserting them.
    groupEvidence <- txtEvidence() |>
      filter(
        status == "staged" |
          (status == "committed" & group_id == as.character(input$cID))
      )

    # Nothing left to score once every highlight for this competency has
    # been marked for deletion (and nothing new was staged) - the whole
    # competency review is being removed, so specificity doesn't apply.
    onlyDeletions <- nrow(groupEvidence) == 0 &&
      any(
        txtEvidence()$status == "deleted" &
          txtEvidence()$group_id == as.character(input$cID)
      )

    if (is.null(input$specificity) && !onlyDeletions) {
      showNotification(
        "Please select a specificity score before saving.",
        type = "warning"
      )
      return(invisible())
    }

    if (onlyDeletions) {
      existingRow <- reviewScores()$compScores |>
        filter(competency_id == as.integer(input$cID))
      if (nrow(existingRow) > 0) {
        tbl_delete(
          existingRow |> select(id),
          conn,
          "competency_score",
          commit = FALSE
        )
      }

      scores <- dbReviewUpdate(
        conn = conn,
        statusCode = ra_inprogress,
        overallScores = data.frame(id = as.integer(input$reviewID)),
        commit = TRUE
      )
    } else {
      evidence <- str_trim(groupEvidence$text)
      if (length(evidence) == 0) {
        showModal(modalDialog(
          HTML(
            "Please make sure to provide miminal text evidence by higlighting",
            "pieces of text and clicking the 'Add highlighted' button in the rubric"
          ),
          title = "Text evidence missing"
        ))
      }

      req(length(evidence) > 0)

      comment <- str_trim(input$competencyComment)
      compScores <- data.frame(
        review_assignment_id = as.integer(input$reviewID),
        competency_id = as.integer(input$cID),
        specificity = as.integer(input$specificity),
        note = ifelse(comment == "", NA, comment)
      )

      compText <- data.frame(
        review_assignment_id = as.integer(input$reviewID),
        competency_id = as.integer(input$cID),
        text_match = evidence,
        start = groupEvidence$start,
        end = groupEvidence$end
      )

      scores <- dbReviewUpdate(
        conn = conn,
        statusCode = ra_inprogress,
        compScores = compScores,
        compText = compText,
        removeNotListed = F,
        commit = T
      )
    }

    # Update the reviewScores var (used in submission tab)
    reviewScores(scores)

    updateActionButton(inputId = "addComp", label = "Update competency review")
    updateReviewID(input$reviewID)

    tabStatusIcon("comp", 1, session = session)
    tabStatusIcon("submit", 1, session = session)
    forceRefresh(forceRefresh() + 1)
    highlightRefreshTrigger(highlightRefreshTrigger() + 1)
    showNotification(sprintf("Competency updated"), type = "message")
  })

  # Keep the specificity selector and the competency picker in sync with the
  # highlight evidence itself.
  # - A pending deletion (a committed highlight marked for removal) locks
  #   "cID" (via shinyjs) so the user can't switch competency away from it
  #   before resolving the deletion - a deleted row is always tied to the
  #   competency it was deleted from, so leaving it would strand it.
  # - Staged evidence does NOT lock "cID": staging is independent of the
  #   active competency (see mod_highlight_server()), so the user can browse
  #   other competencies while something is staged and commit it later.
  # Either staged or pending-deletion evidence still forces a specificity
  # reselect, since an existing saved score wasn't chosen against that
  # pending evidence. With neither, restore the previously saved score for
  # the current competency, if any.
  observeEvent(
    list(input$cID, input$reviewID, txtEvidence()),
    {
      req(reviewScores())
      compScores <- reviewScores()$compScores |>
        filter(competency_id == as.integer(input$cID))

      hasStaged <- any(txtEvidence()$status == "staged")
      hasDeleted <- any(txtEvidence()$status == "deleted")

      if (hasDeleted) {
        shinyjs::disable("cID")
      } else {
        shinyjs::enable("cID")
      }

      resetNeeded <- hasStaged || hasDeleted || nrow(compScores) == 0
      oldVal <- if (nrow(compScores) == 1) compScores$specificity else NULL

      updateRadioButtons(
        inputId = "specificity",
        label = examples_label(
          if (resetNeeded && !is.null(oldVal)) {
            sprintf("Specificity score (old = %s)", oldVal)
          } else {
            "Specificity score"
          },
          review_rubric_opts()$specificity
        ),
        selected = if (resetNeeded) character(0) else compScores$specificity
      )
    },
    ignoreInit = TRUE
  )

  # Add or update the overall scores
  observeEvent(input$addOverall, {
    if (is.null(input$util) || is.null(input$sent)) {
      showModal(modalDialog(
        HTML("Please make sure to indicate both a Utility and Sentiment score"),
        title = "Score missing"
      ))
    }

    req(!is.null(input$util) && !is.null(input$sent))

    rr_opts <- review_rubric_opts()
    overallScores <- data.frame(
      id = as.integer(input$reviewID),
      utility_score_id = rr_opts$utility$id[
        rr_opts$utility$value == input$util
      ],
      utility_score_value = as.integer(input$util),
      sentiment_score_id = rr_opts$sentiment$id[
        rr_opts$sentiment$value == input$sent
      ],
      sentiment_score_value = as.integer(input$sent),
      note = str_trim(input$reviewComment)
    )

    scores <- dbReviewUpdate(
      conn = conn,
      statusCode = ra_inprogress,
      overallScores = overallScores,
      removeNotListed = F,
      commit = T
    )

    reviewScores(scores)

    updateActionButton(inputId = "addOverall", label = "Update overall review")
    updateReviewID(input$reviewID)

    tabStatusIcon("overall", 1, session = session)
    tabStatusIcon("submit", 1, session = session)
    forceRefresh(forceRefresh() + 1)
    showNotification(sprintf("Scores updated"), type = "message")
  })

  # The summary of the review scores before submitting
  output$summary <- renderUI({
    req(reviewScores())

    overallScores <- reviewScores()$overallScores

    # Build lookup vectors for score labels from the review's rubric
    rr_opts <- review_rubric_opts()
    spec_lookup <- setNames(
      rr_opts$specificity$description,
      as.integer(rr_opts$specificity$value)
    )
    util_lookup <- setNames(
      rr_opts$utility$description,
      as.integer(rr_opts$utility$value)
    )
    sent_lookup <- setNames(
      rr_opts$sentiment$description,
      as.integer(rr_opts$sentiment$value)
    )

    compScores <- reviewScores()$compScores |>
      select(specificity, competency_id) |>
      left_join(
        competencies_db |> select(competency_id, competency = name),
        by = "competency_id"
      ) |>
      mutate(score = spec_lookup[as.character(specificity)]) |>
      select(competency, score)

    tagList(
      tags$h3("Review Summary"),
      tags$b("COMPETENCIES"),
      datatable(
        compScores,
        options = list(paging = FALSE, searching = FALSE, info = FALSE),
        selection = "none",
        rownames = FALSE
      ),
      tags$b("UTILITY"),
      p(util_lookup[as.character(overallScores$utility_score_value)]),
      tags$b("SENTIMENT"),
      p(sent_lookup[as.character(overallScores$sentiment_score_value)]),
      tags$hr()
    )
  })

  # When the complete button is clicked
  observeEvent(input$complete, {
    if (
      (nrow(reviewScores()$compScores) == 0 ||
        is.na(reviewScores()$overallScores$utility_score_value) ||
        is.na(reviewScores()$overallScores$sentiment_score_value)) &
        !input$flag
    ) {
      showModal(modalDialog(
        "You must have reviewed at least one competency or checked the issue",
        "flag in order to compete a review",
        title = "Error"
      ))
      return()
    }

    data <- data.frame(
      id = as.integer(input$reviewID),
      statusCode = ifelse(input$flag, ra_flagged, ra_completed)
    )
    tbl_update(data, conn, "review_assignment", returnData = F)

    updateReviewID(input$reviewID)
    tabStatusIcon("comp", 2, session = session)
    tabStatusIcon("overall", 2, session = session)
    tabStatusIcon("submit", 2, session = session)
    showNotification("Changes marked as complete", type = "message")
  })

  #### ANALYSIS TAB ####

  # One row per (evaluation, rubric) review round — an evaluation reviewed
  # under two rubric versions shows up as two separate rows, so analysis
  # never mixes completion stats or reviewer scores across rubric versions.
  analysisReviewData <- {
    ra <- tbl(conn, "review_assignment") |>
      left_join(
        tbl(conn, "reviewer") |> select(reviewer_id = id, human, username),
        by = "reviewer_id"
      ) |>
      collect() |>
      group_by(evaluation_id, reviewer_id, rubric_id) |>
      filter(modified == max(modified)) |>
      ungroup() |>
      mutate(username = ifelse(is.na(username), "AI Model", username))

    # Per-review scores, computed once per session. Incomplete reviews are
    # expected here (this tab shows in-progress review groups), so
    # error_on_incomplete = FALSE.
    analysisScores <- review_scores(conn, ra$id, error_on_incomplete = FALSE) |>
      mutate(total_frac = coverage + avg_specificity + utility) |>
      left_join(
        ra |> select(review_id = id, evaluation_id, rubric_id),
        by = "review_id"
      )

    scoreSummary <- analysisScores |>
      group_by(evaluation_id, rubric_id) |>
      summarise(
        avg_score = mean(total_frac, na.rm = TRUE) * 100,
        sd_score = sd(total_frac, na.rm = TRUE) * 100,
        .groups = "drop"
      )

    reviewGroups <- ra |>
      group_by(evaluation_id, rubric_id) |>
      summarise(
        assign_date = format(as.Date(min(created)), "%Y-%m-%d"),
        n = sum(statusCode %in% ra_done_codes),
        total = n(),
        completed_reviewers = {
          done <- sort(unique(username[statusCode %in% ra_done_codes]))
          if (length(done) == 0) "None" else paste(done, collapse = ", ")
        },
        .groups = "drop"
      ) |>
      mutate(perc = round(100 * n / total, 1)) |>
      left_join(scoreSummary, by = c("evaluation_id", "rubric_id"))

    evalInfo <- tbl(conn, "evaluation") |>
      left_join(tbl(conn, "rotation"), by = c("rotation_id" = "id")) |>
      left_join(tbl(conn, "clerkship"), by = c("clerkship_id" = "id")) |>
      select(id, summary_flg, clerkship, core_faculty) |>
      collect()

    reviewGroups |>
      left_join(evalInfo, by = c("evaluation_id" = "id")) |>
      mutate(
        type = ifelse(summary_flg == 1, "summative", "formative"),
        core_faculty = !is.na(core_faculty) & core_faculty == 1
      ) |>
      select(
        evaluation_id,
        assign_date,
        rubric_id,
        type,
        clerkship,
        core_faculty,
        n,
        total,
        perc,
        avg_score,
        sd_score,
        completed_reviewers
      ) |>
      arrange(desc(evaluation_id))
  }

  # Columns whose visible cell is a formatted string (for a plain text filter
  # box instead of DT's default range-slider on numeric columns) each carry a
  # hidden twin holding the raw numeric value, linked via columnDefs orderData
  # so sorting/ordering still works correctly.
  output$analysis_review_table <- renderDT({
    df <- analysisReviewData
    display <- data.frame(
      `Eval ID` = as.character(df$evaluation_id),
      `Assign Date` = df$assign_date,
      `Rubric ID` = as.character(df$rubric_id),
      Type = df$type,
      Clerkship = df$clerkship,
      `Core Faculty` = df$core_faculty,
      N = sprintf(
        '<span title="%s">%d</span>',
        htmltools::htmlEscape(df$completed_reviewers),
        df$n
      ),
      `%` = sprintf("%.1f", df$perc),
      `Avg Score` = ifelse(
        is.na(df$avg_score),
        "—",
        ifelse(
          is.na(df$sd_score),
          sprintf("%.1f", df$avg_score),
          sprintf("%.1f ± %.1f", df$avg_score, df$sd_score)
        )
      ),
      eval_id_sort = df$evaluation_id,
      rubric_id_sort = df$rubric_id,
      n_sort = df$n,
      perc_sort = df$perc,
      avg_score_sort = ifelse(is.na(df$avg_score), -1, df$avg_score),
      check.names = FALSE
    )
    datatable(
      display,
      selection = "single",
      rownames = FALSE,
      escape = -match("N", names(display)),
      filter = list(position = 'top', clear = FALSE, plain = FALSE),
      options = list(
        pageLength = 15,
        dom = "tip",
        scrollX = TRUE,
        order = list(list(1, "desc"), list(2, "desc"), list(0, "asc")),
        columnDefs = list(
          list(targets = 0, orderData = 9),
          list(targets = 2, orderData = 10),
          list(targets = 6, orderData = 11),
          list(targets = 7, orderData = 12),
          list(targets = 8, orderData = 13),
          list(targets = c(9, 10, 11, 12, 13), visible = FALSE)
        )
      )
    )
  })

  selected_analysis_review <- reactive({
    row <- input$analysis_review_table_rows_selected
    req(row)
    list(
      eval_id = analysisReviewData$evaluation_id[row],
      rubric_id = analysisReviewData$rubric_id[row]
    )
  })

  output$analysis_score_summary <- renderUI({
    sel <- selected_analysis_review()
    scores <- analysisScores |>
      filter(evaluation_id == sel$eval_id, rubric_id == sel$rubric_id)

    if (nrow(scores) == 0) {
      return(tags$p(
        "No completed reviews yet for this evaluation.",
        class = "text-muted"
      ))
    }

    stat_pair <- function(x) {
      c(mean = mean(x, na.rm = TRUE) * 100, sd = sd(x, na.rm = TRUE) * 100)
    }
    row <- function(label, s, header = FALSE) {
      td_fn <- if (header) tags$th else tags$td
      tags$tr(
        td_fn(label),
        td_fn(sprintf("%.1f", s["mean"])),
        td_fn(if (is.na(s["sd"])) "" else sprintf("%.1f", s["sd"]))
      )
    }

    tags$table(
      class = "table table-sm mt-1",
      tags$tbody(
        row("Coverage", stat_pair(scores$coverage)),
        row("Avg Specificity", stat_pair(scores$avg_specificity)),
        row("Utility", stat_pair(scores$utility)),
        row("Total", stat_pair(scores$total_frac), header = TRUE)
      )
    )
  })

  analysisInfo <- reactive({
    sel <- selected_analysis_review()
    overall <- tbl(conn, "review_assignment") |>
      filter(evaluation_id == sel$eval_id, rubric_id == sel$rubric_id) |>
      left_join(
        tbl(conn, "reviewer") |>
          select(reviewer_id = id, reviewer = username),
        by = "reviewer_id"
      ) |>
      collect() |>
      mutate(reviewer = ifelse(is.na(reviewer), "AI Model", reviewer))

    compInfo <- tbl(conn, "competency_score") |>
      filter(review_assignment_id %in% local(overall$id)) |>
      collect() |>
      left_join(
        overall |> select(review_assignment_id = id, reviewer_id, reviewer),
        by = "review_assignment_id"
      )

    compText <- tbl(conn, "competency_text") |>
      filter(competency_score_id %in% local(compInfo$id)) |>
      collect() |>
      left_join(
        compInfo |> select(competency_score_id = id, reviewer_id),
        by = "competency_score_id"
      )

    list(
      overall = overall,
      compInfo = compInfo,
      compText = compText
    )
  })

  overlapHighlights <- reactive({
    info <- analysisInfo()
    incompleteReviewers <- info$overall$reviewer_id[!info$overall$statusCode %in% ra_done_codes]

    info$compText |>
      left_join(
        info$compInfo |>
          select(competency_score_id = id, competency_id, score = specificity, note),
        by = "competency_score_id"
      ) |>
      transmute(
        id = id,
        user_id = ifelse(
          reviewer_id %in% incompleteReviewers,
          paste0(reviewer_id, "|incomplete"),
          as.character(reviewer_id)
        ),
        group_id = as.character(competency_id),
        start = start,
        end = end,
        score = score,
        note = note
      )
  })

  mod_overlap_server(
    "analysis_overlap",
    highlights = overlapHighlights,
    group_names = all_competency_names,
    user_names = all_reviewer_names_overlap,
    text = reactive({
      dbGetEvals(
        ids = selected_analysis_review()$eval_id,
        conn = conn,
        redacted = T,
        includeQuestions = T,
        html = T,
        subtitleTag = "b"
      ) |>
        pull(evaluation)
    })
  )

  comparisonTable <- reactive({
    info <- analysisInfo()

    # Build lookup structures from server-scope rubric data
    spec_lookup <- setNames(
      specificity_opts$description,
      as.integer(specificity_opts$value)
    )
    util_df <- data.frame(
      utility_score_id = utility_opts$id,
      util = utility_opts$description
    )
    sent_df <- data.frame(
      sentiment_score_id = sentiment_opts$id,
      sent = sentiment_opts$description
    )
    comp_metric <- data.frame(
      competency_id = competencies_db$competency_id,
      metric = paste("COMPETENCY -", competencies_db$name)
    )

    bind_rows(
      info$compInfo |>
        select(reviewer, competency_id, specificity, note) |>
        mutate(
          specificity = ifelse(
            is.na(specificity),
            NA_character_,
            paste(specificity, "-", spec_lookup[as.character(specificity)])
          ),
          specificity = ifelse(
            is.na(note),
            specificity,
            sprintf(
              "%s<br><i style='color:#e04233;'>NOTE: %s<i>",
              specificity,
              note
            )
          )
        ) |>
        pivot_wider(
          id_cols = competency_id,
          names_from = reviewer,
          values_from = specificity
        ) |>
        left_join(comp_metric, by = "competency_id") |>
        select(-competency_id),
      info$overall |>
        select(
          utility_score_id,
          utility_score_value,
          sentiment_score_id,
          sentiment_score_value,
          note
        ) |>
        mutate(
          note = ifelse(
            is.na(note),
            NA,
            sprintf("<i style='color:#e04233;'>%s<i>", note)
          )
        ) |>
        left_join(util_df, by = "utility_score_id") |>
        left_join(sent_df, by = "sentiment_score_id") |>
        mutate(
          util = ifelse(
            is.na(utility_score_value),
            NA_character_,
            paste(utility_score_value, "-", util)
          ),
          sent = ifelse(
            is.na(sentiment_score_value),
            NA_character_,
            paste(sentiment_score_value, "-", sent)
          )
        ) |>
        select(util, sent, note) |>
        t() |>
        as.data.frame() |>
        rename_with(function(x) as.character(info$overall$reviewer)) |>
        mutate(metric = c("UTILITY", "SENTIMENT", "REVIEW NOTE"))
    )
  })

  output$analysis_table <- renderDT(
    {
      comparisonTable() |>
        select(METRIC = metric, everything())
    },
    select = "single",
    escape = F,
    options = list(paging = F, searching = F, info = F, ordering = F),
    rownames = F
  )

  output$textMatches <- renderUI({
    cID <- input$analysis_table_rows_selected
    cID <- ifelse(
      is.null(cID) || cID > n_distinct(analysisInfo()$compInfo$competency_id),
      NA,
      cID
    )

    if (is.na(cID)) {
      return(tagList())
    }

    cID <- unique(analysisInfo()$compInfo$competency_id)[cID]

    html <- analysisInfo()$compInfo |>
      filter(competency_id == cID) |>
      select(reviewer, id) |>
      left_join(
        analysisInfo()$compText |> select(id = competency_score_id, text_match),
        by = "id"
      ) |>
      group_by(reviewer) |>
      summarise(
        html = as.character(tagList(
          tags$label(reviewer[1]),
          tags$ul(lapply(text_match, tags$li))
        )),
        .groups = "drop"
      ) |>
      pull(html) |>
      paste(collapse = "")

    tagList(
      card(
        card_header("Text Matches for selected competency"),
        HTML(html)
      )
    )
  })

  ### Import or export the database as pins
  observeEvent(input$refreshDB, {
    showModal(modalDialog(
      title = "ADMINPASSWORD",
      checkboxGroupInput(
        "pinAction",
        "Action",
        choices = c("import", "export", "download")
      ),
      passwordInput("adminPass", "Admin Password"),
      actionButton("adminCheck", "Authenticate")
    ))
  })

  observeEvent(input$adminCheck, {
    if (
      Sys.getenv("adminPass") == "" ||
        Sys.getenv("adminPass") != input$adminPass
    ) {
      showNotification("Password incorrect or not activated", type = "error")
      return()
    }

    pinAction <- input$pinAction[input$pinAction %in% c("import", "export")]
    if (length(pinAction) > 0) {
      check <- pinDB(dbInfo, pinAction)

      if (check$success) {
        showNotification(check$msg, type = "message")
      } else {
        showNotification(check$msg, type = "error")
      }

      if ("import" %in% pinAction && check$success) {
        # The connection opened at app start still points at the replaced
        # file, so reload the session to pick up the freshly imported DB
        removeModal()
        session$reload()
        return()
      }
    }

    removeModal()
    if ("download" %in% input$pinAction) {
      showModal(modalDialog(mod_dbSetup_ui("dbMod", "link")))
    }
  })

  #### RUBRIC TAB ####

  loadRubrics <- function() {
    tbl(conn, "rubric") |>
      select(id, timestamp) |>
      collect() |>
      arrange(desc(id)) |>
      mutate(label = sprintf("%d - %s", id, substr(timestamp, 1, 10)))
  }

  local({
    rubrics_df <- loadRubrics()
    updateSelectInput(
      session,
      "rubricID",
      choices = setNames(rubrics_df$id, rubrics_df$label)
    )
  })

  # Incrementing this forces all rubric_*_orig reactives to re-fetch from DB.
  rubric_refresh_trigger <- reactiveVal(0)

  # Original competency data for the selected rubric (DB state, read-only reference)
  rubric_comps_orig <- reactive({
    rubric_refresh_trigger()
    req(input$rubricID)
    tbl(conn, "rubric_competency") |>
      filter(rubric_id == local(as.integer(input$rubricID))) |>
      inner_join(
        tbl(conn, "competency") |>
          select(competency_id = id, name, description, note),
        by = "competency_id"
      ) |>
      arrange(order) |>
      select(
        rc_id = id,
        competency_id,
        comp_order = order,
        name,
        description,
        note
      ) |>
      collect()
  })

  # Mutable copy updated as the user edits cells
  rubric_comps_edited <- reactiveVal(NULL)

  observeEvent(rubric_comps_orig(), {
    rubric_comps_edited(rubric_comps_orig())
  })

  output$rubricCompTable <- renderDT(
    {
      req(rubric_comps_edited())
      rubric_comps_edited() |>
        select(
          Order = comp_order,
          Name = name,
          Description = description,
          Note = note
        )
    },
    editable = list(target = "cell"),
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      ordering = FALSE
    ),
    rownames = FALSE,
    selection = "none"
  )

  observeEvent(input$rubricCompTable_cell_edit, {
    info <- input$rubricCompTable_cell_edit
    df <- rubric_comps_edited()
    # Displayed columns (0-indexed): 0=comp_order, 1=name, 2=description, 3=note
    col_name <- c("comp_order", "name", "description", "note")[info$col + 1]
    df[info$row, col_name] <- DT::coerceValue(
      info$value,
      df[info$row, col_name]
    )
    rubric_comps_edited(df)
  })

  # Add a blank row for a brand-new competency, pre-filled with the next
  # available order. It only reaches the database once the rubric is saved.
  observeEvent(input$addCompetency, {
    df <- rubric_comps_edited()
    req(df)
    next_order <- suppressWarnings(max(
      as.integer(df$comp_order),
      0,
      na.rm = TRUE
    )) +
      1
    rubric_comps_edited(rbind(
      df,
      data.frame(
        rc_id = NA_integer_,
        competency_id = NA_integer_,
        comp_order = next_order,
        name = "",
        description = "",
        note = NA_character_
      )
    ))
  })

  # ── Specificity ───────────────────────────────────────────────────────────────

  rubric_spec_orig <- reactive({
    rubric_refresh_trigger()
    req(input$rubricID)
    rid <- as.integer(input$rubricID)
    rows <- tbl(conn, "rubric_specificity") |>
      filter(rubric_id == local(rid)) |>
      left_join(
        tbl(conn, "specificity") |>
          select(specificity_id = id, value, description, example, note),
        by = "specificity_id"
      ) |>
      select(specificity_id, value, description, example, note) |>
      collect() |>
      arrange(value)
    if (nrow(rows) == 0) {
      tbl(conn, "specificity") |>
        select(specificity_id = id, value, description, example, note) |>
        collect() |>
        arrange(value)
    } else {
      rows
    }
  })

  rubric_spec_edited <- reactiveVal(NULL)
  observeEvent(rubric_spec_orig(), {
    rubric_spec_edited(rubric_spec_orig())
  })

  output$rubricSpecTable <- renderDT(
    {
      req(rubric_spec_edited())
      rubric_spec_edited() |>
        select(
          Value = value,
          Description = description,
          Example = example,
          Note = note
        )
    },
    editable = list(target = "cell"),
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      ordering = FALSE
    ),
    rownames = FALSE,
    selection = "none"
  )

  observeEvent(input$rubricSpecTable_cell_edit, {
    info <- input$rubricSpecTable_cell_edit
    df <- rubric_spec_edited()
    col_name <- c("value", "description", "example", "note")[info$col + 1]
    df[info$row, col_name] <- DT::coerceValue(
      info$value,
      df[info$row, col_name]
    )
    rubric_spec_edited(df)
  })

  # ── Utility ───────────────────────────────────────────────────────────────────

  rubric_util_orig <- reactive({
    rubric_refresh_trigger()
    req(input$rubricID)
    rid <- as.integer(input$rubricID)
    rows <- tbl(conn, "rubric_utility") |>
      filter(rubric_id == local(rid)) |>
      left_join(
        tbl(conn, "utility") |>
          select(utility_id = id, value, description, example, note),
        by = "utility_id"
      ) |>
      select(utility_id, value, description, example, note) |>
      collect() |>
      arrange(value)
    if (nrow(rows) == 0) {
      tbl(conn, "utility") |>
        select(utility_id = id, value, description, example, note) |>
        collect() |>
        arrange(value)
    } else {
      rows
    }
  })

  rubric_util_edited <- reactiveVal(NULL)
  observeEvent(rubric_util_orig(), {
    rubric_util_edited(rubric_util_orig())
  })

  output$rubricUtilTable <- renderDT(
    {
      req(rubric_util_edited())
      rubric_util_edited() |>
        select(
          Value = value,
          Description = description,
          Example = example,
          Note = note
        )
    },
    editable = list(target = "cell"),
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      ordering = FALSE
    ),
    rownames = FALSE,
    selection = "none"
  )

  observeEvent(input$rubricUtilTable_cell_edit, {
    info <- input$rubricUtilTable_cell_edit
    df <- rubric_util_edited()
    col_name <- c("value", "description", "example", "note")[info$col + 1]
    df[info$row, col_name] <- DT::coerceValue(
      info$value,
      df[info$row, col_name]
    )
    rubric_util_edited(df)
  })

  # ── Sentiment ─────────────────────────────────────────────────────────────────

  rubric_sent_orig <- reactive({
    rubric_refresh_trigger()
    req(input$rubricID)
    rid <- as.integer(input$rubricID)
    rows <- tbl(conn, "rubric_sentiment") |>
      filter(rubric_id == local(rid)) |>
      left_join(
        tbl(conn, "sentiment") |>
          select(sentiment_id = id, value, description, example, note),
        by = "sentiment_id"
      ) |>
      select(sentiment_id, value, description, example, note) |>
      collect() |>
      arrange(value)
    if (nrow(rows) == 0) {
      tbl(conn, "sentiment") |>
        select(sentiment_id = id, value, description, example, note) |>
        collect() |>
        arrange(value)
    } else {
      rows
    }
  })

  rubric_sent_edited <- reactiveVal(NULL)
  observeEvent(rubric_sent_orig(), {
    rubric_sent_edited(rubric_sent_orig())
  })

  output$rubricSentTable <- renderDT(
    {
      req(rubric_sent_edited())
      rubric_sent_edited() |>
        select(
          Value = value,
          Description = description,
          Example = example,
          Note = note
        )
    },
    editable = list(target = "cell"),
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      ordering = FALSE
    ),
    rownames = FALSE,
    selection = "none"
  )

  observeEvent(input$rubricSentTable_cell_edit, {
    info <- input$rubricSentTable_cell_edit
    df <- rubric_sent_edited()
    col_name <- c("value", "description", "example", "note")[info$col + 1]
    df[info$row, col_name] <- DT::coerceValue(
      info$value,
      df[info$row, col_name]
    )
    rubric_sent_edited(df)
  })

  observeEvent(input$updateRubric, {
    edited <- rubric_comps_edited()
    orig <- rubric_comps_orig()

    orders_raw <- suppressWarnings(as.integer(edited$comp_order))
    if (any(is.na(orders_raw))) {
      showModal(modalDialog(
        "Order values must be whole numbers.",
        title = "Invalid order values"
      ))
      return()
    }

    # Order 0 is a proxy for deleting the competency from this rubric version
    edited <- edited[orders_raw != 0, ]
    orders <- orders_raw[orders_raw != 0]

    if (nrow(edited) == 0) {
      showModal(modalDialog(
        "At least one competency must remain (order can't be 0 for all rows).",
        title = "Invalid order values"
      ))
      return()
    }

    # Remaining order values must be integers 1-N with no duplicates or gaps
    if (!identical(sort(orders), seq_len(nrow(edited)))) {
      showModal(modalDialog(
        sprintf(
          "Order values must be the integers 1 to %d with no duplicates (use 0 to remove a competency).",
          nrow(edited)
        ),
        title = "Invalid order values"
      ))
      return()
    }

    if (
      any(!nzchar(trimws(edited$name))) ||
        any(!nzchar(trimws(edited$description)))
    ) {
      showModal(modalDialog(
        "Name and description cannot be blank for any competency.",
        title = "Missing required fields"
      ))
      return()
    }

    # Sort rows by order ascending before saving
    edited <- edited[order(orders), ]
    edited$comp_order <- sort(orders)

    # Insert a new competency row only when name/description/note changed
    # (or it's a brand-new row with no competency_id yet); otherwise reuse
    # the existing competency_id
    new_comp_ids <- integer(nrow(edited))
    for (i in seq_len(nrow(edited))) {
      orig_row <- orig[orig$competency_id %in% edited$competency_id[i], ]
      orig_note <- if (nrow(orig_row) == 0 || is.na(orig_row$note)) {
        NA_character_
      } else {
        orig_row$note
      }
      edit_note <- if (
        is.na(edited$note[i]) || !nzchar(trimws(edited$note[i]))
      ) {
        NA_character_
      } else {
        edited$note[i]
      }
      changed <- nrow(orig_row) == 0 ||
        edited$name[i] != orig_row$name ||
        edited$description[i] != orig_row$description ||
        !identical(edit_note, orig_note)

      if (changed) {
        new_comp_ids[i] <- tbl_insert(
          data.frame(
            cID = edited$comp_order[i],
            name = edited$name[i],
            description = edited$description[i],
            note = edit_note
          ),
          conn,
          "competency"
        ) |>
          pull(id)
      } else {
        new_comp_ids[i] <- edited$competency_id[i]
      }
    }

    # Use the latest stored prompts of each type
    latest_extract_id <- tbl(conn, "prompt") |>
      filter(task == "comp_extract") |>
      summarise(id = max(id, na.rm = TRUE)) |>
      pull(id)
    latest_score_id <- tbl(conn, "prompt") |>
      filter(task == "comp_score") |>
      summarise(id = max(id, na.rm = TRUE)) |>
      pull(id)

    new_rubric_id <- tbl_insert(
      data.frame(
        prompt_extract_id = if (
          length(latest_extract_id) == 0 || is.na(latest_extract_id)
        ) {
          NA_integer_
        } else {
          as.integer(latest_extract_id)
        },
        prompt_score_id = if (
          length(latest_score_id) == 0 || is.na(latest_score_id)
        ) {
          NA_integer_
        } else {
          as.integer(latest_score_id)
        }
      ),
      conn,
      "rubric"
    ) |>
      pull(id)

    tbl_insert(
      data.frame(
        rubric_id = new_rubric_id,
        competency_id = new_comp_ids,
        order = edited$comp_order
      ),
      conn,
      "rubric_competency",
      returnData = FALSE
    )

    # Helper: normalize blank/NA to NA_character_ for comparison and storage
    cell_val <- function(x) {
      if (is.na(x) || !nzchar(trimws(x))) NA_character_ else as.character(x)
    }

    # Resolve scoring table IDs (create new row if content changed, reuse if not)
    resolve_score_ids <- function(ed, orig_data, id_col, table_name) {
      new_ids <- integer(nrow(ed))
      for (i in seq_len(nrow(ed))) {
        orig_row <- orig_data[orig_data[[id_col]] == ed[[id_col]][i], ]
        changed <- nrow(orig_row) == 0 ||
          !identical(cell_val(ed$value[i]), cell_val(orig_row$value)) ||
          !identical(
            cell_val(ed$description[i]),
            cell_val(orig_row$description)
          ) ||
          !identical(cell_val(ed$example[i]), cell_val(orig_row$example)) ||
          !identical(cell_val(ed$note[i]), cell_val(orig_row$note))
        if (changed) {
          new_ids[i] <- tbl_insert(
            data.frame(
              value = ed$value[i],
              description = ed$description[i],
              example = cell_val(ed$example[i]),
              note = cell_val(ed$note[i]),
              stringsAsFactors = FALSE
            ),
            conn,
            table_name
          ) |>
            pull(id)
        } else {
          new_ids[i] <- ed[[id_col]][i]
        }
      }
      new_ids
    }

    new_spec_ids <- resolve_score_ids(
      rubric_spec_edited(),
      rubric_spec_orig(),
      "specificity_id",
      "specificity"
    )
    tbl_insert(
      data.frame(rubric_id = new_rubric_id, specificity_id = new_spec_ids),
      conn,
      "rubric_specificity",
      returnData = FALSE
    )

    new_util_ids <- resolve_score_ids(
      rubric_util_edited(),
      rubric_util_orig(),
      "utility_id",
      "utility"
    )
    tbl_insert(
      data.frame(rubric_id = new_rubric_id, utility_id = new_util_ids),
      conn,
      "rubric_utility",
      returnData = FALSE
    )

    new_sent_ids <- resolve_score_ids(
      rubric_sent_edited(),
      rubric_sent_orig(),
      "sentiment_id",
      "sentiment"
    )
    tbl_insert(
      data.frame(rubric_id = new_rubric_id, sentiment_id = new_sent_ids),
      conn,
      "rubric_sentiment",
      returnData = FALSE
    )

    rubrics_df <- loadRubrics()
    updateSelectInput(
      session,
      "rubricID",
      choices = setNames(rubrics_df$id, rubrics_df$label),
      selected = new_rubric_id
    )

    showNotification(
      sprintf("New rubric version %d created", new_rubric_id),
      type = "message"
    )
  })

  observeEvent(input$updateRubricInPlace, {
    selected_rid <- as.integer(input$rubricID)

    n_reviews <- tbl(conn, "review_assignment") |>
      filter(rubric_id == local(selected_rid)) |>
      count() |>
      pull(n)

    if (n_reviews > 0) {
      showModal(modalDialog(
        sprintf(
          "Rubric %d has been used in %d review(s) and cannot be edited in place. Use 'Save as new rubric version' instead.",
          selected_rid,
          as.integer(n_reviews)
        ),
        title = "Rubric in use"
      ))
      return()
    }

    comps_check <- rubric_comps_edited()
    live_rows <- suppressWarnings(as.integer(comps_check$comp_order)) != 0
    if (
      any(!nzchar(trimws(comps_check$name[live_rows]))) ||
        any(!nzchar(trimws(comps_check$description[live_rows])))
    ) {
      showModal(modalDialog(
        "Name and description cannot be blank for any competency.",
        title = "Missing required fields"
      ))
      return()
    }

    norm <- function(x) {
      if (is.na(x) || !nzchar(trimws(x))) NA_character_ else as.character(x)
    }

    # Update a content row if any of the specified columns changed
    update_row_if_changed <- function(
      row_id,
      ed_row,
      orig_df,
      id_col,
      table_name,
      cols
    ) {
      orig_row <- orig_df[orig_df[[id_col]] == row_id, ]
      if (nrow(orig_row) == 0) {
        return(invisible(NULL))
      }
      if (
        !any(sapply(cols, function(col) {
          !identical(norm(ed_row[[col]]), norm(orig_row[[col]]))
        }))
      ) {
        return(invisible(NULL))
      }
      upd <- as.data.frame(
        c(
          list(id = row_id),
          setNames(lapply(cols, function(col) norm(ed_row[[col]])), cols)
        ),
        stringsAsFactors = FALSE
      )
      tbl_update(upd, conn, table_name, returnData = FALSE)
    }

    comps_ed <- rubric_comps_edited()
    orig_comps <- rubric_comps_orig()

    # Order 0 is a proxy for deleting the competency from this rubric
    comp_orders <- suppressWarnings(as.integer(comps_ed$comp_order))
    to_delete <- comps_ed[!is.na(comp_orders) & comp_orders == 0, ]
    comps_ed <- comps_ed[is.na(comp_orders) | comp_orders != 0, ]

    to_delete <- to_delete[!is.na(to_delete$rc_id), ]
    if (nrow(to_delete) > 0) {
      tbl_delete(
        data.frame(id = to_delete$rc_id),
        conn,
        "rubric_competency",
        returnData = FALSE
      )
    }

    # Rows added via "Add competency" have no competency_id yet; create the
    # competency and attach it to this rubric
    new_rows <- comps_ed[is.na(comps_ed$competency_id), ]
    if (nrow(new_rows) > 0) {
      for (i in seq_len(nrow(new_rows))) {
        new_id <- tbl_insert(
          data.frame(
            cID = new_rows$comp_order[i],
            name = new_rows$name[i],
            description = new_rows$description[i],
            note = norm(new_rows$note[i])
          ),
          conn,
          "competency"
        ) |>
          pull(id)

        tbl_insert(
          data.frame(
            rubric_id = selected_rid,
            competency_id = new_id,
            order = new_rows$comp_order[i]
          ),
          conn,
          "rubric_competency",
          returnData = FALSE
        )
      }
      comps_ed <- comps_ed[!is.na(comps_ed$competency_id), ]
    }

    for (i in seq_len(nrow(comps_ed))) {
      update_row_if_changed(
        comps_ed$competency_id[i],
        comps_ed[i, ],
        orig_comps,
        "competency_id",
        "competency",
        c("name", "description", "note")
      )
    }

    spec_ed <- rubric_spec_edited()
    orig_specs <- rubric_spec_orig()
    for (i in seq_len(nrow(spec_ed))) {
      update_row_if_changed(
        spec_ed$specificity_id[i],
        spec_ed[i, ],
        orig_specs,
        "specificity_id",
        "specificity",
        c("value", "description", "example", "note")
      )
    }

    util_ed <- rubric_util_edited()
    orig_utils <- rubric_util_orig()
    for (i in seq_len(nrow(util_ed))) {
      update_row_if_changed(
        util_ed$utility_id[i],
        util_ed[i, ],
        orig_utils,
        "utility_id",
        "utility",
        c("value", "description", "example", "note")
      )
    }

    sent_ed <- rubric_sent_edited()
    orig_sents <- rubric_sent_orig()
    for (i in seq_len(nrow(sent_ed))) {
      update_row_if_changed(
        sent_ed$sentiment_id[i],
        sent_ed[i, ],
        orig_sents,
        "sentiment_id",
        "sentiment",
        c("value", "description", "example", "note")
      )
    }

    # Competency/rule/score edits change what the extraction/scoring prompts
    # should say, so regenerate and relink them to keep the stored prompt
    # text in sync with the rubric's current content.
    rubric_link_prompts(conn, selected_rid)

    rubric_refresh_trigger(rubric_refresh_trigger() + 1)
    showNotification(
      if (nrow(to_delete) > 0) {
        sprintf(
          "Rubric %d updated in place (%d competenc%s removed)",
          selected_rid,
          nrow(to_delete),
          if (nrow(to_delete) == 1) "y" else "ies"
        )
      } else {
        sprintf("Rubric %d updated in place", selected_rid)
      },
      type = "message"
    )
  })
  #### ASSIGNMENT TAB ####

  # Reactive: evaluations eligible to assign, respecting the checkbox filter.
  # Always excluded: any eval already assigned with the latest rubric.
  # Unchecked: also exclude evals that have any assignment (any rubric).
  # Checked:   include evals assigned only under a different rubric.
  assignment_eval_choices <- reactive({
    all_evals <- tbl(conn, "evaluation") |>
      left_join(tbl(conn, "rotation"), by = c("rotation_id" = "id")) |>
      left_join(tbl(conn, "clerkship"), by = c("clerkship_id" = "id")) |>
      left_join(
        tbl(conn, "evaluator") |>
          select(evaluator_id = id, original_evaluator_id),
        by = "evaluator_id"
      ) |>
      select(
        id,
        complete,
        summary_flg,
        rotation_date,
        clerkship,
        original_evaluator_id,
        core_faculty
      ) |>
      collect() |>
      mutate(original_evaluator_id = as.character(original_evaluator_id)) |>
      arrange(id)

    latest_assigned <- tbl(conn, "review_assignment") |>
      filter(rubric_id == local(latest_rubric_id)) |>
      pull(evaluation_id) |>
      unique()

    if (isTRUE(input$includeOtherRubric)) {
      all_evals |> filter(!id %in% latest_assigned)
    } else {
      any_assigned <- tbl(conn, "review_assignment") |>
        pull(evaluation_id) |>
        unique()
      all_evals |> filter(!id %in% any_assigned)
    }
  })

  output$assignment_eval_table <- renderDT({
    df <- assignment_eval_choices()
    df$complete <- ifelse(df$complete == 1, "complete", "incomplete")
    df$summary_flg <- ifelse(df$summary_flg == 1, "summative", "formative")
    df$core_faculty <- !is.na(df$core_faculty) & df$core_faculty == 1
    names(df) <- c(
      "ID",
      "Complete",
      "Type",
      "Rotation Date",
      "Clerkship",
      "Evaluator ID",
      "Core Faculty"
    )
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      filter = list(
        position = 'top',
        clear = FALSE, # hide the X clear button
        plain = FALSE # use styled inputs
      ),
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
    )
  })

  selected_assignment_eval_id <- reactive({
    row <- input$assignment_eval_table_rows_selected
    req(row)
    assignment_eval_choices()$id[row]
  })

  output$assignment_evaluation <- renderUI({
    req(selected_assignment_eval_id())
    div(
      HTML(
        dbGetEvals(
          ids = selected_assignment_eval_id(),
          conn = conn,
          redacted = T,
          includeQuestions = T,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 70vh; overflow-y: auto;"
    )
  })

  observeEvent(input$assignToAll, {
    req(selected_assignment_eval_id())
    eval_id <- selected_assignment_eval_id()

    all_reviewers <- tbl(conn, "reviewer") |>
      select(id) |>
      collect()

    existing <- tbl(conn, "review_assignment") |>
      filter(evaluation_id == eval_id) |>
      pull(reviewer_id)

    new_reviewers <- all_reviewers$id[!all_reviewers$id %in% existing]

    for (rid in new_reviewers) {
      dbReviewAssignment(
        conn = conn,
        reviewer_id = rid,
        evaluation_id = eval_id,
        commit = TRUE
      )
    }

    showNotification(
      sprintf(
        "Assigned to %d reviewer(s). %d already had an assignment.",
        length(new_reviewers),
        length(existing)
      ),
      type = "message"
    )
  })
}

shinyApp(ui, server)

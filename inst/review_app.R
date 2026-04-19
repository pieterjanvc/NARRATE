# https://rstudio.github.io/bslib/articles/cards/index.html
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(sqlife)


dbInfo <- "../local/clean_up.db"
# dbInfo <- "../local/cfme.db"
# dbInfo <- "~/Downloads/cfme.db"

# This is the db used during deployment, see deployShinyApp()
if (!file.exists(dbInfo)) {
  dbInfo <- "cfme.db"
  library(CFME)
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
          card_header(
            div(
              "Student evaluation",
              checkboxInput(
                "showQuestions",
                "Show questions",
                value = T,
                width = "auto"
              ),
              class = "d-flex gap-3"
            )
          ),
          uiOutput("evaluation")
        ),
        navset_card_tab(
          nav_panel(
            title = div(" Competencies", id = "compTitle"),
            selectInput("cID", "Competency", choices = NULL, width = "100%"),
            uiOutput("compDescr"),
            mod_highlight_ui(
              "highlights",
              "evaluation",
              "Text evidence (required)"
            ),
            radioButtons(
              "specificity",
              "Specificity score",
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
          selectInput(
            "analysis_evalID",
            "Select a review to compare",
            choices = c(),
            width = "100%"
          ),
          uiOutput("analysis_evaluation")
        )
      ),
      uiOutput("textMatches"),
      layout_columns(
        card(
          card_header("Comparison"),
          div(DTOutput("analysis_table"))
        )
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
    schema = "../inst/cfme.sql",
    useDB = dbInfo
  )
  # DB Connection
  conn <- dbGetConn(dbInfo, session = session)

  # ── Rubric data ──────────────────────────────────────────────────────────────
  # Load once per session from the competency and score tables populated by
  # rubric_parsing(). These replace the old parsePrompt() approach.

  competencies_db <- tbl(conn, "competency") |>
    group_by(cID) |>
    filter(id == max(id)) |>
    ungroup() |>
    arrange(cID) |>
    collect()

  scores_db <- tbl(conn, "score") |> collect()
  specificity_opts <- scores_db |>
    filter(category == "Specificity") |>
    arrange(as.integer(value))
  utility_opts <- scores_db |>
    filter(category == "Utility") |>
    arrange(as.integer(value))
  sentiment_opts <- scores_db |>
    filter(category == "Sentiment") |>
    arrange(as.integer(value))

  # Helper: named vector of integer value → "N - description" label
  score_choices <- function(opts) {
    setNames(as.integer(opts$value), paste(opts$value, "-", opts$description))
  }

  reviewScores <- reactiveVal()

  # Highlight selection module
  defaultEvidence <- reactiveVal(c())
  resetSel <- reactiveVal()
  txtEvidence <- mod_highlight_server(
    "highlights",
    defaults = defaultEvidence,
    reset = resetSel
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
          c(1, 0, -1, 2, 5, setdiff(c(0:5, -1), unique(statusCode)))
        ),
        evaluation_id
      ) |>
      mutate(
        descr = sprintf(
          "%s (%s %s) - %s",
          evaluation_id,
          ifelse(complete == 1, "complete", "incomplete"),
          ifelse(summary_flg == 1, "summative", "formative"),
          case_when(
            statusCode == 0 ~ "New",
            statusCode == 1 ~ "In progress",
            statusCode %in% c(2, 5) ~ "Completed",
            statusCode == -1 ~ "Completed with flag",
            TRUE ~ "Error"
          )
        )
      )

    lblInfo <- reviews |>
      filter(statusCode %in% c(-1, 0, 1, 2, 5)) |>
      group_by(statusCode) |>
      summarise(n = n())

    lblInfo <- data.frame(statusCode = c(-1, 0, 1, 2, 5)) |>
      left_join(lblInfo, by = "statusCode") |>
      mutate(n = ifelse(is.na(n), 0, n)) |>
      pull(n)
    # lblInfo: [1]=-1 (flagged), [2]=0 (new), [3]=1 (in progress), [4]=2 (done), [5]=5 (AI done)

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
      filter(id == reviewID, statusCode == 0) |>
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
    if (!is.null(extract_res) && isTRUE(extract_res$statusCode == 3)) {
      llm_comp_score_run(conn, review_ids = rid, force = TRUE)
    }

    final_status <- tbl(conn, "review_assignment") |>
      filter(id == rid) |>
      pull(statusCode)

    removeModal()
    updateReviewID(input$reviewID)
    forceRefresh(forceRefresh() + 1)
    showNotification(
      if (isTRUE(final_status == 5)) {
        "AI review complete"
      } else {
        "AI review failed"
      },
      type = if (isTRUE(final_status == 5)) "message" else "error"
    )
  })

  # Reload UI when review selection changes or after a forced refresh
  forceRefresh <- reactiveVal(0)
  observeEvent(
    c(input$reviewID, forceRefresh()),
    {
      reviewID <- as.integer(input$reviewID)

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

      reviewStatus <- review_assingment$statusCode %in% c(-1, 2, 5)

      if (reviewStatus) {
        compStatus <- 2
        overallStatus <- 2
        submitStatus <- 2
      } else {
        compStatus <- nrow(compScores) > 0 + reviewStatus
        overallStatus <- !is.na(review_assingment$utility) + reviewStatus
        submitStatus <- compStatus & overallStatus + reviewStatus
      }

      tabStatusIcon("comp", compStatus, session = session)
      tabStatusIcon("overall", overallStatus, session = session)
      tabStatusIcon("submit", submitStatus, session = session)

      # Competency list (from competency table)
      updateSelectInput(
        inputId = "cID",
        choices = setNames(competencies_db$id, competencies_db$name)
      )

      # Specificity score options
      updateRadioButtons(
        inputId = "specificity",
        label = "Specificity score",
        choices = score_choices(specificity_opts),
        selected = NULL
      )

      # Overall scores
      updateRadioButtons(
        inputId = "util",
        label = "Utility score",
        choices = score_choices(utility_opts),
        selected = if (is.na(review_assingment$utility)) {
          character(0)
        } else {
          review_assingment$utility
        }
      )
      updateRadioButtons(
        inputId = "sent",
        label = "Sentiment score",
        choices = score_choices(sentiment_opts),
        selected = if (is.na(review_assingment$sentiment)) {
          character(0)
        } else {
          review_assingment$sentiment
        }
      )

      updateTextAreaInput(
        input = "reviewComment",
        value = review_assingment$note
      )

      # Submission tab
      updateCheckboxInput(
        inputId = "flag",
        value = review_assingment$statusCode == -1
      )

      updateActionButton(
        inputId = "complete",
        label = ifelse(
          review_assingment$statusCode %in% c(-1, 2, 5),
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

    compText <- reviewScores()$compText |>
      filter(competency_score_id %in% compScores$id)

    # Update all competency scoring values
    defaultEvidence(compText$text_match)
    resetSel(Sys.time())

    updateRadioButtons(
      inputId = "specificity",
      selected = if (nrow(compScores) == 0) {
        character(0)
      } else {
        compScores$specificity
      }
    )

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
      competencies_db$id == as.integer(input$cID)
    ]
    tagList(tags$i(if (length(desc) == 1) desc else ""))
  })

  # The UI that shows the evaluation
  output$evaluation <- renderUI({
    req(input$reviewID)
    evalID <- tbl(conn, "review_assignment") |>
      filter(id == as.integer(input$reviewID)) |>
      pull(evaluation_id)

    div(
      HTML(
        dbGetEvals(
          ids = evalID,
          conn = conn,
          redacted = T,
          includeQuestions = input$showQuestions,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 70vh; overflow-y: auto;"
    )
  })

  # Add or update a competency review
  observeEvent(input$addComp, {
    evidence <- str_trim(txtEvidence()$text)
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

    if (is.null(input$specificity)) {
      showModal(modalDialog(
        HTML("Please make sure to select a specificity score"),
        title = "Text evidence missing"
      ))
    }

    req(input$specificity)

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
      text_match = evidence
    )

    scores <- dbReviewUpdate(
      conn = conn,
      statusCode = 1,
      compScores = compScores,
      compText = compText,
      removeNotListed = F,
      commit = T
    )

    # Update the reviewScores var (used in submission tab)
    reviewScores(scores)

    updateActionButton(inputId = "addComp", label = "Update competency review")
    updateReviewID(input$reviewID)

    tabStatusIcon("comp", 1, session = session)
    tabStatusIcon("submit", 1, session = session)
    showNotification(sprintf("Competency updated"), type = "message")
  })

  # Add or update the overall scores
  observeEvent(input$addOverall, {
    if (is.null(input$util) || is.null(input$sent)) {
      showModal(modalDialog(
        HTML("Please make sure to indicate both a Utility and Sentiment score"),
        title = "Score missing"
      ))
    }

    req(!is.null(input$util) && !is.null(input$sent))

    overallScores <- data.frame(
      id = as.integer(input$reviewID),
      utility = as.integer(input$util),
      sentiment = as.integer(input$sent),
      note = str_trim(input$reviewComment)
    )

    scores <- dbReviewUpdate(
      conn = conn,
      statusCode = 1,
      overallScores = overallScores,
      removeNotListed = F,
      commit = T
    )

    reviewScores(scores)

    updateActionButton(inputId = "addOverall", label = "Update overall review")
    updateReviewID(input$reviewID)

    tabStatusIcon("overall", 1, session = session)
    tabStatusIcon("submit", 1, session = session)
    showNotification(sprintf("Scores updated"), type = "message")
  })

  # The summary of the review scores before submitting
  output$summary <- renderUI({
    req(reviewScores())

    overallScores <- reviewScores()$overallScores

    # Build lookup vectors for score labels
    spec_lookup <- setNames(
      specificity_opts$description,
      as.integer(specificity_opts$value)
    )
    util_lookup <- setNames(
      utility_opts$description,
      as.integer(utility_opts$value)
    )
    sent_lookup <- setNames(
      sentiment_opts$description,
      as.integer(sentiment_opts$value)
    )

    compScores <- reviewScores()$compScores |>
      select(specificity, competency_id) |>
      left_join(
        competencies_db |> select(competency_id = id, competency = name),
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
      p(util_lookup[as.character(overallScores$utility)]),
      tags$b("SENTIMENT"),
      p(sent_lookup[as.character(overallScores$sentiment)]),
      tags$hr()
    )
  })

  # When the complete button is clicked
  observeEvent(input$complete, {
    if (
      (nrow(reviewScores()$compScores) == 0 ||
        is.na(reviewScores()$overallScores$utility) ||
        is.na(reviewScores()$overallScores$sentiment)) &
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
      id = input$reviewID,
      statusCode = ifelse(input$flag, -1, 2)
    )
    tbl_update(data, conn, "review_assignment", returnData = F)

    updateReviewID(input$reviewID)
    tabStatusIcon("comp", 2, session = session)
    tabStatusIcon("overall", 2, session = session)
    tabStatusIcon("submit", 2, session = session)
    showNotification("Changes marked as complete", type = "message")
  })

  #### ANALYSIS TAB ####

  # Populate the review dropdown
  reviewInfo <- tbl(conn, "review_assignment") |>
    left_join(
      tbl(conn, "reviewer") |> select(reviewer_id = id, human),
      by = "reviewer_id"
    ) |>
    group_by(evaluation_id, reviewer_id) |>
    filter(modified == max(modified)) |>
    group_by(evaluation_id) |>
    # filter(any(statusCode > 0)) |> # Add once out of dev
    summarise(
      nAI = n() - sum(human),
      nHuman = sum(human),
      nComplete = sum(statusCode %in% c(-1, 2, 5))
    ) |>
    ungroup() |>
    collect()

  updateSelectInput(
    session,
    "analysis_evalID",
    choices = setNames(
      reviewInfo$evaluation_id,
      sprintf(
        "%i - %i/%i completed",
        reviewInfo$evaluation_id,
        reviewInfo$nComplete,
        reviewInfo$nAI + reviewInfo$nHuman
      )
    )
  )

  output$analysis_evaluation <- renderUI({
    req(input$analysis_evalID)
    div(
      HTML(
        dbGetEvals(
          ids = as.integer(input$analysis_evalID),
          conn = conn,
          redacted = T,
          includeQuestions = T,
          html = T,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 40vh; overflow-y: auto;"
    )
  })

  analysisInfo <- reactive({
    overall <- tbl(conn, "review_assignment") |>
      filter(evaluation_id == as.integer(input$analysis_evalID)) |>
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

  comparisonTable <- reactive({
    info <- analysisInfo()

    # Build lookup structures from server-scope rubric data
    spec_lookup <- setNames(
      specificity_opts$description,
      as.integer(specificity_opts$value)
    )
    util_df <- data.frame(
      utility = as.integer(utility_opts$value),
      util = utility_opts$description
    )
    sent_df <- data.frame(
      sentiment = as.integer(sentiment_opts$value),
      sent = sentiment_opts$description
    )
    comp_metric <- data.frame(
      competency_id = competencies_db$id,
      metric = paste("COMPETENCY -", competencies_db$name)
    )

    bind_rows(
      info$compInfo |>
        select(reviewer, competency_id, specificity, note) |>
        mutate(
          specificity = spec_lookup[as.character(specificity)],
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
        select(utility, sentiment, note) |>
        mutate(
          note = ifelse(
            is.na(note),
            NA,
            sprintf("<i style='color:#e04233;'>%s<i>", note)
          )
        ) |>
        left_join(util_df, by = "utility") |>
        left_join(sent_df, by = "sentiment") |>
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
    }

    removeModal()
    if ("download" %in% input$pinAction) {
      showModal(modalDialog(mod_dbSetup_ui("dbMod", "link")))
    }
  })
}

shinyApp(ui, server)

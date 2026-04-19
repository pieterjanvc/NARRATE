# https://rstudio.github.io/bslib/articles/cards/index.html

dbInfo <- "../local/demo.db"
# dbInfo <- "../local/cfme.db"
dbInfo <- "~/Downloads/cfme.db"

# This is the db used during deployment, see deployShinyApp()
if (!file.exists(dbInfo)) {
  dbInfo <- "cfme.db"
  # These are the libraries that the app needs when deployed
  library(shiny)
  library(bslib)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(DT)
  library(sqlife)
  library(CFME)
} else {
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

  reviewScores <- reactiveVal()
  prompt <- reactiveVal()

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
        match(statusCode, c(1, 0, -1, 2, setdiff(c(0:2), unique(statusCode)))),
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
            statusCode == 2 ~ "Completed",
            statusCode == -1 ~ "Completed with flag",
            TRUE ~ "Error"
          )
        )
      )
    lblInfo <- reviews |>
      filter(statusCode < 3) |>
      group_by(statusCode) |>
      summarise(n = n())

    # Calculate each for status
    lblInfo <- data.frame(statusCode = -1:2) |>
      left_join(lblInfo, by = "statusCode") |>
      mutate(n = ifelse(is.na(n), 0, n)) |>
      pull(n)

    # Set the eval IDs
    updateSelectInput(
      session,
      "reviewID",
      label = sprintf(
        "%i to start - %i in progress - %i competed",
        lblInfo[2],
        lblInfo[3],
        lblInfo[4] + lblInfo[1]
      ),
      choices = setNames(
        reviews$id,
        reviews$descr
      ),
      selected = ifelse(missing(selected), reviews$id[1], selected)
    )
  }

  observeEvent(input$reviewerID, {
    updateReviewID()
  })

  # Add the AI review button when not a human
  output$aiReviewUI <- renderUI({
    reviewID <- as.integer(input$reviewID)

    # Check if the review is AI and new
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

    # Run the AI review
    llmReview <- llm_review(
      conn,
      review_assignment_id = as.integer(input$reviewID),
      log = "apiLog.csv",
      maxTries = 3
    )

    dbAIreview(conn, llmReview)

    # Set the review as finished (or flag if error)
    data <- data.frame(
      id = as.integer(input$reviewID),
      statusCode = ifelse(llmReview[[1]]$statusCode == 3, 2, -1)
    )
    tbl_update(data, conn, "review_assignment", returnData = F)

    updateReviewID(input$reviewID)
    tabStatusIcon("comp", 2, session = session)
    tabStatusIcon("overall", 2, session = session)
    tabStatusIcon("submit", 2, session = session)
    removeModal()
    forceRefresh(forceRefresh() + 1)
    showNotification("AI review complete", type = "message")
  })

  # Get the prompt and use it to create the rubric
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

      reviewStatus <- review_assingment$statusCode %in% c(-1, 2)

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

      # Check if the eval was already reviewed and use the same prompt version
      prompt_id <- review_assingment$prompt_id

      # Otherwise use the latest prompt version
      if (length(prompt_id) == 0) {
        prompt_id <- tbl(conn, "prompt") |>
          filter(id == max(id)) |>
          pull(id)
      }

      # Get the prompt text and parse it
      text <- tbl(conn, "prompt") |>
        filter(id == prompt_id) |>
        pull(prompt)
      parsed <- parsePrompt(text)

      # Update inputs based on rubric phrasing

      # Competency list
      updateSelectInput(
        inputId = "cID",
        choices = setNames(
          1:length(parsed$content$competencies),
          sapply(parsed$content$competencies, "[[", "name")
        )
      )

      # Specificity score
      #  The value is adjusted in the input$cID function
      updateRadioButtons(
        inputId = "specificity",
        label = parsed$content$compScore$specificity$desciption,
        choices = setNames(
          1:length(parsed$content$compScore$specificity$options),
          parsed$content$compScore$specificity$options
        ),
        selected = NULL
      )

      # Overall scores
      updateRadioButtons(
        inputId = "util",
        label = parsed$content$overallScore$util$desciption,
        choices = setNames(
          1:length(parsed$content$overallScore$util$options),
          parsed$content$overallScore$util$options
        ),
        selected = if (is.na(review_assingment$utility)) {
          character(0)
        } else {
          review_assingment$utility
        }
      )
      updateRadioButtons(
        inputId = "sent",
        label = parsed$content$overallScore$sent$desciption,
        choices = setNames(
          1:length(parsed$content$overallScore$sent$options),
          parsed$content$overallScore$sent$options
        ),
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
          review_assingment$statusCode %in% c(-1, 2),
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

      prompt(list(text = text, parsed = parsed))
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

  # This is the definition underneath the selected competency
  output$compDescr <- renderUI({
    tagList(tags$i(
      prompt()$parsed$content$competencies[[input$cID]]$description
    ))
  })

  # The UI that shows the evaluation
  output$evaluation <- renderUI({
    req(input$reviewID)
    # Get the evaluation ID for the assigned review
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
    # Check
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

    # Add prompt text to IDs and scores
    promptText <- prompt()$parsed$content

    compScores <- reviewScores()$compScores |>
      select(specificity, competency_id) |>
      left_join(
        data.frame(
          competency_id = 1:length(promptText$competencies),
          competency = sapply(promptText$competencies, "[[", "name")
        ),
        by = "competency_id"
      ) |>
      left_join(
        data.frame(
          specificity = 1:length(promptText$compScore$specificity$options),
          specificity_score = promptText$compScore$specificity$options
        ),
        by = "specificity"
      ) |>
      select(competency, score = specificity_score)

    overallScores <- reviewScores()$overallScores

    # ACTUAL UI
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
      p(promptText$overallScore$util$options[overallScores$utility]),
      tags$b("SENTIMENT"),
      p(promptText$overallScore$sent$options[overallScores$sentiment]),
      tags$hr()
    )
  })

  #When the complete button is clicked
  observeEvent(input$complete, {
    if (
      (nrow(reviewScores()$compScores) == 0 ||
        is.na(reviewScores()$overallScores$utility) ||
        is.na(reviewScores()$overallScores$sent)) &
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

  # Pupulate the review dropdown
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
      nComplete = sum(statusCode %in% c(-1, 2))
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

    prompt <- parsePrompt(
      tbl(conn, "prompt") |>
        filter(id == local(max(overall$prompt_id))) |>
        pull(prompt)
    )

    list(
      overall = overall,
      compInfo = compInfo,
      compText = compText,
      prompt = prompt
    )
  })

  comparisonTable <- reactive({
    # test <<- analysisInfo()
    prompt <- analysisInfo()$prompt$content
    bind_rows(
      analysisInfo()$compInfo |>
        select(reviewer, competency_id, specificity, note) |>
        mutate(
          specificity = prompt$compScore$specificity$options[specificity],
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
        left_join(
          data.frame(
            competency_id = 1:length(prompt$competencies),
            metric = paste(
              "COMPETENCY -",
              sapply(prompt$competencies, "[[", "name")
            )
          ),
          by = "competency_id"
        ) |>
        select(-competency_id),
      analysisInfo()$overall |>
        select(utility, sentiment, note) |>
        mutate(
          note = ifelse(
            is.na(note),
            NA,
            sprintf("<i style='color:#e04233;'>%s<i>", note)
          )
        ) |>
        left_join(
          data.frame(
            utility = 1:length(prompt$overallScore$util$options),
            util = prompt$overallScore$util$options
          ),
          by = "utility"
        ) |>
        left_join(
          data.frame(
            sentiment = 1:length(prompt$overallScore$sent$options),
            sent = prompt$overallScore$sent$options
          ),
          by = "sentiment"
        ) |>
        select(util, sent, note) |>
        t() |>
        as.data.frame() |>
        rename_with(function(x) {
          as.character(analysisInfo()$overall$reviewer)
        }) |>
        mutate(metric = c("UTILITY", "SENTIMENT", "REVIEW NOTE"))
    )
  })

  # input <- list(analysis_evalID = 660)
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
    # Check password
    if (
      Sys.getenv("adminPass") == "" ||
        Sys.getenv("adminPass") != input$adminPass
    ) {
      showNotification("Password incorrect or not activated", type = "error")
      return()
    }

    # Set pins if needed
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
    # Download if set
    if ("download" %in% input$pinAction) {
      showModal(modalDialog(mod_dbSetup_ui("dbMod", "link")))
    }
  })
}

shinyApp(ui, server)

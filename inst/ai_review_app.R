library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(sqlife)

dbInfo <- "../local/new_process.db"

# This is the db used during deployment, see deployShinyApp()
if (!file.exists(dbInfo)) {
  dbInfo <- "new_process.db"
  library(CFME)
} else {
  devtools::load_all()
}

# Competency names as defined in inst/prompt_comp_extract.md
competency_names <- c(
  "Medical Knowledge",
  "Medical History Taking and Physical Examination",
  "Provide Effective Oral and Written Professional Communication",
  "Clinical Reasoning and Decision Making",
  "Interpersonal and Communication Skills",
  "Scholarly Inquiry and Evidence-Based Medicine Integration",
  "Professionalism",
  "Interprofessional and Team-Based Care"
)

# Specificity score labels from inst/prompt_comp_score.md
specificity_labels <- c(
  "1 - General qualifiers only",
  "2 - Non-specific evidence",
  "3 - At least one specific example",
  "4 - Multiple or detailed examples"
)

# Utility score labels from inst/prompt_comp_score.md
utility_labels <- c(
  "1 - Low / not useful",
  "2 - Moderately useful",
  "3 - Highly useful"
)

# Sentiment score labels from inst/prompt_comp_score.md
sentiment_labels <- c(
  "1 - Clearly negative or red flags",
  "2 - Slightly negative or coded language",
  "3 - Not enough information",
  "4 - Generic positive language",
  "5 - Specific positive language"
)

# AI-completed status codes for review_assignment (statusCode %in% c(-1, 2, 5))
ai_complete_codes <- c(-1L, 2L, 5L)

ui <- page_fluid(
  theme = bs_theme(preset = "journal"),
  tags$style(HTML(
    "
    .control-label {
      font-weight: bold;
    }
    .html-fill-item {
      overflow: visible !important;
    }
  "
  )),

  # Top row: select a completed AI review
  layout_columns(
    col_widths = 12,
    card(
      card_header("AI Review Viewer"),
      selectInput(
        "reviewID",
        "Select completed AI review assignment",
        choices = c(),
        width = "100%"
      )
    )
  ),

  # Second row: evaluation text (left) + AI assessment cards (right)
  layout_columns(
    col_widths = c(8, 4),

    # Left: evaluation text preview
    card(
      card_header("Student Evaluation"),
      uiOutput("evaluation"),
      max_height = 500
    ),

    # Right: two vertically stacked cards
    tagList(
      card(
        card_header("Overall Scores"),
        uiOutput("overallScores")
      )
    )
  ),

  card(
    fill = FALSE,
    card_header("Competencies"),
    DTOutput("competencyTable")
  )
)

server <- function(input, output, session) {
  conn <- dbGetConn(dbInfo, session = session)

  # Populate the dropdown with AI-completed review assignments
  ai_reviews <- tbl(conn, "review_assignment") |>
    filter(statusCode %in% ai_complete_codes) |>
    inner_join(
      tbl(conn, "reviewer") |>
        filter(human == 0) |>
        select(reviewer_id = id, model),
      by = "reviewer_id"
    ) |>
    select(id, evaluation_id, statusCode, model) |>
    collect() |>
    mutate(
      status_label = case_when(
        statusCode == -1L ~ "Completed with flag",
        statusCode == 2L ~ "Completed",
        statusCode == 5L ~ "Batch scoring complete",
        TRUE ~ as.character(statusCode)
      ),
      label = sprintf(
        "Review %i (eval %i) - %s [%s]",
        id,
        evaluation_id,
        status_label,
        model
      )
    ) |>
    arrange(id)

  updateSelectInput(
    session,
    "reviewID",
    choices = setNames(ai_reviews$id, ai_reviews$label)
  )

  # Reactive: fetch all data for the selected review
  selected_review <- reactive({
    req(input$reviewID)
    rid <- as.integer(input$reviewID)

    assignment <- tbl(conn, "review_assignment") |>
      filter(id == rid) |>
      collect()

    comp_scores <- tbl(conn, "competency_score") |>
      filter(review_assignment_id == rid) |>
      collect()

    comp_text <- tbl(conn, "competency_text") |>
      filter(competency_score_id %in% local(comp_scores$id)) |>
      collect()

    list(
      assignment = assignment,
      comp_scores = comp_scores,
      comp_text = comp_text
    )
  })

  # Left panel: evaluation text
  output$evaluation <- renderUI({
    req(input$reviewID)
    rid <- as.integer(input$reviewID)

    eval_id <- tbl(conn, "review_assignment") |>
      filter(id == rid) |>
      pull(evaluation_id)

    div(
      HTML(
        dbGetEvals(
          ids = eval_id,
          conn = conn,
          redacted = TRUE,
          includeQuestions = TRUE,
          html = TRUE,
          subtitleTag = "b"
        ) |>
          pull(evaluation)
      ),
      style = "max-height: 70vh; overflow-y: auto;"
    )
  })

  # Right top card: competency table
  output$competencyTable <- renderDT({
    data <- selected_review()

    comp_text_summary <- data$comp_text |>
      left_join(
        data$comp_scores |> select(competency_score_id = id, competency_id),
        by = "competency_score_id"
      ) |>
      group_by(competency_id) |>
      summarise(text = paste(text_match, collapse = "; "), .groups = "drop")

    table_data <- data$comp_scores |>
      select(id = competency_id, specificity) |>
      left_join(comp_text_summary, by = c("id" = "competency_id")) |>
      mutate(
        name = competency_names[id],
        specificity = ifelse(
          is.na(specificity) | specificity < 1L | specificity > 4L,
          as.character(specificity),
          specificity_labels[specificity]
        )
      ) |>
      select(id, name, specificity, text) |>
      arrange(id)

    DT::datatable(
      table_data,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      ),
      selection = "none",
      rownames = FALSE,
      colnames = c("ID", "Competency", "Specificity", "Text Evidence")
    )
  })

  # Right bottom card: overall scores
  output$overallScores <- renderUI({
    a <- selected_review()$assignment

    utility_val <- a$utility
    sentiment_val <- a$sentiment

    utility_txt <- if (
      !is.na(utility_val) && utility_val >= 1L && utility_val <= 3L
    ) {
      utility_labels[utility_val]
    } else {
      paste("Score:", utility_val)
    }

    sentiment_txt <- if (
      !is.na(sentiment_val) && sentiment_val >= 1L && sentiment_val <= 5L
    ) {
      sentiment_labels[sentiment_val]
    } else {
      paste("Score:", sentiment_val)
    }

    tagList(
      tags$b("Utility"),
      tags$p(utility_txt),
      tags$b("Sentiment"),
      tags$p(sentiment_txt)
    )
  })
}

shinyApp(ui, server)

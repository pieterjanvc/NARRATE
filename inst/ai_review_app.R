library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(sqlife)

# dbInfo <- "../local/clean_up.db"
dbInfo <- "../local/narrate-ai.db"

# This is the db used during deployment, see deployShinyApp()
if (!file.exists(dbInfo)) {
  dbInfo <- "../narrate.db"
  library(NARRATE)
} else {
  devtools::load_all()
}

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
      div(DTOutput("reviewsTable"))
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
    card_header("Competencies"),
    div(DTOutput("competencyTable"))
  )
)

server <- function(input, output, session) {
  conn <- dbGetConn(dbInfo, session = session)

  # Weights for quality score: coverage 35%, avg specificity 45%, utility 20%
  w_c <- 0.35
  w_s <- 0.45
  w_u <- 0.20

  comp_summary_all <- tbl(conn, "competency_score") |>
    filter(!is.na(specificity)) |>
    group_by(review_assignment_id) |>
    summarise(
      n_competencies = n(),
      avg_specificity = mean(specificity, na.rm = TRUE),
      .groups = "drop"
    )

  # Rubric content (competency count, specificity/utility value ranges) is
  # versioned in the DB and can differ between rubric_id's, so normalization
  # ranges are looked up per rubric rather than assumed fixed.
  rubric_n_competencies <- tbl(conn, "rubric_competency") |>
    count(rubric_id, name = "n_total_competencies")

  rubric_specificity_range <- tbl(conn, "rubric_specificity") |>
    left_join(
      tbl(conn, "specificity") |> select(specificity_id = id, value),
      by = "specificity_id"
    ) |>
    group_by(rubric_id) |>
    summarise(
      spec_min = min(value, na.rm = TRUE),
      spec_max = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  rubric_utility_range <- tbl(conn, "rubric_utility") |>
    left_join(
      tbl(conn, "utility") |> select(utility_id = id, value),
      by = "utility_id"
    ) |>
    group_by(rubric_id) |>
    summarise(
      util_min = min(value, na.rm = TRUE),
      util_max = max(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Populate the dropdown with AI-completed review assignments
  ai_reviews <<- tbl(conn, "review_assignment") |>
    filter(statusCode %in% ai_complete_codes) |>
    inner_join(
      tbl(conn, "reviewer") |>
        filter(human == 0) |>
        select(reviewer_id = id, model),
      by = "reviewer_id"
    ) |>
    left_join(comp_summary_all, by = c("id" = "review_assignment_id")) |>
    left_join(rubric_n_competencies, by = "rubric_id") |>
    left_join(rubric_specificity_range, by = "rubric_id") |>
    left_join(rubric_utility_range, by = "rubric_id") |>
    left_join(
      tbl(conn, "evaluation") |>
        select(evaluation_id = id, evaluator_id, summary_flg, rotation_id),
      by = "evaluation_id"
    ) |>
    left_join(
      tbl(conn, "evaluator") |> select(evaluator_id = id, evaluator),
      by = "evaluator_id"
    ) |>
    left_join(
      tbl(conn, "rotation") |> select(rotation_id = id, rotation_date),
      by = "rotation_id"
    ) |>
    select(
      id,
      evaluation_id,
      evaluator,
      summary_flg,
      rotation_date,
      utility = utility_score_value,
      n_competencies,
      avg_specificity,
      n_total_competencies,
      spec_min,
      spec_max,
      util_min,
      util_max
    ) |>
    collect() |>
    mutate(
      n_competencies = ifelse(is.na(n_competencies), 0L, n_competencies),
      coverage_norm = n_competencies / n_total_competencies,
      specificity_norm = ifelse(
        n_competencies > 0,
        (avg_specificity - spec_min) / (spec_max - spec_min),
        0
      ),
      utility_norm = ifelse(
        !is.na(utility),
        (utility - util_min) / (util_max - util_min),
        0
      ),
      total_score = round(
        (w_c * coverage_norm + w_s * specificity_norm + w_u * utility_norm) *
          100,
        1
      )
    ) |>
    arrange(desc(total_score))

  random_sel <- sample(1:nrow(ai_reviews), 100) |> sort()
  display_reviews <- ai_reviews[random_sel, ]

  # Selection table: one row per AI review assignment. input$reviewsTable_rows_selected
  # refers to display_reviews' row order regardless of any client-side sort/search.
  output$reviewsTable <- renderDT({
    datatable(
      display_reviews |>
        mutate(Summary = ifelse(summary_flg == 1L, "Yes", "No")) |>
        select(
          Score = total_score,
          Evaluator = evaluator,
          Summary,
          `Evaluation ID` = evaluation_id,
          `Rotation Date` = rotation_date
        ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, order = list(list(0, "desc")))
    )
  })

  # Reactive: the review_assignment id behind the currently selected table row
  rid_selected <- reactive({
    req(input$reviewsTable_rows_selected)
    display_reviews$id[input$reviewsTable_rows_selected]
  })

  # Reactive: fetch all data for the selected review, including the
  # competency names/order and score-level descriptions for the specific
  # rubric this assignment was scored against.
  selected_review <- reactive({
    rid <- rid_selected()

    assignment <- tbl(conn, "review_assignment") |>
      filter(id == rid) |>
      collect()

    comp_scores <- tbl(conn, "competency_score") |>
      filter(review_assignment_id == rid) |>
      collect()

    comp_text <- tbl(conn, "competency_text") |>
      filter(competency_score_id %in% local(comp_scores$id)) |>
      collect()

    rubric_id <- assignment$rubric_id

    competency_map <- tbl(conn, "rubric_competency") |>
      filter(rubric_id == local(rubric_id)) |>
      select(competency_id, comp_order = order) |>
      left_join(
        tbl(conn, "competency") |> select(competency_id = id, name),
        by = "competency_id"
      ) |>
      collect()

    specificity_map <- tbl(conn, "rubric_specificity") |>
      filter(rubric_id == local(rubric_id)) |>
      select(specificity_id) |>
      left_join(
        tbl(conn, "specificity") |>
          select(specificity_id = id, value, description),
        by = "specificity_id"
      ) |>
      collect()

    utility_map <- tbl(conn, "rubric_utility") |>
      filter(rubric_id == local(rubric_id)) |>
      select(utility_id) |>
      left_join(
        tbl(conn, "utility") |> select(utility_id = id, value, description),
        by = "utility_id"
      ) |>
      collect()

    sentiment_map <- tbl(conn, "rubric_sentiment") |>
      filter(rubric_id == local(rubric_id)) |>
      select(sentiment_id) |>
      left_join(
        tbl(conn, "sentiment") |> select(sentiment_id = id, value, description),
        by = "sentiment_id"
      ) |>
      collect()

    list(
      assignment = assignment,
      comp_scores = comp_scores,
      comp_text = comp_text,
      competency_map = competency_map,
      specificity_map = specificity_map,
      utility_map = utility_map,
      sentiment_map = sentiment_map
    )
  })

  # Left panel: evaluation text
  output$evaluation <- renderUI({
    rid <- rid_selected()

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

    spec_desc <- setNames(
      data$specificity_map$description,
      data$specificity_map$value
    )

    table_data <- data$comp_scores |>
      select(competency_id, specificity) |>
      left_join(data$competency_map, by = "competency_id") |>
      left_join(comp_text_summary, by = "competency_id") |>
      mutate(
        specificity = ifelse(
          is.na(specificity) | !as.character(specificity) %in% names(spec_desc),
          as.character(specificity),
          spec_desc[as.character(specificity)]
        )
      ) |>
      arrange(comp_order) |>
      select(id = competency_id, name, specificity, text)

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
    data <- selected_review()
    a <- data$assignment
    rid <- rid_selected()

    utility_val <- a$utility_score_value
    sentiment_val <- a$sentiment_score_value
    score_row <- ai_reviews[ai_reviews$id == rid, ]

    util_desc <- setNames(data$utility_map$description, data$utility_map$value)
    sent_desc <- setNames(
      data$sentiment_map$description,
      data$sentiment_map$value
    )

    utility_txt <- if (
      !is.na(utility_val) && as.character(utility_val) %in% names(util_desc)
    ) {
      sprintf("%d - %s", utility_val, util_desc[[as.character(utility_val)]])
    } else {
      paste("Score:", utility_val)
    }

    sentiment_txt <- if (
      !is.na(sentiment_val) && as.character(sentiment_val) %in% names(sent_desc)
    ) {
      sprintf(
        "%d - %s",
        sentiment_val,
        sent_desc[[as.character(sentiment_val)]]
      )
    } else {
      paste("Score:", sentiment_val)
    }

    tagList(
      tags$b("Utility"),
      tags$p(utility_txt),
      tags$b("Sentiment"),
      tags$p(sentiment_txt),
      tags$hr(),
      tags$b("Quality Score"),
      tags$table(
        class = "table table-sm mt-1",
        tags$tbody(
          tags$tr(
            tags$td("Coverage (N competencies)"),
            tags$td(sprintf(
              "%d / %d  \u2192  %.0f%%",
              score_row$n_competencies,
              score_row$n_total_competencies,
              score_row$coverage_norm * 100
            ))
          ),
          tags$tr(
            tags$td("Avg Specificity"),
            tags$td(sprintf(
              "%.2f / %d  \u2192  %.0f%%",
              ifelse(
                is.na(score_row$avg_specificity),
                0,
                score_row$avg_specificity
              ),
              score_row$spec_max,
              score_row$specificity_norm * 100
            ))
          ),
          tags$tr(
            tags$td("Utility"),
            tags$td(sprintf(
              "%d / %d  \u2192  %.0f%%",
              utility_val,
              score_row$util_max,
              score_row$utility_norm * 100
            ))
          ),
          tags$tr(
            tags$th("Total Score (35 / 45 / 20 weights)"),
            tags$th(sprintf("%.1f / 100", score_row$total_score))
          )
        )
      )
    )
  })
}

shinyApp(ui, server)

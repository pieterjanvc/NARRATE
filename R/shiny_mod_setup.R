#' Module UI setup tab
#'
#' @param id Module ID
#'
#' @import shiny
#' @importFrom DT DTOutput renderDT
#'
#' @returns Shiny UI element
#' @export
#'
mod_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(DTOutput(ns("evalsTable")))
}

#' Module server setup tab
#'
#' @param id Module ID
#' @param conn connection to the database
#'
#' @import shiny dplyr
#'
#' @returns trigger if there is an update
#'
#' @export
#'
mod_setup_server <- function(id, conn) {
  evalOverview <- function(conn) {
    tbl(conn, "evaluation") |> collect()
  }

  moduleServer(id, function(input, output, session) {
    output$evalsTable <- renderDT(
      {
        tbl(conn, "evaluation") |> collect()
      },
      server = T
    )
    return()
  })
}

library(shiny)

ui <- fluidPage(
  mod_setup_ui("dev")
)

server <- function(input, output, session) {
  dbInfo <- "../local/narrate.db"
  conn <- dbGetConn(dbInfo, session = session)
  x <- mod_setup_server("dev", conn)
  observe({
    print(x)
  })
}

shinyApp(ui, server)

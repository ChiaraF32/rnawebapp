#' test UI Function
#'
#' @description A shiny Module for testing PanelApp phenotype dropdowns.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectizeInput

mod_test_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("select_phenotype"),
                   "Choose Phenotype from PanelApp",
                   choices = NULL,
                   multiple = TRUE),
    verbatimTextOutput(ns("selected"))
  )
}

#' test Server Function
#'
#' @noRd
mod_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(TRUE, {
      panels <- fetch_all_panels("uk")
      ids <- as.character(panels$id)
      names_clean <- trimws(as.character(panels$name))
      updateSelectizeInput(session, "select_phenotype",
                           choices = setNames(ids, names_clean),
                           server = TRUE)
    }, once = TRUE)

    output$selected <- renderPrint({
      input$select_phenotype
    })
  })
}




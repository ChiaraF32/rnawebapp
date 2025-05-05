#' parameters UI Function
#'
#' @description A shiny Module for selecting parameters for analysis of RNAseq data
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags uiOutput actionButton checkboxGroupInput
mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Parameters"),
    util_page_layout(
      nav_id = ns("nav_buttons"),
      home_id = ns("home_btn"),
      tags$div(
        style = "text-align:left; padding: 30px;",
        tags$h1("Choose Analysis Parameters"),
        checkboxGroupInput(ns("analysis_type"), "Analysis Type", selected = NULL, choices = c("Individual", "Cohort")),
        uiOutput(ns("sample_selection")),
        checkboxGroupInput(ns("results"), "Results to Display", selected = NULL, choices = c("Aberrant Expression", "Aberrant Splicing", "RNA Variant Calls", "RNA Fusions / SVs")),
        uiOutput(ns("alignment_upload"))
      )
    )
  )
}

#' parameters Server Functions
#'
#' @noRd
#'
#' @importFrom shiny NS renderUI selectInput fileInput
mod_parameters_server <- function(id, go_to_individual_res, go_to_cohort_res, go_to_processing, go_to_index, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    go_to_results <- function() {
      if ("Individual" %in% input$analysis_type) {
        go_to_individual_res()
      } else {
        go_to_cohort_res()
      }
    }

    #Buttons
    mod_nav_buttons_server("nav_buttons", next_page = go_to_results, previous_page = go_to_processing)
    mod_home_button_server("home_btn", go_to_index = go_to_index)

    # Conditional sample selection
    output$sample_selection <- renderUI({
      if ("Individual" %in% input$analysis_type) {
        selectInput(ns("samples"), "Select Samples", choices = c("gene1", "gene2", "gene3"), multiple = TRUE)
      } else {
        NULL
      }
    })

    # Condition Alignment Upload
    output$alignment_upload <- renderUI({
      if ("Individual" %in% input$analysis_type) {
        fileInput(ns("alignment_file"), "Upload Alignment File", multiple = TRUE)
      } else {
        NULL
      }
    })
  })
}


## To be copied in the UI
# mod_parameters_ui("parameters_1")

## To be copied in the server
# mod_parameters_server("parameters_1")

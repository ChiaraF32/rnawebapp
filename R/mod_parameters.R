#' parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags uiOutput actionButton checkboxGroupInput
mod_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "text-align:left; padding: 50px;",
      tags$h1("Choose Analysis Parameters"),
      checkboxGroupInput(ns("analysis_type"), "Analysis Type", selected = NULL, choices = c("Individual", "Cohort")),
      uiOutput(ns("sample_selection")),
      checkboxGroupInput(ns("results"), "Results to Display", selected = NULL, choices = c("Aberrant Expression", "Aberrant Splicing", "RNA Variant Calls", "RNA Fusions / SVs")),
      uiOutput(ns("alignment_upload")),
      tags$br(),
      actionButton(ns("proceed"), "Proceed to Results", class = "btn btn-success")
    )
  )
}

#' parameters Server Functions
#'
#' @noRd
#'
#' @importFrom shiny NS renderUI selectInput fileInput
mod_parameters_server <- function(id, go_to_individual_res, go_to_cohort_res, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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

    observeEvent(input$proceed, {
      showNotification("Proceeding to data results")
      if ("Individual" %in% input$analysis_type) {
        go_to_individual_res()
      } else {
        go_to_cohort_res()
      }
    })
  })
}


## To be copied in the UI
# mod_parameters_ui("parameters_1")

## To be copied in the server
# mod_parameters_server("parameters_1")

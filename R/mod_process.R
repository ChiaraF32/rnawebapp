#' process UI Function
#'
#' @description A shiny Module for processing the uploaded data and rendering summaries of that data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        tags$div(
          style = "text-align:left; padding: 30px;",
          tags$h1("Data Processing Progress"),

          uiOutput(ns("check_samples_ui")),
          tags$br(),
          uiOutput(ns("convert_genes_ui")),
          tags$br(),
          uiOutput(ns("check_missing_ui")),
          tags$br(),
          uiOutput(ns("complete_ui"))
        )
      ),
      column(
        width = 8,
        # Later - processing output
      )
    )
  )
}

#' process Server Functions
#'
#' @noRd
mod_process_server <- function(id, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Simulate processing steps (replace with real logic)
    processing_state <- reactiveVal("start")

    output$check_samples_ui <- renderUI({
      if (processing_state() %in% c("samples_checked", "genes_converted", "done")) {
        tags$p("✅ Checking sample identifiers... Done")
      } else {
        tags$p("⏳ Checking sample identifiers...")
      }
    })

    output$convert_genes_ui <- renderUI({
      if (processing_state() %in% c("genes_converted", "done")) {
        tags$p("✅ Converting gene names... Done")
      } else if (processing_state() == "samples_checked") {
        tags$p("⏳ Converting gene names...")
      } else {
        tags$p("⏹️ Converting gene names... Waiting")
      }
    })

    output$check_missing_ui <- renderUI({
      if (processing_state() == "done") {
        tags$p("✅ Checking for missing data... Done")
      } else if (processing_state() == "genes_converted") {
        tags$p("⏳ Checking for missing data...")
      } else {
        tags$p("⏹️ Checking for missing data... Waiting")
      }
    })

    output$complete_ui <- renderUI({
      if (processing_state() == "done") {
        tags$p("🎉 Data Processing Complete!")
      }
    })

    # Simulated processing sequence — replace with actual logic
    observe({
      # Step 1
      Sys.sleep(1)
      processing_state("samples_checked")

      # Step 2
      Sys.sleep(1)
      processing_state("genes_converted")

      # Step 3
      Sys.sleep(1)
      processing_state("done")
    })
  })
}

## To be copied in the UI
# mod_process_ui("process_1")

## To be copied in the server
# mod_process_server("process_1")

#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags fileInput actionButton br
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "padding: 30px;",
      tags$h2("Upload Your Data"),

      fileInput(ns("samplesheet"), "1. Upload Samplesheet"),
      fileInput(ns("outrider"), "2. Upload OUTRIDER Dataset"),
      fileInput(ns("fraser"), "3. Upload FRASER Dataset"),
      fileInput(ns("vcf"), "5. Upload RNA Variant Calls"),
      fileInput(ns("fusions"), "5. Upload RNA SV/Fusion Calls"),

      tags$br(),
      actionButton(ns("proceed"), "Proceed to Data Processing", class = "btn btn-success")
    )

  )
}

#' upload Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent observe req showNotification
#' @importFrom utils read.csv
mod_upload_server <- function(id, go_to_processing, uploaded_data){
  moduleServer(id, function(input, output, session){

    observeEvent(input$samplesheet, {
      req(input$samplesheet)
      uploaded_data$samplesheet <- read.csv(input$samplesheet$datapath)
    })

    observeEvent(input$proceed, {
      showNotification("Proceeding to data processing...")
      go_to_processing()
    })

    observeEvent(input$outrider, {
      req(input$outrider)
      uploaded_data$outrider <- readRDS(input$outrider$datapath)
    })

    observeEvent(input$fraser, {
      req(input$fraser)
      uploaded_data$fraser <- readRDS(input$fraser$datapath)
    })

    # Optional: observe file uploads for debug or processing
    observe({
      req(input$samplesheet)
      message("Samplesheet uploaded: ", input$samplesheet$name)
    })

    observe({
      req(input$outrider)
      message("OUTRIDER uploaded: ", input$outrider$name)
    })

    observe({
      req(input$fraser)
      message("FRASER uploaded: ", input$fraser$name)
    })

    # Repeat for other inputs if needed
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")

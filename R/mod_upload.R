#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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

      br(),
      actionButton(ns("proceed"), "Proceed to Data Processing", class = "btn btn-success")
    )

  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id){
  moduleServer(id, function(input, output, session){

    observeEvent(input$proceed, {
      showNotification("Proceeding to data processing...")
      # You can later route to another module here
    })

    # Optional: observe file uploads for debug or processing
    observe({
      req(input$samplesheet)
      message("Samplesheet uploaded: ", input$samplesheet$name)
    })

    # Repeat for other inputs if needed
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")

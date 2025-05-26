#' upload UI Function
#'
#' @description Shiny module for uploading data
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags fileInput
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Upload"),
    util_page_layout(
      nav_id = ns("nav_buttons"),
      home_id = ns("home_btn"),

      tags$h2("Upload Your Data"),

      fileInput(ns("samplesheet"), "1. Upload Samplesheet"),
      fileInput(ns("outrider"), "2. Upload OUTRIDER Dataset"),
      fileInput(ns("fraser"), "3. Upload FRASER Dataset"),
      fileInput(ns("vcf"), "4. Upload RNA Variant Calls"),
      fileInput(ns("fusions"), "5. Upload RNA SV/Fusion Calls"),
      textInput(ns("fraser_dir"), "6. Specify Path to FRASER Datasets", placeholder = "./savedObjects/MUSCLE--v38/")
      )
  )
}

#' upload Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent observe req showNotification
#' @importFrom utils read.csv
mod_upload_server <- function(id, go_to_processing, go_to_index, uploaded_data){
  moduleServer(id, function(input, output, session){

    mod_nav_buttons_server("nav_buttons", next_page = go_to_processing, previous_page = go_to_index)
    mod_home_button_server("home_btn", go_to_index = go_to_index)

    observeEvent(input$samplesheet, {
      req(input$samplesheet)
      uploaded_data$samplesheet <- readr::read_tsv(input$samplesheet$datapath, col_types = readr::cols())
    })

    observeEvent(input$outrider, {
      req(input$outrider)
      uploaded_data$outrider <- readRDS(input$outrider$datapath)
    })

    observeEvent(input$fraser, {
      req(input$fraser)
      uploaded_data$fraser <- readRDS(input$fraser$datapath)
    })

    observeEvent(input$fraser_dir, {
      req(input$fraser_dir)
      uploaded_data$fraser_dir <- input$fraser_dir
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
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")

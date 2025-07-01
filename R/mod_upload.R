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
      fileInput(ns("rna_vcf"), "3. Upload RNA Variant Calls"),
      textInput(ns("fraser_dir"), "4. Specify Path to FRASER Working Directory", placeholder = "~/RNAwebapp/data/"),
      textInput(ns("analysis_name"), "5. Specify Analysis Name of Project", placeholder = "MUSCLE--v113"),
      actionButton(ns("load_fraser"), "6. Load FRASER Dataset", class = "btn btn-primary")
    )
  )
}

#' upload Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent observe req showNotification
#' @importFrom utils read.csv
#' @importFrom FRASER loadFraserDataSet
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

    observeEvent(input$fraser_dir, {
      req(input$fraser_dir, input$analysis_name)
      uploaded_data$fraser <- loadFraserDataSet(dir = input$fraser_dir, name = input$analysis_name)
    })

    observeEvent(input$rna_vcf, {
      req(input$rna_vcf)
      uploaded_data$rna_vcf <- readRDS(input$rna_vcf$datapath)
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

    observeEvent(input$load_fraser, {
      req(input$fraser_dir, input$analysis_name)
      tryCatch({
        uploaded_data$fraser <- loadFraserDataSet(dir = input$fraser_dir, name = input$analysis_name)
        shiny::showNotification("✅ FRASER dataset loaded", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("❌ Failed to load FRASER dataset:", e$message), type = "error")
      })
    })
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")

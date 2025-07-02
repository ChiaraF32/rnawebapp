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

      fluidRow(
        column(
          width = 6,
          tags$div(
            style = "text-align:left; padding: 30px;",
            tags$h2("Upload Data"),
            fileInput(ns("samplesheet"), "Upload Samplesheet", accept = c(".csv", ".tsv")),
            fileInput(ns("outrider"), "Upload OUTRIDER Dataset", accept = ".Rds"),
            fileInput(ns("rna_vcf"), "Upload RNA Variant Calls", accept = c(".Rds", ".vcf")),
            textInput(ns("fraser_dir"), "Specify Path to FRASER Working Directory", placeholder = "~/RNAwebapp/data/"),
            textInput(ns("analysis_name"), "Specify Analysis Name of Project", placeholder = "MUSCLE--v113"),
            actionButton(ns("load_fraser"), "Load FRASER Dataset", class = "btn btn-primary")
          )
        ),
        column(
          width = 6,
          tags$div(
            style = "text-align:left; padding: 30px;",
            tags$h2("Check Samples"),
            actionButton(ns("check_samples_btn"), "Check for sample mismatches", class = "btn btn-primary"),
            uiOutput(ns("check_samples"))
          )
        )
      )
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

    observeEvent(input$check_samples_btn, {
      req(input$samplesheet, uploaded_data$outrider, uploaded_data$fraser)
      output$check_samples <- renderUI({
          ods<- uploaded_data$outrider
          fds <- uploaded_data$fraser
          compare_samples(ods, fds)
        })
      }
    )
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")

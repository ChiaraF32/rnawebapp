#' process UI Function
#'
#' @description A shiny Module for processing the uploaded data and rendering summaries of that data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column tags uiOutput plotOutput
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
        width = 4,
        tags$div(
          style = "padding: 30px",
          tags$h1("Data Summary"),
          DT::DTOutput(ns("data_summary"))
        )
      ),
      column(
        width = 4,
        tags$div(
          style = "padding: 30px",
          tags$h1("Phenotype"),
          plotOutput(ns("phenotype_plot"))
        )
      )
    )
  )
}

#' process Server Functions
#'
#' @noRd
#' @importFrom shiny reactiveVal renderUI renderPlot observeEvent observe req tags
#' @importFrom shinipsum random_DT random_ggplot
mod_process_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track which step is complete
    processing_state <- reactiveVal("start")  # start → samples_checked → genes_converted → done

    # --- Progress UI Renderers ---
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

    output$data_summary <- DT::renderDT({
      if (processing_state() != "done") {
        return(data.frame(Message = "Processing not yet complete."))
      }
      shinipsum::random_DT(nrow = 5, ncol = 2)
    })

    output$phenotype_plot <- renderPlot({
      req(processing_state() == "done")
      shinipsum::random_ggplot("bar")
    })

    # --- Observe uploaded_data and trigger steps ---

    observe({
      req(uploaded_data$samplesheet, uploaded_data$outrider, uploaded_data$fraser)
      # Your validation logic here
      message("✅ All inputs detected. Starting Step 1...")
      Sys.sleep(1)  # simulate processing time
      processing_state("samples_checked")
    })

    observeEvent(processing_state(), {
      req(uploaded_data$outrider, uploaded_data$fraser)
      if (processing_state() == "samples_checked") {
        # Step 2: Convert gene names (e.g., Ensembl → Symbols)
        # Your conversion logic here
        Sys.sleep(1)
        processing_state("genes_converted")
      }
    }, ignoreInit = TRUE)

    observeEvent(processing_state(), {
      if (processing_state() == "genes_converted") {
        # Step 3: Check for missing data
        # Your QC logic here
        Sys.sleep(1)
        processing_state("done")
      }
    }, ignoreInit = TRUE)
  })
}

## To be copied in the UI
# mod_process_ui("process_1")

## To be copied in the server
# mod_process_server("process_1")

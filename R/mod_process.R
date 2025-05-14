#' process UI Function
#'
#' @description A shiny Module for processing the uploaded data and rendering summaries of that data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column tags uiOutput plotOutput
#' @importFrom DT DTOutput
mod_process_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Processing"),
    util_page_layout(
      nav_id = ns("nav_buttons"),
      home_id = ns("home_btn"),

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
            uiOutput(ns("fix_bam_paths_ui")),
            tags$br(),
            uiOutput(ns("fix_fraser_paths_ui")),
            tags$br(),
            uiOutput(ns("calculate_results_ui")),
            tags$br(),
            uiOutput(ns("annotate_results_ui")),
            tags$br(),
            uiOutput(ns("merge_results")),
            tags$br(),
            uiOutput(ns("complete_ui"))
          )
        ),
        column(
          width = 4,
          tags$div(
            style = "padding: 30px",
            tags$h1("Data Summary"),
            DTOutput(ns("data_summary"))
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
  )
}

#' process Server Functions
#'
#' @noRd
#' @importFrom shiny reactiveVal renderUI renderPlot observeEvent observe req tags
#' @importFrom shinipsum random_DT random_ggplot
#' @importFrom DT renderDT
#' @importFrom later later
mod_process_server <- function(id, go_to_parameters, go_to_upload, go_to_index, uploaded_data, processed_data, current_page) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive values
    summary_table <- reactiveVal(data.frame())
    phenotype_chart <- reactiveVal()
    results <- reactiveVal(list())
    processing_state <- reactiveVal("start")  # start → samples_checked → genes_converted → bams_corrected → fraser_corrected → results_calculated → done

    #Buttons
    mod_nav_buttons_server("nav_buttons", next_page = go_to_parameters, previous_page = go_to_upload)
    mod_home_button_server("home_btn", go_to_index = go_to_index)

    observeEvent(current_page(), {
      req(current_page() == ROUTES$PROCESSING)

      message("📍 Navigated to Processing page — starting processing.")

      samplesheet <- uploaded_data$samplesheet
      outrider <- uploaded_data$outrider
      fraser <- uploaded_data$fraser

      later::later(function() {
        if (!is.null(samplesheet) &&
            !is.null(outrider) &&
            !is.null(fraser)) {

          message("✅ All inputs detected. Starting Step 1...")

          summary_table(summarise_data(outrider, fraser))
          phenotype_chart(plot_phenotype_distribution(samplesheet))
          processing_state("samples_checked")
        } else {
          showNotification("Missing input data!", type = "error")
        }
      }, delay = 0.1)
    })

    # --- Progress UI Renderer ---
    output$check_samples_ui <- renderUI({
      render_step_ui(
        step_name = "Checking sample identifiers",
        completed_states = c("samples_checked", "genes_converted", "bams_corrected", "fraser_corrected", "results_calculated", "results_annotated", "done"),
        current_state = processing_state()
        )
      })

    output$convert_genes_ui <- renderUI({
      render_step_ui(
        step_name = "Converting gene names",
        completed_states = c("genes_converted", "bams_corrected", "fraser_corrected", "results_calculated", "results_annotated", "done"),
        current_state = processing_state()
      )
    })

    output$fix_bam_paths_ui <- renderUI({
      render_step_ui(
        step_name = "Correcting BAM paths",
        completed_states = c("bams_corrected", "fraser_corrected", "results_calculated", "results_annotated", "done"),
        current_state = processing_state()
      )
    })

    output$fix_fraser_paths_ui <- renderUI({
      render_step_ui(
        step_name = "Correcting FRASER paths",
        completed_states = c("fraser_corrected", "results_calculated", "results_annotated", "done"),
        current_state = processing_state()
      )
    })

    output$calculate_results_ui <- renderUI({
      render_step_ui(
        step_name = "OUTRIDER & FRASER Results Calculated",
        completed_states = c("results_calculated", "results_annotated", "done"),
        current_state = processing_state()
      )
    })

    output$annotate_results_ui <- renderUI({
      render_step_ui(
        step_name = "OUTRIDER & FRASER Results Annotated",
        completed_states = c("results_annotated", "done"),
        current_state = processing_state()
      )
    })

    output$merge_results <- renderUI({
      render_step_ui(
        step_name = "Merging OUTRIDER and FRASER Results",
        completed_states = c("done"),
        current_state = processing_state()
      )
    })

    output$complete_ui <- renderUI({
      if (processing_state() == "done") {
        tags$p("🎉 Data Processing Complete!")
      }
    })

    output$data_summary <- renderDT({
      if (processing_state() != "done") {
        return(data.frame(Message = "Processing not yet complete."))
      }
      summary_table()
    })

    output$phenotype_plot <- renderPlot({
      req(processing_state() == "done")
      req(phenotype_chart())
      phenotype_chart()
    })

    # --- Observe uploaded_data and trigger steps ---

    ## change ensembl id to hgnc for outrider
    observeEvent(processing_state(), {
      req(uploaded_data$outrider, uploaded_data$fraser)
      if (processing_state() == "samples_checked") {

        outrider <- uploaded_data$outrider

        #add delay function
        later::later(function() {
          # Annotate Ensembl IDs with HGNC
          annotated_ods <- annotate_ensembl_ids(outrider)

          # Save back into uploaded_data
          processed_data$outrider <- annotated_ods

          # Update processing state
          processing_state("genes_converted")
        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)

    ## change the BAM paths
    observeEvent(processing_state(), {
      req(processed_data$outrider, uploaded_data$fraser, uploaded_data$bam_dir)
      if (processing_state() == "genes_converted") {

        outrider <- processed_data$outrider
        fraser <- uploaded_data$fraser
        bam_dir <- uploaded_data$bam_dir

        later::later(function() {

          #Fix paths to BAM files
          outrider_fixed <- update_bam_paths(outrider, bam_dir)
          fraser_fixed <- update_bam_paths(fraser, bam_dir)

          # Save back to uploaded date
          processed_data$outrider <- outrider_fixed
          processed_data$fraser <- fraser_fixed

          # Update processing state
          processing_state("bams_corrected")
        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)

    ## change the fraser paths
    observeEvent(processing_state(), {
      req(processed_data$fraser)
      if (processing_state() == "bams_corrected") {

        fraser <- processed_data$fraser

        later::later(function() {

          # Fix paths to fraser objects
          fds_fixed <- fixFdsH5Paths(fraser)

          # Save back to uploaded data
          processed_data$fraser <- fds_fixed

          # Update processing state
          processing_state("fraser_corrected")
        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)

    ## calculate results
    observeEvent(processing_state(), {
      req(processed_data$outrider, processed_data$fraser)
      if (processing_state() == "fraser_corrected") {

        outrider <- processed_data$outrider
        fraser <- processed_data$fraser

        later::later(function() {

          # Calculate fraser and outrider results
          results(generate_results(outrider, fraser))
          processing_state("results_calculated")
        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)

    observeEvent(processing_state(), {
      if (processing_state() == "results_calculated") {

        res <- results()

        later::later(function() {

          # Annotate fraser and outrider results
          fres_annotated <- annotate_results_with_omim_go(
            results = res$frares,
            add_omim = TRUE,
            add_go = FALSE
          )

          # OUTRIDER example (with same gene list)
          ores_annotated <- annotate_results_with_omim_go(
            results = res$outres,
            add_omim = TRUE,
            add_go = FALSE
          )

          processed_data$annotated_results <- list(frares = fres_annotated, outres = ores_annotated)

          # Update processing state
          processing_state("results_annotated")
        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)

    observeEvent(processing_state(), {
      if (processing_state() == "results_annotated") {

        res <- processed_data$annotated_results

        later::later(function() {
          merged <- merge_outrider_fraser(res$outres, res$frares)
          processed_data$merged <- merged
          processing_state("done")

        }, delay = 0.1)
      }
    }, ignoreInit = TRUE)
  })
}

## To be copied in the UI
# mod_process_ui("process_1")

## To be copied in the server
# mod_process_server("process_1")

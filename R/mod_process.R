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
mod_process_server <- function(id, go_to_parameters, go_to_upload, go_to_index, uploaded_data, current_page) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive values
    summary_table <- reactiveVal(data.frame())
    results <- reactiveVal(list())
    annotated_results <- reactiveVal()
    processing_state <- reactiveVal("start")  # start → samples_checked → genes_converted → bams_corrected → fraser_corrected → results_calculated → done

    #Buttons
    mod_nav_buttons_server("nav_buttons", next_page = go_to_parameters, previous_page = go_to_upload)
    mod_home_button_server("home_btn", go_to_index = go_to_index)

    observeEvent(current_page(), {
      req(current_page() == ROUTES$PROCESSING)

      message("📍 Navigated to Processing page — starting processing.")
      processing_state("start")
    })

    # --- Progress UI Renderer ---
    output$check_samples_ui <- renderUI({
      if (processing_state() %in% c("samples_checked", "genes_converted", "bams_corrected", "fraser_corrected", "results_calculated", "done")) {
        tags$p("✅ Checking sample identifiers... Done")
      } else {
        tags$p("⏳ Checking sample identifiers...")
      }
    })

    output$convert_genes_ui <- renderUI({
      if (processing_state() %in% c("genes_converted", "bams_corrected", "fraser_corrected", "results_calculated", "done")) {
        tags$p("✅ Converting gene names... Done")
      } else if (processing_state() == "samples_checked") {
        tags$p("⏳ Converting gene names...")
      } else {
        tags$p("⏹️ Converting gene names... Waiting")
      }
    })

    output$fix_bam_paths_ui <- renderUI({
      if (processing_state() %in% c("bams_corrected", "fraser_corrected", "results_calculated", "done")) {
        tags$p("✅ Correcting BAM paths... Done")
      } else if (processing_state() == "genes_converted") {
        tags$p("⏳ Correcting BAM paths...")
      } else {
        tags$p("⏹️ Correcting BAM paths... Waiting")
      }
    })

    output$fix_fraser_paths_ui <- renderUI({
      if (processing_state() %in% c("fraser_corrected", "results_calculated", "done")) {
        tags$p("✅ Correcting FRASER paths... Done")
      } else if (processing_state() == "bams_corrected") {
        tags$p("⏳ Correcting FRASER paths...")
      } else {
        tags$p("⏹️ Correcting FRASER paths... Waiting")
      }
    })

    output$calculate_results_ui <- renderUI({
      if (processing_state() %in% c("results_calculated", "done")) {
        tags$p("✅ OUTRIDER & FRASER Results Calculated... Done")
      } else if (processing_state() == "fraser_corrected") {
        tags$p("⏳ Calculating OUTRIDER & FRASER Results...")
      } else {
        tags$p("⏹️ Calculating OUTRIDER & FRASER Results... Waiting")
      }
    })

    output$annotate_results_ui <- renderUI({
      if (processing_state() %in% c("done")) {
        tags$p("✅ OUTRIDER & FRASER Results Annotated... Done")
      } else if (processing_state() == "results_calculated") {
        tags$p("⏳ Annotating OUTRIDER & FRASER Results...")
      } else {
        tags$p("⏹️ Annotating OUTRIDER & FRASER Results... Waiting")
      }
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
      shinipsum::random_ggplot("bar")
    })

    # --- Observe uploaded_data and trigger steps ---

    ## sample summary
    observe({
      req(processing_state() == "start")
      req(uploaded_data$samplesheet, uploaded_data$outrider, uploaded_data$fraser)
      message("✅ All inputs detected. Starting Step 1...")

      # Summarise which data is available for each sample
      summary_table(summarise_data(uploaded_data$outrider, uploaded_data$fraser))

      # Update processing state
      processing_state("samples_checked")
    })

    ## change ensembl id to hgnc for outrider
    observeEvent(processing_state(), {
      req(uploaded_data$outrider, uploaded_data$fraser)
      if (processing_state() == "samples_checked") {

        # Annotate Ensembl IDs with HGNC
        annotated_ods <- annotate_ensembl_ids(uploaded_data$outrider)

        # Save back into uploaded_data
        uploaded_data$outrider <- annotated_ods

        # Update processing state
        processing_state("genes_converted")
      }
    }, ignoreInit = TRUE)

    ## change the BAM paths
    observeEvent(processing_state(), {
      req(uploaded_data$outrider, uploaded_data$fraser, uploaded_data$bam_dir)
      if (processing_state() == "genes_converted") {

        #Fix paths to BAM files
        outrider_fixed <- update_bam_paths(uploaded_data$outrider, uploaded_data$bam_dir)
        fraser_fixed <- update_bam_paths(uploaded_data$fraser, uploaded_data$bam_dir)

        # Save back to uploaded date
        uploaded_data$outrider <- outrider_fixed
        uploaded_data$fraser <- fraser_fixed

        # Update processing state
        processing_state("bams_corrected")
      }
    }, ignoreInit = TRUE)

    ## change the fraser paths
    observeEvent(processing_state(), {
      req(uploaded_data$fraser)
      if (processing_state() == "bams_corrected") {

        # Fix paths to fraser objects
        fds_fixed <- fixFdsH5Paths(uploaded_data$fraser)

        # Save back to uploaded data
        uploaded_data$fraser <- fds_fixed

        # Update processing state
        processing_state("fraser_corrected")
      }
    }, ignoreInit = TRUE)

    ## calculate results
    observeEvent(processing_state(), {
      req(uploaded_data$outrider, uploaded_data$fraser)
      if (processing_state() == "fraser_corrected") {

        # Calculate fraser and outrider results
        results(generate_results(uploaded_data$outrider, uploaded_data$fraser))
        processing_state("results_calculated")
      }
    }, ignoreInit = TRUE)

    observeEvent(processing_state(), {
      if (processing_state() == "results_calculated") {
        req(results())

        # Annotate fraser and outrider results
        fres_annotated <- annotate_results_with_omim_go(
          results = results()$frares,
          add_omim = TRUE,
          add_go = TRUE
        )

        # OUTRIDER example (with same gene list)
        ores_annotated <- annotate_results_with_omim_go(
          results = results()$outres,
          add_omim = TRUE,
          add_go = TRUE
        )

        annotated_results(list(frares = fres_annotated, outres = ores_annotated))

        # Update processing state
        processing_state("done")
      }
    }, ignoreInit = TRUE)
  })
}

## To be copied in the UI
# mod_process_ui("process_1")

## To be copied in the server
# mod_process_server("process_1")

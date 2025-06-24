#' individual_res UI Function
#'
#' @description A shiny Module for displaying results of individual analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput uiOutput plotOutput column tags fluidRow textOutput actionButton
#' @importFrom DT DTOutput
#'
mod_individual_res_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Results"),
    util_page_layout(
      home_id = ns("home_btn"),
      tags$div(
        style = "padding: 20px;",
        fluidRow(
          style = "text-align:left;",
          tags$h1("Individual Results"),
          column(6,
                 uiOutput(ns("select_sample")),
                 textOutput(ns("phenotype")),
                 tags$br(),
                 actionButton(ns("display_results"), label = "Display Results", class = "btn btn-success")),
          column(6,
          tags$h2("Overlapping FRASER & OUTRIDER Results"),
          "Genes aberrantly expressed with ≥1 splice event",
          DTOutput(ns("genes_overlap")))
        ),

        tags$hr(),

        fluidRow(
          column(6,
                 style = 'overflow-x:auto;',
                 tags$h2("OUTRIDER Results"),
                 DTOutput(ns("outrider_res"))
          ),
          column(6,
                 style = 'overflow-x:auto;',
                 tags$h2("FRASER Results"),
                 DTOutput(ns("fraser_res"))
          )
        ),

        fluidRow(
          column(6,
                 tags$h2("OUTRIDER Volcano Plot"),
                 plotOutput(ns("outrider_volcplot"))
          ),
          column(6,
                 tags$h2("FRASER Volcano Plot"),
                 selectInput(ns("fraser_metric"), "Choose Metric", choices = c("jaccard", "theta", "psi5", "psi3")),
                 plotOutput(ns("fraser_volcplot"))
          )
        ),

        tags$hr(),

        fluidRow(
          column(12,
            tags$h2("ggshashimi Plot of Aberrant Splice Junction"),
            uiOutput(ns("select_event_ui")),
            uiOutput(ns("controls")),
            actionButton(ns("plot_sashimi"), "Generate Sashimi Plot", class = "btn btn-primary"),
            imageOutput(ns("sashimi"), height = "auto")
          )
        ),

        fluidRow(
          column(6,
                 tags$h2("RNA Variant Calling Results"),
                 DTOutput(ns("rvc_table"))
          ),
          column(6,
                 tags$h2("RNA Fusions & SV Results"),
                 DTOutput(ns("fusionsv_table"))
          )
        ),

        fluidRow(
          column(6,
                 style = "text-align:left; padding: 20px;",
                 tags$br(),
                 actionButton(ns("return"), "Return to parameter selection", class = 'btn btn-warning')),
          column(6,
                 style = "text-align:left; padding: 20px;",
                 tags$br(),
                 downloadButton(ns("download"), "Download Report", class = "btn btn-success"))
        )
      )
    )
  )
}

#' individual_res Server Functions
#'
#' @noRd
#'
#' @importFrom shiny observeEvent reactive req renderText renderPlot
#' @importFrom DT renderDT
#' @importFrom shinipsum random_DT random_ggplot
mod_individual_res_server <- function(id, go_to_parameters, go_to_index, uploaded_data, processed_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # reactive logic for action button
    results_ready <- reactiveVal(FALSE)

    observeEvent(input$display_results, {
      req(selected_sample())
      results_ready(TRUE)
    })

    observeEvent(input$select_sample, {
      results_ready(FALSE)
    })

    observeEvent(input$return, {
      go_to_parameters()})

    mod_home_button_server("home_btn", go_to_index = go_to_index)

    selected_sample <- reactive({
      req(input$select_sample)
      input$select_sample
    })

    output$select_sample <- renderUI({
      req(uploaded_data$samplesheet)
      selectInput(
        ns("select_sample"),
        "Choose Sample",
        choices = uploaded_data$samplesheet$RNA_ID,
        selected = NULL
      )
    })


    output$phenotype <- renderText({
      req(uploaded_data$samplesheet, selected_sample())
      sample_row <- uploaded_data$samplesheet[uploaded_data$samplesheet$RNA_ID == selected_sample(), ]
      paste0("Phenotype: ", sample_row$PHENOTYPE)
    })

    output$genes_overlap <- renderDT({
      req(results_ready(), processed_data$merged)
      selected_sample()
      filt_merged <- filtered_annotated_table(processed_data$merged, samples = selected_sample())
      filt_merged
    })

    output$fraser_res <- render_gene_table(processed_data$annotated_results$frares, sample_id = reactive({
      req(results_ready())
      selected_sample()
    }))

    output$outrider_res <- render_gene_table(processed_data$annotated_results$outres, sample_id = reactive({
      req(results_ready())
      selected_sample()
    }))

    output$outrider_volcplot <- renderPlot({
      req(results_ready(), processed_data$outrider, selected_sample())
      OUTRIDER::plotVolcano(
        processed_data$outrider,
        sampleID = selected_sample(),
        xaxis = "zscore",
        label = "aberrant",
        basePlot = TRUE
      )
    })

    output$fraser_volcplot <- renderPlot({
      req(results_ready(), processed_data$fraser, selected_sample(), input$fraser_metric)
      FRASER::plotVolcano(
        processed_data$fraser,
        sampleID = selected_sample(),
        label = "aberrant",
        type = input$fraser_metric
      )
    })

    output$select_event_ui <- renderUI({
      req(results_ready(), processed_data$annotated_results)
      filtered_data <- filtered_annotated_table(processed_data$annotated_results$frares, samples = selected_sample())
      if (nrow(filtered_data) == 0) {
        return(tags$div(
          class = "alert alert-info",
          "ℹ️ No aberrant splicing events available for this sample."
        ))
      }
      choices <- setNames(
        seq_len(nrow(filtered_data)),  # the value returned by selectInput
        paste0(
          "gene: ", filtered_data$geneID,
          " | coord: ", filtered_data$coord,
          " | ", filtered_data$type)
      )
      selectInput(
        ns("select_event"),
        "Select Aberrant Splice Event",
        choices = choices
      )
    })

    output$controls <- renderUI({
      req(uploaded_data$samplesheet)
      selectInput(
        ns("controls"),
        "Choose Control Samples (At Least 2)",
        choices = uploaded_data$samplesheet$RNA_ID,
        multiple = TRUE
      )
    })


    sashimi_path <- reactiveVal(NULL)

    observeEvent(input$plot_sashimi, {
      req(uploaded_data$samplesheet, input$select_event, input$controls, length(input$controls) >= 2)

      filtered_data <- filtered_annotated_table(processed_data$annotated_results$frares, samples = selected_sample())

      result_paths <- generate_sashimi_plot(
        result_object = filtered_data,
        sample_index = as.integer(input$select_event),
        samplesheet = uploaded_data$samplesheet,
        controls = input$controls
      )

      if (!is.null(result_paths) && file.exists(result_paths$png)) {
        sashimi_path(result_paths$png)
      } else {
        sashimi_path("www/placeholder.png")
      }
    })

    output$sashimi <- renderImage({
      req(sashimi_path())
      list(
        src = normalizePath(sashimi_path()),
        contentType = "image/png",
        width = "75%",
        alt = "Sashimi plot"
      )
    }, deleteFile = FALSE)

    output$rvc_table <- filtered_VC(uploaded_data$rna_vcf, sample_id = reactive({
      req(results_ready())
      selected_sample()
    }))

    output$fusionsv_table <- render_rna_fusions(uploaded_data$samplesheet, sample_id = reactive({
      req(results_ready())
      selected_sample()
    }))

    output$download <- downloadHandler(
      filename = function() {
        paste0("Sample_Report_", selected_sample(), ".pdf")
      },
      content = function(file) {
        req(results_ready(), selected_sample())

        # Extract phenotype
        sample_row <- uploaded_data$samplesheet[uploaded_data$samplesheet$RNA_ID == selected_sample(), ]
        phenotype <- sample_row$PHENOTYPE

        # Extract results
        outres <- processed_data$annotated_results$outres
        frares <- processed_data$annotated_results$frares
        merged <- processed_data$merged

        # Extract datasets
        ods <- processed_data$outrider
        fds <- processed_data$fraser

        # Copy Rmd template to temp directory
        temp_dir <- tempdir()
        tempReport <- file.path(temp_dir, "report_template.Rmd")
        tempHeader <- file.path(temp_dir, "header.tex")
        tempLogo <- file.path(temp_dir, "logo.png")
        tempFonts <- file.path(temp_dir, "fonts.tex")
        sample_id <- selected_sample()
        sashimi_dir <- file.path("generated/plots")  # your image source dir
        sashimi_files <- list.files(
          sashimi_dir,
          pattern = paste0("^", sample_id, "_.*sashimi_plot\\.png$"),
          full.names = TRUE
        )

        # Copy to tempdir so Rmd can access them
        file.copy(sashimi_files, temp_dir, overwrite = TRUE)
        file.copy("inst/reports/report_template.Rmd", tempReport, overwrite = TRUE)
        file.copy("inst/reports/header.tex", tempHeader, overwrite = TRUE)
        file.copy("inst/reports/fonts.tex", tempFonts, overwrite = TRUE)
        file.copy("inst/reports/logo.png", tempLogo, overwrite = TRUE)

        # Render PDF report
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = list(
            sample_id = selected_sample(),
            phenotype = phenotype,
            outres = outres,
            frares = frares,
            ods = ods,
            fds = fds,
            merged = merged
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}

## To be copied in the UI
# mod_individual_res_ui("individual_res_1")

## To be copied in the server
# mod_individual_res_server("individual_res_1")

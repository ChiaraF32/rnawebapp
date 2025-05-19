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
          uiOutput(ns("select_sample")),
          tags$h2("Genes aberrantly expressed with ≥1 splice event"),
          column(6, DTOutput(ns("genes_overlap")))
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
                 actionButton(ns("download"), "Download Report", class = "btn btn-success"))
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

    observeEvent(input$return, {
      go_to_parameters()})

    mod_home_button_server("home_btn", go_to_index = go_to_index)

    selected_sample <- reactive({
      req(input$select_sample)
    })

    output$select_sample <- renderUI({
      req(uploaded_data$samplesheet)
      selectInput(
        ns("select_sample"),
        "Choose Sample",
        choices = uploaded_data$samplesheet$RNA_ID
      )
    })

    output$genes_overlap <- renderDT({
      req(processed_data$merged)
      selected_sample()
      filt_merged <- filtered_annotated_table(processed_data$merged, samples = selected_sample())
      filt_merged
    })

    output$fraser_res <- render_gene_table(processed_data$annotated_results$frares, sample_id = selected_sample)

    output$outrider_res <- render_gene_table(processed_data$annotated_results$outres, sample_id = selected_sample)

    output$outrider_volcplot <- renderPlot(
      OUTRIDER::plotVolcano(processed_data$outrider, sampleID = selected_sample(), xaxis = "zscore", label = "aberrant", basePlot = TRUE)
    )

    output$fraser_volcplot <- renderPlot({
      req(processed_data$fraser)
      selected_sample()
      req(input$fraser_metric)
      FRASER::plotVolcano(processed_data$fraser, sampleID = selected_sample(), label = "aberrant", type = input$fraser_metric)
    })

    output$select_event_ui <- renderUI({
      req(processed_data$annotated_results)
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

    output$sashimi <- renderImage({
      req(uploaded_data$bam_dir, input$select_event)

      filtered_data <- filtered_annotated_table(processed_data$annotated_results$frares, samples = selected_sample())

      result_paths <- generate_sashimi_plot(
        result_object = filtered_data,
        sample_index = as.integer(input$select_event),
        bam_dir = uploaded_data$bam_dir,
        controls = input$controls
      )

      if (is.null(result_paths) || !file.exists(result_paths$png)) {
        return(list(
          src = "www/placeholder.png",  # optional fallback
          contentType = "image/png",
          alt = "Sashimi plot not available"
        ))
      }

      list(
        src = normalizePath(result_paths$png),
        contentType = "image/png",
        width = "100%",
        alt = "Sashimi plot"
      )
    }, deleteFile = FALSE)

    output$rvc_table <- renderDT(random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$fusionsv_table <- renderDT(random_DT(nrow = 6, ncol = 9, type = "numchar"))

  })
}

## To be copied in the UI
# mod_individual_res_ui("individual_res_1")

## To be copied in the server
# mod_individual_res_server("individual_res_1")

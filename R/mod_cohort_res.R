#' cohort_res UI Function
#'
#' @description A shiny Module for displaying results from cohort analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow tags column actionButton
#' @importFrom DT DTOutput
mod_cohort_res_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Results"),
    util_page_layout(
      home_id = ns("home_btn"),
      tags$div(
        style = "padding: 20px;",

        fluidRow(
          tags$h1("Cohort Results"),
          tags$p("Explore gene expression and outliers across cohort"),
          tags$hr()),

        fluidRow(
          column(4,
                 tags$h2("Cohort Gene Expression"),
                 selectInput(ns("select_phenotype"), "Choose Phenotype from PanelApp", choices = NULL, multiple = TRUE),
                 uiOutput(ns("select_gene")),
                 uiOutput(ns("select_sample"))),
          column(8, plotOutput(ns("gene_plot")))
        ),

        fluidRow(
          column(6,
                 tags$h2("OUTRIDER Results"),
                 DTOutput(ns("outrider_res"))),
          column(6,
                 tags$h2("FRASER Results"),
                 DTOutput(ns("fraser_res")))
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

#' cohort_res Server Functions
#'
#' @noRd
#'
#' @importFrom shiny observeEvent renderPlot
#' @importFrom DT renderDT
#' @importFrom shinipsum random_DT random_ggplot
#' @importFrom SummarizedExperiment rownames
mod_cohort_res_server <- function(id, go_to_parameters, go_to_index, uploaded_data, processed_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    panels <- reactiveVal()

    observeEvent(input$return, {
      go_to_parameters()})

    mod_home_button_server("home_btn", go_to_index = go_to_index)

    observe({
      all_panels <- fetch_all_panels("uk")
      panels(all_panels)

      updateSelectInput(
        session,
        inputId = "select_phenotype",
        choices = setNames(all_panels$id, all_panels$name)
      )
    })

    phenotype_genes <- reactive({
      req(input$select_phenotype)
      unique(unlist(
        lapply(input$select_phenotype, function(panel_id) {
          get_genes_for_panel(panel_id, region = "uk")
        })
      ))
    })

    output$select_sample <- renderUI({
      req(uploaded_data$samplesheet)
      selectInput(
        ns("select_sample"),
        "Choose Sample",
        choices = uploaded_data$samplesheet$RNA_ID
      )
    })

    output$select_gene <- renderUI({
      req(processed_data$outrider)
      selectInput(
        ns("select_gene"),
        "Choose Gene(s)",
        choices = rownames(processed_data$outrider),
        multiple = TRUE
        )
    })

    output$gene_plot <- renderPlot({
      req(processed_data$outrider)
      req(input$select_gene)
      req(input$select_sample)
      plot_gene_expression_multi(processed_data$outrider, union(input$select_gene, phenotype_genes()), input$select_sample)
    })

    output$fraser_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      req(input$select_gene)
      filtered_data <- dplyr::filter(processed_data$annotated_results$frares, geneID %in% union(input$select_gene, phenotype_genes()))
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })

    output$outrider_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      req(input$select_gene)
      filtered_data <- dplyr::filter(processed_data$annotated_results$outres, geneID %in% union(input$select_gene, phenotype_genes()))
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })
  })
}

## To be copied in the UI
# mod_cohort_res_ui("cohort_res_1")

## To be copied in the server
# mod_cohort_res_server("cohort_res_1")

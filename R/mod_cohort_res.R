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
                 selectizeInput(ns("select_phenotype"), "Choose Phenotype from PanelApp", choices = NULL, multiple = TRUE),
                 uiOutput(ns("select_gene")),
                 uiOutput(ns("select_sample"))),
          column(8,
                 plotOutput(ns("gene_plot")),
                 fluidRow(
                   column(4, actionButton(ns("prev_page"), "Previous")),
                   column(4, textOutput(ns("page_status")), style = "margin-top: 10px; text-align: center;"),
                   column(4, actionButton(ns("next_page"), "Next"))
                 )
          )
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

    observeEvent(input$return, {
      go_to_parameters()})

    mod_home_button_server("home_btn", go_to_index = go_to_index)

    # Create a trigger that can be updated externally
    update_trigger <- reactiveVal(Sys.time())

    observeEvent(update_trigger(), {
      # This will run once when the module initializes
      all_panels <- suppressWarnings(fetch_all_panels("uk"))
      # Use the correct ID - this was the key issue
      updateSelectizeInput(session, "select_phenotype", choices = setNames(all_panels$id, all_panels$name))
    })

    # Expose the trigger for external refresh
    session$userData$refresh_cohort_dropdown <- function() {
      update_trigger(Sys.time())  # change value to retrigger observeEvent
    }

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

    # pagination logic
    all_selected_genes <- reactive({
      genes1 <- input$select_gene
      genes2 <- phenotype_genes()

      # Ensure NULLs are treated as empty character vectors
      genes1 <- if (is.null(genes1)) character() else genes1
      genes2 <- if (is.null(genes2)) character() else genes2

      combined <- union(genes1, genes2)
      validate(
        need(length(combined) > 0, "Please select at least one gene or phenotype panel.")
      )
      combined
    })

    genes_per_page <- 6
    page <- reactiveVal(1)

    total_pages <- reactive({
      genes <- all_selected_genes()
      ceiling(length(genes) / genes_per_page)
    })

    # Reset page when gene list changes
    observeEvent(all_selected_genes(), {
      page(1)
    })

    # Navigation buttons
    observeEvent(input$next_page, {
      if (page() < total_pages()) page(page() + 1)
    })

    observeEvent(input$prev_page, {
      if (page() > 1) page(page() - 1)
    })

    paged_genes <- reactive({
      genes <- all_selected_genes()
      start <- (page() - 1) * genes_per_page + 1
      end <- min(page() * genes_per_page, length(genes))
      genes[start:end]
    })

    output$page_status <- renderText({
      paste("Page", page(), "of", total_pages())
    })

    ## rendering plots

    output$gene_plot <- renderPlot({
      req(processed_data$outrider, input$select_sample)
      genes_to_plot <- paged_genes()
      req(length(genes_to_plot) > 0)

      plot_gene_expression_multi(
        ods = processed_data$outrider,
        genes = genes_to_plot,
        sampleID = input$select_sample
      )
    })

    output$fraser_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      filtered_data <- dplyr::filter(processed_data$annotated_results$frares, geneID %in% all_selected_genes())
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })

    output$outrider_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      filtered_data <- dplyr::filter(processed_data$annotated_results$outres, geneID %in% all_selected_genes())
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })
  })
}

## To be copied in the UI
# mod_cohort_res_ui("cohort_res_1")

## To be copied in the server
# mod_cohort_res_server("cohort_res_1")

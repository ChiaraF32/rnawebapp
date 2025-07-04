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
                 mod_sample_selector_ui(ns("select_sample")),
                 numericInput(ns("plot_number"), "Choose number of panels", value = 6, min = 1, max = 12, step = 1),
                 mod_display_trigger_ui(ns("display_trigger"))),
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
                 mod_export_excel_ui(ns("export_excel")))
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

    # sample selector server
    selected_sample <- mod_sample_selector_server(
      "select_sample",
      samplesheet = reactive(uploaded_data$samplesheet)
    )

    # Navigation (prev page, home page)
    observeEvent(input$return, {
      go_to_parameters()})
    mod_home_button_server("home_btn", go_to_index = go_to_index)

    #reactive logic for action button
    results_ready <- mod_display_trigger_server(
      "display_trigger",
      reset_when = reactive({
        list(input$select_gene, selected_sample(), input$select_phenotype)
      })
    )

    # Load PanelApp options drop-down
    ## Create a trigger that will ensure panelApp is loaded when module initialises
    update_trigger <- reactiveVal(Sys.time())
    ## Fetch panels and update SelectInput
    observeEvent(update_trigger(), {
      all_panels <- fetch_all_panels("uk")
      updateSelectizeInput(session, "select_phenotype", choices = setNames(all_panels$id, all_panels$name))
    })
    ## Expose the trigger for external refresh
    session$userData$refresh_cohort_dropdown <- function() {
      update_trigger(Sys.time())
    }

    # Collect the genes from the chosen phenotype panel
    phenotype_genes <- reactive({
      if (is.null(input$select_phenotype) || length(input$select_phenotype) == 0) return(character(0))
      unique(unlist(
        lapply(input$select_phenotype, function(panel_id) {
          get_genes_for_panel(panel_id, region = "uk")
        })
      ))
    })

    # Create object containing genes that are expressed in the outrider data set
    expressed_genes <- reactive({
      req(processed_data$outrider)
      norm_counts <- SummarizedExperiment::assay(processed_data$outrider, normalized = TRUE)
      expressed <- rownames(norm_counts)[rowSums(norm_counts) > 0]
      expressed
    })

    # Update the gene selection dropdown based on the genes that are expressed
    output$select_gene <- renderUI({
      req(processed_data$outrider)
      selectInput(
        ns("select_gene"),
        "Choose Gene(s)",
        choices = expressed_genes(),
        multiple = TRUE
        )
    })

    #Create reactive for selected gene
    selected_gene <- reactive({
      if (is.null(input$select_gene)) character() else input$select_gene
    })

    # Define pagination logic to display gene plots across multiple pages

    ## Create an object with all selected genes, including those selected manually and those pulled from PanelApp
    ## Filter to include only genes that are expressed
    all_selected_genes <- reactive({
      genes1 <- selected_gene()
      genes2 <- phenotype_genes()
      expressed <- expressed_genes()

      # Ensure NULLs are treated as empty character vectors
      genes1 <- if (is.null(genes1)) character() else genes1
      genes2 <- if (is.null(genes2)) character() else genes2

      combined <- union(genes1, genes2)
      valid <- intersect(combined, expressed)
      validate(
        need(length(genes1) > 0 || length(genes2) > 0,
             "Please select at least one gene or phenotype panel."),
        need(length(valid) > 0, "None of the selected genes are expressed.")
      )

      valid
    })

    ## Define the number of plots (genes) to display per page
    genes_per_page <- reactive({ input$plot_number })
    page <- reactiveVal(1)

    ## Define the total number of pages
    total_pages <- reactive({
      genes <- all_selected_genes()
      ceiling(length(genes) / genes_per_page())
    })

    ## Reset page when gene list changes
    observeEvent(all_selected_genes(), {
      page(1)
    })

    ## Navigation buttons for plotting
    observeEvent(input$next_page, {
      if (page() < total_pages()) page(page() + 1)
    })
    observeEvent(input$prev_page, {
      if (page() > 1) page(page() - 1)
    })

    ## Define pagination logic
    paged_genes <- reactive({
      req(all_selected_genes())
      genes <- all_selected_genes()
      start <- (page() - 1) * genes_per_page() + 1
      end <- min(page() * genes_per_page(), length(genes))
      genes[start:end]
    })

    ## Keep track of which page is active
    output$page_status <- renderText({
      paste("Page", page(), "of", total_pages())
    })

    ## Render gene plot

    output$gene_plot <- renderPlot({
      req(results_ready(), processed_data$outrider, selected_sample())
      genes_to_plot <- paged_genes()
      req(length(genes_to_plot) > 0)

      plot_gene_expression_multi(
        ods = processed_data$outrider,
        genes = genes_to_plot,
        sampleID = selected_sample()
      )
    })

    # Render datatables with fraser and outrider results filtered according to chosen genes

    output$fraser_res <- renderDT({
      req(results_ready(), processed_data$annotated_results)
      util_nowrap_dt(
        filtered_annotated_table(processed_data$annotated_results$frares, all_selected_genes()),
        nowrap_columns = c("GO_TERMS", "Phenotypes")
    )
  })

    output$outrider_res <- renderDT({
      req(results_ready(), processed_data$annotated_results)
      util_nowrap_dt(
        filtered_annotated_table(processed_data$annotated_results$outres, all_selected_genes()),
        nowrap_columns = c("GO_TERMS", "Phenotypes")
      )
    })

    mod_export_excel_server(
      id = "export_excel",
      fraser_data = reactive({
        req(results_ready(), processed_data$annotated_results)
        filtered_annotated_table(processed_data$annotated_results$frares, all_selected_genes())
      }),
      outrider_data = reactive({
        req(results_ready(), processed_data$annotated_results)
        filtered_annotated_table(processed_data$annotated_results$outres, all_selected_genes())
      }),
      enabled = results_ready
    )
  })
}

## To be copied in the UI
# mod_cohort_res_ui("cohort_res_1")

## To be copied in the server
# mod_cohort_res_server("cohort_res_1")

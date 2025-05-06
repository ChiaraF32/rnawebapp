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
          textOutput(ns("genes_overlap"))
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
                 plotOutput(ns("fraser_volcplot"))
          )
        ),

        tags$hr(),

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

    overlap <- reactive({
      req(input$select_sample)
      sample(1:10, 1)
    })

    output$select_sample <- renderUI({
      req(uploaded_data$samplesheet)
      selectInput(
        ns("select_sample"),
        "Choose Sample",
        choices = uploaded_data$samplesheet$RNA_ID
      )
    })

    output$genes_overlap <- renderText({
      paste0("Genes aberrantly expressed and spliced: ", overlap())
    })

    output$fraser_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      req(input$select_sample)
      filtered_data <- dplyr::filter(processed_data$annotated_results$frares, sampleID %in% input$select_sample)
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })

    output$outrider_res <- renderDT({
      req(processed_data$annotated_results)
      req(is.list(processed_data$annotated_results))
      req(input$select_sample)
      filtered_data <- dplyr::filter(processed_data$annotated_results$outres, sampleID %in% input$select_sample)
      util_nowrap_dt(filtered_data, nowrap_columns = c("GO_TERMS", "Phenotypes"))
    })

    output$outrider_volcplot <- renderPlot(
      OUTRIDER::plotVolcano(processed_data$outrider, sampleID = input$select_sample, xaxis = "zscore", label = aberrant, basePlot = TRUE)
    )

    output$fraser_volcplot <- renderPlot(
      FRASER::plotVolcano(processed_data$fraser, sampleID = input$select_sample, type = "theta")
    )

    output$rvc_table <- renderDT(random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$fusionsv_table <- renderDT(random_DT(nrow = 6, ncol = 9, type = "numchar"))

  })
}

## To be copied in the UI
# mod_individual_res_ui("individual_res_1")

## To be copied in the server
# mod_individual_res_server("individual_res_1")

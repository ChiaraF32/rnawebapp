#' cohort_res UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cohort_res_ui <- function(id) {
  ns <- NS(id)
  tagList(
    util_progress_bar(current_step = "Results"),
    tags$div(
      style = "padding: 20px;",
      fluidRow(
        tags$h1("Cohort Results"),
        tags$p("Explore gene expression and outliers across cohort"),
        tags$hr()),

      fluidRow(
        column(4,
               tags$h2("Cohort Gene Expression"),
               selectInput(ns("select_phenotype"), "Choose Phenotype", choices = c("Myopathy", "Neuropathy", "Ataxia")),
               selectInput(ns("select_gene"), "Choose Gene(s)", choices = c("Gene1", "Gene2", "Gene3")),
               selectInput(ns("select_patient"), "Select Sample", choices = c("sample1", "sample2", "sample3"))),
        column(8, plotOutput(ns("gene_plot")))
      ),

      fluidRow(
        column(6,
               tags$h2("OUTRIDER Results"),
               DT::DTOutput(ns("outrider_res"))),
        column(6,
               tags$h2("FRASER Results"),
               DT::DTOutput(ns("fraser_res")))
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
      ),
      mod_home_button_ui(ns("home_btn")),
    )
  )
}

#' cohort_res Server Functions
#'
#' @noRd
mod_cohort_res_server <- function(id, go_to_parameters, go_to_index, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$return, {
      go_to_parameters()})

    mod_home_button_server("home_btn", go_to_index = go_to_index)

    output$gene_plot <- renderPlot(shinipsum::random_ggplot(type = "point"))

    output$outrider_res <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$fraser_res <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))
  })
}

## To be copied in the UI
# mod_cohort_res_ui("cohort_res_1")

## To be copied in the server
# mod_cohort_res_server("cohort_res_1")

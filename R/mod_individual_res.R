#' individual_res UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput uiOutput plotOutput
mod_individual_res_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             tags$h1("Individual Results"),
             selectInput(ns("select_sample"), "Choose Sample", choices = c("sample1", "sample2", "sample3")),
             textOutput(ns("genes_overlap"))
      ),
      column(6,
             actionButton(ns("return"), "Return to parameter selection"))
    ),

    tags$hr(),

    fluidRow(
      column(6,
             tags$h2("OUTRIDER Results"),
             DT::DTOutput(ns("outrider_res"))
             ),
      column(6,
             tags$h2("FRASER Results"),
             DT::DTOutput(ns("fraser_res"))
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
             DT::DTOutput(ns("rvc_table"))
             ),
      column(6,
             tags$h2("RNA Fusions & SV Results"),
             DT::DTOutput(ns("fusionsv_table"))
             )
    )
  )
}

#' individual_res Server Functions
#'
#' @noRd
mod_individual_res_server <- function(id, go_to_parameters, uploaded_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$return, {
      go_to_parameters()})

    overlap <- reactive({
      req(input$select_sample)
      sample(1:10, 1)
    })

    output$genes_overlap <- renderText({
      paste0("Genes aberrantly expressed and spliced: ", overlap())
    })

    output$outrider_res <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$fraser_res <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$outrider_volcplot <- renderPlot(shinipsum::random_ggplot(type = "point"))

    output$fraser_volcplot <- renderPlot(shinipsum::random_ggplot(type = "point"))

    output$rvc_table <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))

    output$fusionsv_table <- DT::renderDT(shinipsum::random_DT(nrow = 6, ncol = 9, type = "numchar"))

  })
}

## To be copied in the UI
# mod_individual_res_ui("individual_res_1")

## To be copied in the server
# mod_individual_res_server("individual_res_1")

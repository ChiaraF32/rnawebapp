#' index UI Function
#'
#' @description Shiny module providing a homepage for the RNAseq web app
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton tags fluidRow column

mod_index_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$div(
      style = "text-align:center; padding: 50px;",
      tags$h1("Welcome to RNA Web App"),
      tags$p("Analyse RNA-seq data from rare disease patients."),
      tags$br(),
      actionButton(ns("get_started"), "Get Started", class = "btn btn-success"),
      tags$br(),
      tags$a(href = "quickstart.html", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             "Quick-Start Guide"),
      tags$a(href = "documentation.html", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             "Documentation"),
      tags$a(href = "https://github.com/ChiaraF32/rnawebapp", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             "GitHub"),
      tags$a(href = "contact.html", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             "Contact")
    ),

    tags$hr(), # horizontal line separator

    tags$div(
      style = "padding: 20px;",
      tags$h3("Example Analyses"),

      fluidRow(
        column(4, plotOutput(ns("plot1"))),
        column(4, DT::DTOutput(ns("table1"))),
        column(4, imageOutput(ns("image1")))
      )
    )
  )
}

#' index Server Functions
#'
#' @noRd
#' @importFrom shiny renderPlot renderImage observeEvent showNotification plotOutput imageOutput
#' @importFrom shinipsum random_DT random_ggplot random_image
mod_index_server <- function(id, go_to_upload){
  moduleServer(id, function(input, output, session){

    #Landing page plot placeholders
    output$plot1 <- renderPlot(shinipsum::random_ggplot(type = "point"))
    output$table1 <- DT::renderDT(shinipsum::random_DT(nrow = 4, ncol = 5, type = "numchar"))
    output$image1 <- renderImage(shinipsum::random_image())

    observeEvent(input$get_started, {
      showNotification("Let's get started!")
      go_to_upload()
    })
  })
}

## To be copied in the UI
# mod_index_ui("index_1")

## To be copied in the server
# mod_index_server("index_1")

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
             shiny::icon("play"), "Quick-Start Guide"),
      tags$a(href = "https://docs.readthedocs.com/platform/latest/tutorial/index.html", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             shiny::icon("book"), "Documentation"),
      tags$a(href = "https://github.com/ChiaraF32/rnawebapp", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             shiny::icon("github"), "GitHub"),
      tags$a(href = "contact.html", target = "_blank",
             class = "btn btn-secondary", style = "margin: 10px;",
             shiny::icon("phone"), "Contact")
    ),

    tags$hr(), # horizontal line separator

    tags$div(
      style = "padding: 20px;",
      tags$h3("Example Analyses"),

      fluidRow(
        column(4, tags$img(src = "www/volcano_plot.png", width = "100%")),
        column(4, tags$img(src = "www/sashimi_plot.png", width = "100%")),
        column(4, tags$img(src = "www/gene_plot.png", width = "100%"))
      )
    )
  )
}

#' index Server Functions
#'
#' @noRd
#' @importFrom shiny renderPlot renderImage observeEvent showNotification plotOutput imageOutput
#' @importFrom shinipsum random_DT random_ggplot random_image
mod_index_server <- function(id, go_to_upload, go_to_test){
  moduleServer(id, function(input, output, session){

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

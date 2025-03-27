#' nav_buttons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nav_buttons_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$div(
        style = "display: flex; gap: 10px; text-align:left; padding: 30px;",
          actionButton(
          inputId = ns("return"),
          label = tagList(icon("arrow-left"), "Return"),
          class = "btn btn-primary"
        ),
        actionButton(
          inputId = ns("proceed"),
          label = tagList(icon("arrow-right"), "Proceed"),
          class = "btn btn-success"
        )
      )
    )
  )
}

#' nav_buttons Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent
mod_nav_buttons_server <- function(id, next_page, previous_page){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$proceed, {
      next_page()
    })
    observeEvent(input$return, {
      previous_page()
    })
  })
}


## To be copied in the UI
# mod_nav_buttons_ui("nav_buttons_1")

## To be copied in the server
# mod_nav_buttons_server("nav_buttons_1")

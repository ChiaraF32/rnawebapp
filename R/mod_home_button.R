#' home_button UI Function
#'
#' @description A reusable "Return to Homepage" button module.
#'
#' @param id The module ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon actionButton fluidRow tags
mod_home_button_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$div(
        style = "text-align:right; padding: 30px",
        actionButton(
          inputId = ns("return_home"),
          label = tagList(icon("home"), "Return to Homepage"),
          class = "home-button"
        )
      )
    )
  )
}

#' home_button Server Function
#'
#' @param id Module ID
#' @param go_to_index A function to route back to the landing/home page
#'
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent
mod_home_button_server <- function(id, go_to_index) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$return_home, {
      go_to_index()
    })
  })
}

## To be copied in the UI
# mod_home_button_ui("home_button_1")

## To be copied in the server
# mod_home_button_server("home_button_1")

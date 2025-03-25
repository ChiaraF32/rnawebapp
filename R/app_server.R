#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # A reactive value to track which "page" we're on
  page <- reactiveVal("index") #Default = landing page

  # Render the UI for the current page
  output$main_ui <- renderUI({ #dynamically outputs UI based on page()
    switch(page(), #selects which UI to display
           "index" = mod_index_ui("index_1"),
           "upload" = mod_upload_ui("upload_1"),
           # Future: "processing" = mod_processing_ui("processing_1")
           )
  })

  #Handle landing page events
  mod_index_server("index_1", go_to_upload = function() page("upload"))

  # Upload server
  mod_upload_server("upload_1")
}

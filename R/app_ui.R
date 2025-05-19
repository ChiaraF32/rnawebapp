#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      uiOutput("main_ui"), #Define ui output to dynamically display one module at a time
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    # Favicon (optional)
    tags$link(rel = "shortcut icon", href = "dna-medical-svgrepo-com.png"),

    # Custom CSS styles
    tags$link(rel = "stylesheet", type = "text/css", href = "www/buttons.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/progress-nav.css")
  )
}

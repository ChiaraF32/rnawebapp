#' layout
#'
#' @description Wrap page content with consistent layout
#'
#' @param ... Main content of the page
#' @param nav_id ID for the navigation button module
#' @param home_id ID for the home button module
#' @param next_page Function to call when "proceed" is clicked
#' @param previous_page Function to call when "return" is clicked
#' @param go_to_home Function to go back to home page
#'
#' @return A tagList with layout
#' @noRd
#'
#' @importFrom shiny tagList div fluidRow column
util_page_layout <- function(..., nav_id = NULL, home_id = NULL,
                             next_page = NULL, previous_page = NULL, go_to_home = NULL) {
  tagList(
    div(
      style = "padding: 30px;",
      ...
    ),
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 20px 30px;",
          if (!is.null(home_id)) mod_home_button_ui(home_id),
          if (!is.null(nav_id)) mod_nav_buttons_ui(nav_id)
        )
      )
    )
  )
}

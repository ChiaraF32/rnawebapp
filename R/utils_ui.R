#' Page layout util
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
util_page_layout <- function(..., nav_id = NULL, home_id = NULL) {

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

#' Render a single step status UI element to use during the data processing module `mod_process.R`
#'
#' @param step_name Character: label shown in the UI
#' @param completed_states Character vector of states considered "done"
#' @param current_state Current value of processing_state()
#'
#' @return A tags$p UI element with status icon and label
#' @noRd
#'
#' @importFrom glue glue
#' @importFrom shiny tags

render_step_ui <- function(step_name, completed_states, current_state) {
  if (current_state %in% completed_states) {
    tags$p(glue::glue("✅ {step_name}... Done"))
  } else if (length(completed_states) > 0 && current_state == completed_states[1]) {
    tags$p(glue::glue("⏳ {step_name}..."))
  } else {
    tags$p(glue::glue("⏹️ {step_name}... Waiting"))
  }
}

#' UI progress navigator
#'
#' @param current_step String indicating the active step (must match one of the `steps`)
#' @param steps Character vector of step labels
#'
#' @return A 'taglist' with the progress navigator UI
#'
#' @description A utils function for building a visual progress navigator
#'
#' @noRd

util_progress_bar <- function(current_step, steps = c("Upload", "Processing", "Parameters", "Results")) {
  tagList(
    tags$div(
      class = "progress-nav",
      lapply(seq_along(steps), function(i) {
        tags$div(
          class = paste(
            "progress-step",
            if (steps[i] == current_step) "active" else "inactive"
          ),
          steps[i]
        )
      })
    ),
    tags$hr()
  )
}

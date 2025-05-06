#' Render a single step status UI element
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

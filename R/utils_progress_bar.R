#' progress navigator
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

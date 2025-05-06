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

#' Utility to create a DT::datatable with truncated long-text columns
#'
#' @param data A data.frame to display
#' @param truncate_columns A character vector of column names to truncate
#' @param max_width Max width in pixels for truncated columns (default: 300)
#'
#' @return A DT::datatable object
#' @importFrom DT datatable JS
util_trunc_dt <- function(data, truncate_columns = NULL, max_width = 300) {
  # Check if specified columns exist
  truncate_columns <- intersect(truncate_columns, colnames(data))

  # Map column names to zero-based column indices
  target_indices <- which(colnames(data) %in% truncate_columns) - 1

  # Generate columnDefs for each target column
  column_defs <- lapply(target_indices, function(index) {
    list(
      targets = index,
      render = DT::JS(
        sprintf(
          "function(data, type, row, meta) {
             return '<div style=\"white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: %dpx;\" title=\"' + data + '\">' + data + '</div>';
           }", max_width
        )
      )
    )
  })

  DT::datatable(
    data,
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      columnDefs = column_defs
    ),
    escape = FALSE,
    rownames = FALSE
  )
}

#' Utility to create a scrollable DT::datatable with nowrap styling
#'
#' @param data A data.frame to display
#' @param page_length Number of rows per page
#' @param nowrap_columns Optional character vector of column names to apply nowrap styling
#'
#' @return A DT::datatable object
#' @importFrom DT datatable JS
util_nowrap_dt <- function(data, page_length = 10, nowrap_columns = NULL) {
  columnDefs <- list()

  if (!is.null(nowrap_columns)) {
    columnDefs <- lapply(
      which(colnames(data) %in% nowrap_columns) - 1,  # 0-based index
      function(i) {
        list(
          targets = i,
          className = "dt-nowrap"
        )
      }
    )
  }

  DT::datatable(
    data,
    options = list(
      scrollX = TRUE,
      pageLength = page_length,
      autoWidth = TRUE,
      columnDefs = columnDefs
    ),
    escape = TRUE,
    rownames = FALSE,
    class = "display nowrap"
  )
}

#' export_excel UI Function
#'
#' @param id Namespace ID
#'
#' @noRd
#'
#' @importFrom shiny downloadButton NS
mod_export_excel_ui <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download_excel"), "Download Excel", class = "btn btn-success")
}

#' export_excel Server Function
#'
#' @param id Module ID
#' @param fraser_data Reactive expression returning a data.frame or tibble
#' @param outrider_data Reactive expression returning a data.frame or tibble
#' @param enabled Reactive expression that determines whether download is active
#'
#' @noRd
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable saveWorkbook
mod_export_excel_server <- function(id, fraser_data, outrider_data, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("Cohort_Results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(enabled())

        fraser_tbl <- fraser_data()
        outrider_tbl <- outrider_data()

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "FRASER Results")
        openxlsx::addWorksheet(wb, "OUTRIDER Results")

        openxlsx::writeDataTable(wb, "FRASER Results", fraser_tbl)
        openxlsx::writeDataTable(wb, "OUTRIDER Results", outrider_tbl)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}

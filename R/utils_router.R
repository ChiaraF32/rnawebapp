#' router
#'
#' @description Utility functions for app-wide page routing.
#'
#' This file defines route constants and a UI render helper for switching between
#' pages in a golem app using `reactiveVal()`-based navigation.
#'
#' @return Nothing directly - this file defines reusable constants and functions
#'
#' @noRd

# Named routes for clarity and typo-proof switching
ROUTES <- list(
  INDEX = "index",
  UPLOAD = "upload",
  PROCESSING = "processing",
  PARAMETERS = "parameters",
  INDIVIDUAL_RES = "individual_res",
  COHORT_RES = "cohort_res",
  TEST = "test"
)

#' Render the correct UI for the current route
#'
#' @param current_page A character string representing the current page (e.g. "index", "upload")
#'
#' @return A UI component corresponding to the active page
#' @noRd
#'
#' @importFrom shiny div
render_router_ui <- function(current_page) {
  switch(current_page,
         "index" = mod_index_ui("index_1"),
         "upload" = mod_upload_ui("upload_1"),
         "processing" = mod_process_ui("process_1"),
         "parameters" = mod_parameters_ui("parameters_1"),
         "individual_res" = mod_individual_res_ui("individual_res_1"),
         "cohort_res" = mod_cohort_res_ui("cohort_res_1"),
         "test" = mod_test_ui("mod_test_1"),
         div("404: Page Not Found")
  )
}

#' Create a navigation callback to a given route
#'
#' @param route A string, one of the defined ROUTES
#' @param page_reactive A reactiveVal() used to hold the current page state
#'
#' @return A function that updates the page reactive when called
#' @noRd
#'
navigate_to <- function(route, page_reactive) {
  force(route)  # ensure route is captured correctly
  force(page_reactive)
  function() {
    page_reactive(route)
  }
}

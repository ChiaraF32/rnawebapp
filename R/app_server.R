#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Create a shared, reactive container for uploaded data
  uploaded_data <- reactiveValues(
    samplesheet = NULL,
    outrider = NULL,
    fraser = NULL,
    vcf = NULL,
    fusions = NULL
  )

  # A reactive value to track which "page" we're on
  page <- reactiveVal(ROUTES$LANDING) #Default = landing page

  # Render the UI for the current page
  output$main_ui <- renderUI({ #dynamically outputs UI based on page()
    render_router_ui(page())
  })

  #Handle landing page events
  mod_index_server("index_1", go_to_upload = navigate_to(ROUTES$UPLOAD, page))
  mod_upload_server("upload_1", go_to_processing = navigate_to(ROUTES$PROCESSING, page), uploaded_data = uploaded_data)
  mod_process_server("process_1", go_to_parameters = navigate_to(ROUTES$PARAMETERS, page), uploaded_data = uploaded_data)
  mod_parameters_server("parameters_1", go_to_individual_res = navigate_to(ROUTES$INDIVIDUAL_RES, page), go_to_cohort_res = navigate_to(ROUTES$COHORT_RES, page), uploaded_data = uploaded_data)
  mod_individual_res_server("individual_res_1", go_to_parameters = navigate_to(ROUTES$PARAMETERS, page), uploaded_data = uploaded_data)
  mod_cohort_res_server("cohort_res_1", go_to_parameters = navigate_to(ROUTES$PARAMETERS, page), uploaded_data = uploaded_data)
}

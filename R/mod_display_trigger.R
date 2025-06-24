# UI
mod_display_trigger_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("trigger"), label = "Display Results", class = "btn btn-success")
}

# Server
mod_display_trigger_server <- function(id, reset_when = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    trigger <- reactiveVal(FALSE)

    observeEvent(input$trigger, {
      trigger(TRUE)
    })

    observeEvent(reset_when(), {
      trigger(FALSE)
    })

    return(trigger)
  })
}

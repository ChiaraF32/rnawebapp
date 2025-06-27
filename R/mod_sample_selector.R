# UI
mod_sample_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sample_select")),
    textOutput(ns("phenotype_display"))
  )
}

# Server
mod_sample_selector_server <- function(id, samplesheet) {
  moduleServer(id, function(input, output, session) {
    selected_sample <- reactiveVal(NULL)

    output$sample_select <- renderUI({
      req(samplesheet())
      selectInput(
        session$ns("sample"),
        "Choose Sample",
        choices = samplesheet()$RNA_ID,
        selected = NULL
      )
    })

    observeEvent(input$sample, {
      selected_sample(input$sample)
    })

    output$phenotype_display <- renderText({
      req(samplesheet(), selected_sample())
      row <- samplesheet()[samplesheet()$RNA_ID == selected_sample(), ]
      paste0("Phenotype: ", row$PHENOTYPE)
    })

    return(selected_sample)
  })
}

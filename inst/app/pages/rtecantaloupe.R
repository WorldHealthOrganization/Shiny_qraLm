rtecantaloupe_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tabPanel(
    "RTE Cantaloupe",
    fluidRow(
             uiOutput(ns("sidebar")),
             uiOutput(ns("stages"))
    )
  )
}

rtecantaloupe_server <- function(input, output, session, id) {
  # Define the processing stages for RTE Cantaloupe
  
  ca_stages <- c("Production", "Harvest", "Brushing", "Storing", "Washing", "Dicing",
                 "Partitioning", "Testing", "Transport", "RTEStorage",
                 "ConsumersTransport", "StoredDices", "Risk")
  
  ca_sidebar_server(input, output, session, id, ca_stages)
  ca_stages_server(input, output, session, id, ca_stages)
  
  observeEvent(input$resetCA, {
    shinyjs::reset("rtecantaloupe-sidebar-inputs")
  })
  
}

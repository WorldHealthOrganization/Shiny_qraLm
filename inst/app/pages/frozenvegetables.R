frozenvegetables_ui <- function(id) {
  ns = NS(id)
  shinyjs::useShinyjs()
  tabPanel(
    "Frozen Vegetables",
    fluidRow(
      uiOutput(ns("sidebar")),
      uiOutput(ns("stages"))
    )
  )
}

frozenvegetables_server <- function(input, output, session, id) {
  
  fv_stages <- c("Production", "Blanching", "Partitioning", "Testing", "Portioning", "Defrosting", "Cooking", "Risk")
  
  fv_sidebar_server(input, output, session, id, fv_stages)
  
  fv_stages_server(input, output, session, id, fv_stages)
  
  observeEvent(input$resetFV, {
    shinyjs::reset("frozenvegetables-sidebar-inputs")
  })
  
}

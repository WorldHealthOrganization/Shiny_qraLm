smokedfish_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tabPanel(
    "Smoked Fish",
    fluidRow(
             uiOutput(ns("sidebar")),
             uiOutput(ns("stages"))
          )
  )
}

smokedfish_server <- function(input, output, session, id) {
  # Define the processing stages for smoked fish
  sf_stages <- c("Production", "Prefilleting", "Filleting",
                 "Holding", "BriningOrSalting", "Smoking", 
                 "Slicing", "Packaging", "Characteristics", "ColdChain", 
                 "Home", "Portioning", "Risk")
  
  sf_sidebar_server(input, output, session, id, sf_stages)
  sf_stages_server(input, output, session, id, sf_stages)
  
  observeEvent(input$resetSF, {
    shinyjs::reset("smokedfish-sidebar-inputs")
  })

}

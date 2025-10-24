smokedfish_ui <- function(id) {
  ns <- NS(id)
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
                 "Holding", "Brining Or Salting", "Smoking", 
                 "Slicing", "Packaging", "Testing", 
                 "Characteristics", "Cold Chain", 
                 "Home", "Portioning", "Risk")
  
  # Source the sidebar module code locally
  source("pages/smokedfish/sidebar.R", local = TRUE)
  sf_sidebar_server(input, output, session, id, sf_stages)
  
  # Source the stages module code locally
  source("pages/smokedfish/stages.R", local = TRUE)
  sf_stages_server(input, output, session, id, sf_stages)
  

}


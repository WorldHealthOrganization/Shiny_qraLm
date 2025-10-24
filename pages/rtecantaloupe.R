rtecantaloupe_ui <- function(id) {
  ns <- NS(id)
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
                 "Partitioning", "Testing", "Transport", "Storage",
                 "Consumers Transport", "StoredDices", "Risk")
  
  # Source the sidebar module code locally to avoid global namespace pollution
  source("pages/rtecantaloupe/sidebar.R", local = TRUE)
  ca_sidebar_server(input, output, session, id, ca_stages)
  
  # Source the stages module code locally
  source("pages/rtecantaloupe/stages.R", local = TRUE)
  ca_stages_server(input, output, session, id, ca_stages)
  
}


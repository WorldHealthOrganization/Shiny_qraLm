ca_sidebar_ui <- function(id) {
  ns <- NS(id)
  column(
    width = 4,
    wellPanel(
      uiOutput(ns("selector")),
      h3("Select Parameters"),
      uiOutput(ns("inputs"))
    )
  )
}

ca_sidebar_server <- function(input, output, session, suffix, ca_stages) {
  ns <- NS(suffix)
  sidebar_id <- ns("sidebar")
  
  output[[sidebar_id]] <- renderUI({
    ca_sidebar_ui(sidebar_id)
  })
  
  # Source external scripts locally to avoid polluting the global namespace
  source("pages/rtecantaloupe/sidebar/stageSelector.R", local = TRUE)
  ca_stageSelector_server(input, output, session, sidebar_id, ca_stages)
  
  source("pages/rtecantaloupe/sidebar/stageInputs.R", local = TRUE)
  ca_stageInputs_server(input, output, session, sidebar_id, ca_stages)
}


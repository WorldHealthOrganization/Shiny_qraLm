sf_sidebar_ui <- function(id) {
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

sf_sidebar_server <- function(input, output, session, suffix, sf_stages) {
  ns <- NS(suffix)
  sidebar_id <- ns("sidebar")
  
  # Render the sidebar UI with the namespaced sidebar_id
  output[[sidebar_id]] <- renderUI({
    sf_sidebar_ui(sidebar_id)
  })
  
  # Source external scripts locally to avoid polluting the global namespace
  source("pages/smokedfish/sidebar/stageSelector.R", local = TRUE)
  sf_stageSelector_server(input, output, session, sidebar_id, sf_stages)
  
  source("pages/smokedfish/sidebar/stageInputs.R", local = TRUE)
  sf_stageInputs_server(input, output, session, sidebar_id, sf_stages)
}


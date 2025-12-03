fv_sidebar_ui <- function(id) {
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

fv_sidebar_server <- function(input, output, session, suffix, fv_stages) {
  ns <- NS(suffix)
  sidebar_id <- ns("sidebar")
  
  # Render the sidebar UI using the namespaced sidebar_id
  output[[sidebar_id]] <- renderUI({
    fv_sidebar_ui(sidebar_id)
  })
  
  fv_stageSelector_server(input, output, session, sidebar_id, fv_stages)
  fv_stageInputs_server(input, output, session, sidebar_id, fv_stages)
}

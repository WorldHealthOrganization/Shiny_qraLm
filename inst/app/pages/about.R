about_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h1(
          HTML("<strong>WHO Risk Estimation Tool for <i>Listeria monocytogenes</i> in Foods</strong>"),
          style = "text-align:center; color: #0093D5; padding-top:20px;"
        ),
        tabsetPanel(
          tabPanel(
            "Tool description",
            uiOutput(ns("overview"))
          ),
          tabPanel(
            "Acknowledgements",
            uiOutput(ns("acknowledgements"))
          )
        )
      )
    )
  )
}

about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    render_md <- function(path) {
      if (!file.exists(path)) {
        return(HTML("<p><em>Content not available.</em></p>"))
      }
      includeMarkdown(path)
    }
    
    output$overview <- renderUI({
      render_md("www/content/about_overview.md")
    })
    
    output$acknowledgements <- renderUI({
      render_md("www/content/about_acknowledgements.md")
    })
  })
}

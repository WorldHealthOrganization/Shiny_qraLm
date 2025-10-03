# UI
prevWithinlotsUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("prev_within_lot"), height = 'auto', width = 'auto')
}  

# Server
prevWithinlotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_within_lot <- plotly::renderPlotly({
      req(data())  # Ensure data() is available
      
      if (!"betaGen" %in% names(data())) return(NULL)  # Handle missing betaGen
      
      df <- data.frame(betaGen = data()$betaGen)
      
      p <- ggplot(df, aes(x = betaGen, y = after_stat(density))) +
        theme_minimal() +
        geom_histogram(fill = "brown", bins = 20)
      
      plotly::ggplotly(p) |>
        plotly::layout(
          title = "Variability in Within-Lot Prevalence",
          xaxis = list(title = "Prevalence"),
          yaxis = list(title = "Frequency"),
          margin = list(l = 50, r = 50, t = 50, b = 50)  # Adjusted bottom margin
        )
    })
  })
}

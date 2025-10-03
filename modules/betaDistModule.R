# UI Function
betaDistUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("betadist"), height = 'auto', width = 'auto')
}  

# Server Function
betaDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$betadist <- plotly::renderPlotly({
      req(data())  # Ensure data() is available before proceeding
      
      # Create ggplot line plot
      p <- ggplot(data(), aes(x = prev, y = prob)) +
        geom_line(color = "blue") +
        theme_minimal() +
        labs(x = "Prevalence", y = "Probability")
      
      # Convert to interactive Plotly
      plotly::ggplotly(p, width = 500, height = 500) |>
        layout(
          title = "",
          xaxis = list(title = "Prevalence"),  # Fixed axis labels
          yaxis = list(title = "Probability"), 
          margin = list(l = 50, r = 50, t = 50, b = 150)
        )
    })
  })
}

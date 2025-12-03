#' Beta distribution UI
#'
#' Provides a Plotly output for visualising beta-distribution shapes.
#'
#' @param id Shiny module namespace identifier.
#' @return Plotly output container.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
betaDistUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("betadist"), height = 'auto', width = 'auto')
}  

#' Beta distribution server
#'
#' Renders a beta distribution line plot based on supplied data.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning beta density values.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
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

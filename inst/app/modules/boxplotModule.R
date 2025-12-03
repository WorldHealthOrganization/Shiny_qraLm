#' Boxplot module UI
#'
#' Creates the Plotly output placeholder for contamination boxplots.
#'
#' @param id Shiny module namespace identifier.
#' @return A Plotly output object.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
boxplotUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("boxPlot"), height = 'auto', width = 'auto')
}  

#' Boxplot module server
#'
#' Renders contamination distributions as boxplots.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning simulation results.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
boxplotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$boxPlot <- plotly::renderPlotly({
      req(data())  # Ensure data() is available
      req(data()$lotMeans)  # Ensure lotMeans exists
      
      lotMeans <- data()$lotMeans
      
      # Create dataframe
      df <- data.frame(Counts = lotMeans)
      
      # Safe log transformation
      df$logCounts <- log1p(df$Counts)  # log1p(x) = log(1 + x), avoids log(0) issues
      
      # Create boxplot
      p <- ggplot(df, aes(x = "", y = Counts)) +  # x = "" ensures categorical x-axis
        theme_minimal() +
        geom_boxplot() +
        labs(y = "Counts", x = "")
      
      # Convert to interactive Plotly
      plotly::ggplotly(p, width = 500, height = 500) |>
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Counts"),
          margin = list(l = 50, r = 50, t = 50, b = 150)
        )
    })
  })
}

#' Risk distribution UI
#'
#' Creates the Plotly container for risk boxplot + histogram pair.
#'
#' @param id Shiny module namespace identifier.
#' @return Plotly output object.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
riskDistUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("risk_dist"))
}

#' Risk distribution server
#'
#' Plots log10 risk distributions for simulated lots.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning `lotMeanRisk`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
riskDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$risk_dist <- plotly::renderPlotly({
      req(data())  # Ensure data() is available
      
      if (!"lotMeanRisk" %in% names(data()) || is.null(data()$lotMeanRisk) || !is.vector(data()$lotMeanRisk) || length(data()$lotMeanRisk) == 0) {
        return(NULL)
      }
      
      wRiskLotMean <- data()$lotMeanRisk
      
      # Safe log10 transformation
      wRiskLotMeanlog <- ifelse(wRiskLotMean > 0, log10(wRiskLotMean), NA)
      
      df <- data.frame(risklog = wRiskLotMeanlog)
      
      box <- plotly::plot_ly(x = ~df$risklog, 
                             type = "box", name = "log10 Risk")
      
      histo <- plotly::plot_ly(x = ~df$risklog, 
                               type = "histogram",
                               histnorm = "probability")
      
      plotly::subplot(box, histo,
                      nrows = 2,
                      heights = c(0.3, 0.7),
                      shareX = TRUE, 
                      shareY = FALSE  # Fixed: Histogram & boxplot should not share Y-axis
      ) |>
        plotly::layout(
          title = "Risk Distribution", 
          showlegend = FALSE,
          xaxis = list(title = "log10 Risk"),
          yaxis = list(
            title = "Probability",
            titlefont = list(size = 14, color = "black"),
            tickfont = list(size = 12, color = "black")
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)  # Adjusted bottom margin
        )
    })
  })
}

#' ECDF lots UI
#'
#' Produces the Plotly output container for cumulative distributions.
#'
#' @param id Shiny module namespace identifier.
#' @return Plotly output object.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
ecdfLotsUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("ecdf_lots"), height = 'auto', width = 'auto')
}  

#' ECDF lots server
#'
#' Renders the empirical cumulative distribution of lot concentrations.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive simulation output.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
ecdfLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$ecdf_lots <- plotly::renderPlotly({
      #    isolate({
      #    cat("variability of contamination in lots (CFU/g)\n")
      req(data())
      
      lotN <- data()$lotMeans
      
      if (is.null(lotN) || !is.vector(lotN) || length(lotN) == 0) {
        return(NULL)
      }
      
      df <- data.frame(Counts= lotN) # weighted cels/g per lot
      
      # logs function
      log_var <- function(x) {
        ifelse(x != 0, log10(x), 0)
      }
      
      df$logCounts <- log_var(df$Counts)
      p <- ggplot2::ggplot(df, aes(x=logCounts)) +
        theme_minimal() +
        stat_ecdf(geom = "step", col="blue", linewidth=1.5)
      
      plot <- plotly::ggplotly(p) |>
        plotly::layout(title = "",
                       xaxis = list(title = "Counts (log10 CFU/g)"),
                       yaxis = list(title = "Cumulative Probability")#,
                       #              margin = list(l = 50, r = 50, t = 50, b = 150)
        )
      return(plot)
      #    })
      
    })
  })
}

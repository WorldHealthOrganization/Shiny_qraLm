#' Between-lot counts distribution UI
#'
#' Displays histogram/boxplot summaries for lot-level contamination.
#'
#' @param id Shiny module namespace identifier.
#' @return Plotly output object.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
countsLotsDistUI <- function(id) {
  ns <- NS(id)
   plotly::plotlyOutput(ns("counts_lots_dist"))
}  

#' Between-lot counts distribution server
#'
#' Renders the lot-level contamination distribution as histogram + boxplot.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning simulation outputs.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
countsLotsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$counts_lots_dist <- plotly::renderPlotly({
  
    CFU <- data()$lotMeans
    if (is.null(CFU) || !is.vector(CFU) || length(CFU) == 0) {
      return(NULL)
    }
    df <- data.frame(CFU=CFU)
    
    histo <- plotly::plot_ly(x = ~df$CFU,
                     type = "histogram", nbinsx = 25,          
                     histnorm = "probability")
    
    box <- plotly::plot_ly(x = ~df$CFU, type = "box") 
    
    plot <- plotly::subplot(box, histo,
                            nrows = 2,
                            heights = c(0.2,0.8),
                            shareX = TRUE, 
                            titleX = TRUE) |>
      plotly::layout(title = "", showlegend = FALSE,
                     xaxis = list(title = "CFU/g"),
                     yaxis = list(title = "Probability"))
    
    return(plot)
      })
   })
}

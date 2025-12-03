#' Within-unit counts distribution UI
#'
#' Provides a Plotly container for unit-level contamination distributions.
#'
#' @param id Shiny module namespace identifier.
#' @return Plotly output object.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
countsUnitsDistUI <- function(id) {
  ns <- NS(id)
   plotly::plotlyOutput(ns("counts_units_dist"))
}  

#' Within-unit counts distribution server
#'
#' Renders histograms/boxplots for unit-level counts excluding zeros.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning simulation outputs.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
countsUnitsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  output$counts_units_dist <- plotly::renderPlotly({
   
    CFU <- data()$unitsCounts
    if (is.null(CFU) || !is.vector(CFU) || length(CFU) == 0) {
      return(NULL)
    }
    
    index <- which(CFU==0)
      if (length(index)==0) {
       PosServings <- CFU
     } else {
       PosServings <- CFU[-index]
     }
    df <- data.frame(counts=PosServings)
  
    histo <- plotly::plot_ly(x = ~df$counts, 
                     type = "histogram", nbinsx = 25,          
                     histnorm = "probability")
    
    box <- plotly::plot_ly(x = ~df$counts, type = "box") 
    
    plot <- plotly::subplot(box, histo,
                            nrows = 2,
                            heights = c(0.2,0.8),
                            shareX = TRUE, 
                            titleX = TRUE) |>
      plotly::layout(title = "", showlegend = FALSE,
                     xaxis = list(title = "CFU/g"),
                     yaxis = list(title = "Probability")
                     )
    return(plot)
      })
   })
}

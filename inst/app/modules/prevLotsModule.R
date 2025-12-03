#' Lot prevalence UI
#'
#' Displays textual prevalence summaries for lots.
#'
#' @param id Shiny module namespace identifier.
#' @return `verbatimTextOutput`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
prevLotsUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("prev_lots"))
}  

#' Lot prevalence server
#'
#' Prints prevalence values based on the supplied dataset.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression containing `P`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
prevLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_lots <- renderPrint({
      req(data())  # Ensure data() is available
      if (!"P" %in% names(data())) return("No prevalence data available")  # Handle missing P
      
      data()$P  # Simply return P without using return()
    })
    
  })
}

#' Beta distribution data UI
#'
#' Builds numeric inputs to parameterize a beta distribution.
#'
#' @param id Shiny module namespace identifier.
#' @return A set of numeric inputs for beta parameters.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
betaDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("size_lot"), "Lot Size", value = 100, min = 1, step = 1),
    numericInput(ns("beta_alpha"), "Beta Alpha", value = 2, min = 0.1, step = 0.1),
    numericInput(ns("beta_beta"), "Beta Beta", value = 5, min = 0.1, step = 0.1)
  )
}  

#' Beta distribution data server
#'
#' Computes beta distribution coordinates based on user inputs.
#'
#' @param id Shiny module namespace identifier.
#' @return Reactive `data.frame` with prevalence/probability columns.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
betaDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <- reactive({
      req(input$size_lot, input$beta_alpha, input$beta_beta)  # Ensure inputs exist
      
      prev <- seq(0, 1, length.out = input$size_lot)
      prob <- dbeta(prev, shape1 = input$beta_alpha, shape2 = input$beta_beta)
      
      data.frame(prev = prev, prob = prob)
    })
    
    return(df)  # Return the reactive dataframe
  })
}

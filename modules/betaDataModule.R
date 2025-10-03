# UI Function
betaDataUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("size_lot"), "Lot Size", value = 100, min = 1, step = 1),
    numericInput(ns("beta_alpha"), "Beta Alpha", value = 2, min = 0.1, step = 0.1),
    numericInput(ns("beta_beta"), "Beta Beta", value = 5, min = 0.1, step = 0.1)
  )
}  

# Server Function
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

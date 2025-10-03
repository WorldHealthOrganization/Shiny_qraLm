# Define the UI
LotGenUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("n_lots"), "Number of lots", value = 1000, min = 1000, max = 5000, step = 500),
    sliderInput(ns("size_lot"), "Lot size (Number of units)", value = 1000, min = 1000, max = 5000, step = 500),
    sliderInput(ns("unit_size"), "Unit size (g):", value = 500, min = 100, max = 1000, step = 100),
    sliderInput(ns("prev"), "Prevalence of contaminated lots", value = 0.057, min = 0.0, max = 1.0, step = 0.10),
    sliderInput(ns("log_mean_c"), "Mean of Counts (log10 CFU/g)", value = 1.023, min = 0.10, max = 5.000, step = 0.100),
    sliderInput(ns("log_sd_c"), "St. dev. of Counts (log10 CFU/g)", value = 0.3267, min = 0.00, max = 2.00, step = 0.100),
    actionButton(ns("simulate"), "Simulate")  # Added missing button
  )
}

# Define the server
LotGenServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <- reactive({
      req(input$simulate)  # Ensure button is clicked before generating data
      LotGen(nLots     = input$n_lots,
             sizeLot   = input$size_lot,  # Fixed incorrect input reference
             unitSize  = input$unit_size,
             P         = input$prev,
             C0MeanLog = input$log_mean_c,
             C0SdLog   = input$log_sd_c)
    })
    
    observeEvent(input$simulate, {
      print(df())  # Example: Output to console, replace with actual use
    })
    
    return(df)  # Return the reactive expression for use elsewhere
  })
}

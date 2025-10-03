# Define the UI
prevUnitsUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("prev_units"))
}  

# Define the Server
prevUnitsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_units <- renderPrint({
      req(data())  # Ensure data() is available
      
      # Check if required elements exist
      if (!"N" %in% names(data())) return("No unit count data available")
      
      # Check if ProbUnitPos exists in data
      has_probunitpos <- "ProbUnitPos" %in% names(data())
      
      if (has_probunitpos) {
        prevUnits <- stats::weighted.mean(
          data()$ProbUnitPos * (data()$N != 0),
          w = rep(1, length(data()$N))
        )
      } else {
        probunitpos <- rep(1, length(data()$N))
        prevUnits <- stats::weighted.mean(
          probunitpos * (data()$N != 0),
          w = rep(1, length(data()$N))
        )
      }
      
      prevUnits  # No need for return()
    })
    
  })
}

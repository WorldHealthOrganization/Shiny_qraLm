# Define the UI
prevLotsUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("prev_lots"))
}  

# Define the Server
prevLotsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$prev_lots <- renderPrint({
      req(data())  # Ensure data() is available
      if (!"P" %in% names(data())) return("No prevalence data available")  # Handle missing P
      
      data()$P  # Simply return P without using return()
    })
    
  })
}

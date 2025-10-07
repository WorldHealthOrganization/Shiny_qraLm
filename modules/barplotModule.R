
# UI Function
barplotUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("barPlot"), height = 'auto', width = 'auto')
}

# Server Function
barplotServer <- function(id, data, Stage) {
  moduleServer(id, function(input, output, session) {
    
    output$barPlot <- plotly::renderPlotly({
      req(data())  # Ensure data is available
      
      # Ensure Stage is handled correctly (if it's reactive, call Stage())
      stage_val <- if (is.reactive(Stage)) Stage() else Stage
      
      # Create data frame
      dat <- data.frame(
        Stage = stage_val, 
        P = data()$P
      )
      
      # Create ggplot bar chart
      bar <- ggplot(dat, aes(x = Stage, y = P)) +
        geom_bar(stat = "identity", width = 0.35, fill = "steelblue") +
        theme_minimal() +
        xlab("") + 
        ylab("Prevalence") +
        theme(legend.position = "none")
      
      # Convert ggplot to interactive plotly
      plotly::ggplotly(bar, width = 500, height = 500) |>
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "Prevalence"),
          margin = list(l = 50, r = 50, t = 50, b = 150)
        )
    })
  })
}

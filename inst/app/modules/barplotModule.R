#' Barplot module UI
#'
#' Provides the Plotly output placeholder for the prevalence barplot.
#'
#' @param id Shiny module namespace identifier.
#' @return A `plotlyOutput` ready to be rendered.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
barplotUI <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("barPlot"), height = 'auto', width = 'auto')
}

#' Barplot module server
#'
#' Renders a Plotly bar chart summarizing prevalence per stage.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning simulation outputs.
#' @param Stage Either a character label or reactive stage name.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
barplotServer <- function(id, data, Stage) {
  moduleServer(id, function(input, output, session) {
    
    output$barPlot <- plotly::renderPlotly({
      req(data())  # Ensure data is available
      
      # Ensure Stage is handled correctly (if it's reactive, call Stage())
      stage_val <- if (is.reactive(Stage)) Stage() else Stage
      
      if (is.null(stage_val)) {
        return(NULL)
      }
      
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

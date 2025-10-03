## LM Concentration in the contaminated lots
# Ui
logCountsDistUI <- function(id) {
  ns <- NS(id)
 
   plotly::plotlyOutput(ns("logCountsDist"), height = 'auto', width = 'auto')
  
}  

# server
logCountsDistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$logCountsDist <- plotly::renderPlotly({
      # Validate dataset
      if (is.null(data()) || !"lotMeans" %in% colnames(data())) {
        return(plotly::plot_ly() |>
                 plotly::layout(title = "Error: Invalid dataset"))
      }
      
      Nlot <- data()$lotMeans
      
      # Handle empty data
      if (length(Nlot) == 0 || all(Nlot == 0)) {
        return(plotly::plot_ly() |>
                 plotly::layout(title = "No data available"))
      }
      
      # Create dataframe
      df <- data.frame(Counts = Nlot)
      
      # Compute log10 of counts
      log_risk <- function(x) {
        ifelse(x != 0, log10(x), 0)
      }
      df$logCounts <- log_risk(df$Counts)
      
      # Create boxplot
      plot1 <- ggplot2::ggplot(df, ggplot2::aes(y = logCounts)) +
        ggplot2::theme_minimal() +
        ggplot2::geom_boxplot(fill = "blue", alpha = 0.2, width = 1.5) +
        ggplot2::ylab('Counts (log10 CFU/g)') +
        ggplot2::xlab('')
      
      # Create histogram
      plot2 <- ggplot2::ggplot(df, ggplot2::aes(x = logCounts)) +
        ggplot2::theme_minimal() +
        ggplot2::geom_histogram(color = "blue", bins = 25, fill = "blue", alpha = 0.6) +
        ggplot2::xlab('Counts (log10 CFU/g)') +
        ggplot2::ylab('Frequency')
      
      # Convert to plotly
      plot1 <- plotly::ggplotly(plot1, width = NULL, height = NULL)
      plot2 <- plotly::ggplotly(plot2, width = NULL, height = NULL)
      
      # Combine plots
      combined_plot <- plotly::subplot(plot1, plot2,
                                       nrows = 2,
                                       shareX = TRUE) |>
        plotly::layout(
          title = "",
          xaxis = list(title = "Counts (log10 CFU/g)"),
          yaxis = list(title = "Frequency"),
          margin = list(l = 50, r = 50, t = 50, b = 150)
        )
      
      return(combined_plot)
    })
  })
}
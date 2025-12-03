#' Histogram module UI
#'
#' Provides a base plotting area to display multiple histograms.
#'
#' @param id Shiny module namespace identifier.
#' @return Base `plotOutput`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
histUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("histograms"),  width = "100%", height = "600px")
}  

#' Histogram module server
#'
#' Draws histograms for each numeric characteristic in the dataset.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive list of vectors to plot.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
histServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$histograms <- renderPlot({
      req(data())
      # Validate dataset
      if (is.null(data()) || !is.list(data())) {
        stop("Error: Invalid dataset")
      }
      
      # Convert data to data frame
      raw_data <- do.call(cbind, data())
      if (ncol(raw_data) == 0) {
        stop("Error: No data available")
      }
      df <- as.data.frame(raw_data)
      
      # Assign column names dynamically if necessary
      default_col_names <- c("NaCl", "pH", "P", "CO2equi", "NIT", "aaWph", 
                             "baWph", "caWph", "daWph", "laWph", "saWph")
      if (length(names(df)) != length(default_col_names)) {
        names(df) <- paste0("Col_", seq_len(ncol(df)))
      } else {
        names(df) <- default_col_names
      }
      
      # Remove specified columns
      cols_to_exclude <- c("NIT", "aaWph", "baWph", "caWph", "saWph")
      df <- df[, !(names(df) %in% cols_to_exclude), drop = FALSE]
      
      # Determine grid layout dynamically
      num_cols <- ncol(df)
      rows <- ceiling(sqrt(num_cols))
      cols <- ceiling(num_cols / rows)
      par(mfrow = c(rows, cols), mar = c(2, 4, 2, 2))
      
      # Plot histograms
      plotDataFrame <- function(dataFrame) {
        for (i in seq_along(dataFrame)) {
          col_name <- names(dataFrame)[i]
          if (is.factor(dataFrame[[i]])) {
            barplot(table(dataFrame[[i]]), main = col_name, xlab = col_name, ylab = "Frequency")
          } else {
            hist(dataFrame[[i]],
                 main = col_name,
                 xlab = col_name,
                 freq = FALSE,
                 breaks = "Freedman-Diaconis",
                 col = "blue",
                 border = "brown")
          }
        }
      }
      
      plotDataFrame(df)
    })
  })
}

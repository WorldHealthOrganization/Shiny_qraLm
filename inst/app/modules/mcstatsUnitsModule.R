#' Unit statistics UI
#'
#' Builds a table output for unit-level contamination statistics.
#'
#' @param id Shiny module namespace identifier.
#' @return A `dataTableOutput`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
mcstatsUnitsUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mcstats_units"))
}  

#' Unit statistics server
#'
#' Produces descriptive stats for contaminated units (non-zero counts).
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression returning `unitsCounts`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
mcstatsUnitsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$mcstats_units <- DT::renderDataTable({
      # Extract unitsCounts
      posUnits <- data()$unitsCounts
      
      # Remove zero values
      posUnits <- posUnits[posUnits > 0]
      
      # Check if posUnits is empty
      if (length(posUnits) == 0) return(NULL)
      
      # Compute statistics
      NStatsMin    <- min(posUnits, na.rm = TRUE)
      NStatsMax    <- max(posUnits, na.rm = TRUE)
      NStatsMedian <- quantile(posUnits, probs = 0.5, na.rm = TRUE)
      NStatsMean   <- mean(posUnits, na.rm = TRUE)
      Q2.5         <- quantile(posUnits, probs = 0.025, na.rm = TRUE)
      Q97.5        <- quantile(posUnits, probs = 0.975, na.rm = TRUE)
      
      Counts <- rbind(
        unname(NStatsMin),
        unname(Q2.5),
        unname(NStatsMean),
        unname(NStatsMedian),
        unname(Q97.5),
        unname(NStatsMax)
      )
      
      # Define safe log10 function
      log_var <- function(x) {
        ifelse(x > 0, log10(x), NA)
      }
      
      logCounts <- round(log_var(Counts), digits = 4)
      
      # Prepare data table
      Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
      MCstats <- data.frame(Statistics, Counts, logCounts)
      names(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g")
      
      # Render DataTable
      DT::datatable(MCstats,
                    class = "cell-border stripe",
                    extensions = 'Buttons',
                    rownames = FALSE,
                    options = list(dom = 'Blrt')) |>
        DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
    })
  })
}

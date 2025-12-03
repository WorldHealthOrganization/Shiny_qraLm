#' Risk statistics UI
#'
#' Provides a table output for risk summary stats.
#'
#' @param id Shiny module namespace identifier.
#' @return A `dataTableOutput`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
riskStatsUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("riskstats"))
}  

#' Risk statistics server
#'
#' Calculates key percentiles for mean risk per lot (and log10 scale).
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive expression with `lotMeanRisk`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
riskStatsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$riskstats <- DT::renderDataTable({
      req(data())  # Ensure data() is available
      
      if (!"lotMeanRisk" %in% names(data())) return(NULL)  # Handle missing lotMeanRisk
      
      wRiskLotMean <- data()$lotMeanRisk
      
      # Safe log10 transformation
      wRiskLotMeanlog <- ifelse(wRiskLotMean > 0, log10(wRiskLotMean), NA)
      
      # Compute statistics
      RiskLotMin    <- min(wRiskLotMean, na.rm = TRUE)
      RiskLotMax    <- max(wRiskLotMean, na.rm = TRUE)
      RiskLotMedian <- quantile(wRiskLotMean, probs = 0.5, na.rm = TRUE)
      RiskLotMean   <- mean(wRiskLotMean, na.rm = TRUE)
      RiskLotQ2.5   <- quantile(wRiskLotMean, probs = 0.025, na.rm = TRUE)
      RiskLotQ97.5  <- quantile(wRiskLotMean, probs = 0.975, na.rm = TRUE)
      
      logRiskLotMin    <- min(wRiskLotMeanlog, na.rm = TRUE)
      logRiskLotMax    <- max(wRiskLotMeanlog, na.rm = TRUE)
      logRiskLotMedian <- quantile(wRiskLotMeanlog, probs = 0.5, na.rm = TRUE)
      logRiskLotMean   <- mean(wRiskLotMeanlog, na.rm = TRUE)
      logRiskLotQ2.5   <- quantile(wRiskLotMeanlog, probs = 0.025, na.rm = TRUE)
      logRiskLotQ97.5  <- quantile(wRiskLotMeanlog, probs = 0.975, na.rm = TRUE)
      
      Stats      <- c(RiskLotMin, RiskLotMax, RiskLotMedian,
                      RiskLotMean, RiskLotQ2.5, RiskLotQ97.5)
      logStats   <- c(logRiskLotMin, logRiskLotMax, logRiskLotMedian,
                      logRiskLotMean, logRiskLotQ2.5, logRiskLotQ97.5)
      StatsNames <- c("Minimum", "Maximum", "Median", "Mean", "pct 2.5th", "pct 97.5th")
      
      riskStats <- data.frame(Statistics = StatsNames, 
                              `Mean risk per lot` = Stats, 
                              `Mean risk per lot (log10)` = logStats,
                              check.names = FALSE)  # Prevents column name conversion
      
      DT::datatable(riskStats, 
                    class = 'cell-border stripe',
                    extensions = 'Buttons',
                    rownames = FALSE,
                    options = list(dom = 'Blrt',
                                   lengthMenu = list(c(6, 12, -1), c(6, 12, "All")))) |>
        DT::formatRound(columns = "Mean risk per lot (log10)", digits = 5) |>
        DT::formatSignif(columns = "Mean risk per lot", digits = 5)
    })
  })
}

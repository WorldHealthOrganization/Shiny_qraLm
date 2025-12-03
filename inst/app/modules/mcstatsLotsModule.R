#' Lot statistics UI
#'
#' Creates a `DT` output for lot-level descriptive stats.
#'
#' @param id Shiny module namespace identifier.
#' @return A `dataTableOutput`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
mcstatsLotsUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mcstats_lots"))
}  

#' Lot statistics server
#'
#' Computes weighted quantiles/means for the lot means.
#'
#' @param id Shiny module namespace identifier.
#' @param data Reactive simulation output containing `lotMeans`.
#' @author Vasco Cadavez (vcadavez@ipb.pt)
#' @author Ursula Gonzales-Barron (ubarron@ipb.pt)
mcstatsLotsServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    output$mcstats_lots <- DT::renderDataTable({
      lotN <- data()$lotMeans
      
      # Check if "ProbUnitPos" exists in data
      has_probunitpos <- "ProbUnitPos" %in% names(data())
      
      if (has_probunitpos) {
        NStats <- Hmisc::wtd.quantile(
          lotN,
          weights = data()$ProbUnitPos,
          probs = c(0.00, 0.50, 0.025, 0.975, 1.00),
          normwt = TRUE,
          na.rm = TRUE
        )
        
        NStatsMean <- stats::weighted.mean(lotN, w = data()$ProbUnitPos, na.rm = TRUE)
      } else {
        probunitpos <- rep(1, if ("N" %in% names(data())) nrow(data()$N) else length(lotN))
        NStats <- Hmisc::wtd.quantile(
          lotN,
          weights = probunitpos,
          probs = c(0.00, 0.50, 0.025, 0.975, 1.00),
          normwt = TRUE,
          na.rm = TRUE
        )
        
        NStatsMean <- stats::weighted.mean(lotN, w = probunitpos, na.rm = TRUE)
      }
      
      # Extract statistics
      NStatsMin    <- NStats[1]
      NStatsMedian <- NStats[2]
      Q2.5         <- NStats[3]
      Q97.5        <- NStats[4]
      NStatsMax    <- NStats[5]
      
      Counts <- rbind(
        unname(NStatsMin),
        unname(Q2.5),
        unname(NStatsMean),
        unname(NStatsMedian),
        unname(Q97.5),
        unname(NStatsMax)
      )
      
      # Log transformation function
      log_var <- function(x) {
        ifelse(x != 0, log10(x), 0)
      }
      
      logCounts <- round(log_var(Counts), digits = 5)
      
      # Define table labels
      Statistics <- c("Minimum", "pct 2.5th", "Mean", "Median", "pct 97.5th", "Maximum")
      MCstats <- data.frame(Statistics, Counts, logCounts)
      
      # Column names and formatting
      if (has_probunitpos) {
        colnames(MCstats) <- c("Statistics", "CFU/g", "log10 CFU/g")
        
        DT::datatable(MCstats,
                      class = "cell-border stripe",
                      extensions = 'Buttons',
                      rownames = FALSE,
                      options = list(dom = 'Blrt')) |>
          DT::formatSignif(columns = c("CFU/g", "log10 CFU/g"), digits = 4)
      } else {
        colnames(MCstats) <- c("Statistics", "CFU/Melon", "log10 CFU/Melon")
        
        DT::datatable(MCstats,
                      class = "cell-border stripe",
                      extensions = 'Buttons',
                      rownames = FALSE,
                      options = list(dom = 'Blrt')) |>
          DT::formatSignif(columns = c("CFU/Melon", "log10 CFU/Melon"), digits = 4)
      }
    })
  })
}

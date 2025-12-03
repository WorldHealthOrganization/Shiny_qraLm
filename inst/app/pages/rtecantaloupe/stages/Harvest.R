ca_Harvest_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_harvest"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_harvest")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_harvest")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_harvest"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_harvest")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_harvest")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_harvest")
    )
  )
}

ca_Harvest_server <- function(input, output, session, suffix, datCanta) {
  ns <- NS(suffix)
  id <- ns("Harvest")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datHarvest <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Harvest", value = 2/13)
      
      values$data <-  generate_datHarvest(input, prefix, datCanta)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datHarvest")
      prevLotsServer("prev_lots_harvest",                data=datFn)
      prevUnitsServer("prev_units_harvest",              data=datFn)
      mcstatsLotsServer("lots_mcstats_harvest",          data=datFn)
      mcstatsUnitsServer("units_mcstats_harvest",        data=datFn)
      countsLotsDistServer("counts_lots_dist_harvest",   data=datFn)
      countsUnitsDistServer("counts_units_dist_harvest", data=datFn)
      ecdfLotsServer("ecdf_prob_harvest",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCAHarvest <- c("prob_cch", "n_plas")
    lapply(inputListCAHarvest , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datHarvest()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Harvest_ui(id)
    }
  })
  
  return(datHarvest)
}

generate_datHarvest <- function(input, prefix, datCanta) {
  set.seed(get_input_value(input, prefix, "seed") + 9713)
  df <- caHarvestCC(
    datCanta(),
    probCCH = get_input_value(input, prefix, "prob_cch"),
    trMean  = -1.42,
    trSd    = 0.52,
    nPlas  = get_input_value(input, prefix, "n_plas")
    )
  return(df)
}

ca_HarvestInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Harvest"),
#  tagList(
    sliderInput(ns("prob_cch"),
                label = makeHelp("Probability of cross-contamination during harvest (<i>probCCH</i>)","caHarvestCC"),
                value = 0.25, min = 0, max = 1, step=0.1),
     sliderInput(ns("n_plas"),
                label = makeHelp("Numbers of LM on food contact surfaces (<i>nPlas</i>)", "caHarvestCC"),
                value = 9, min = 0, max = 20, step=1)
#  )
  )
}

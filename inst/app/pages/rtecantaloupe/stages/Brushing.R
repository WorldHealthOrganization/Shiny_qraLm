ca_Brushing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_brushing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_brushing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_brushing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_brushing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_brushing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_brushing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_brushing")
    )
  )
}

ca_Brushing_server <- function(input, output, session, suffix, datHarvest) {
  ns <- NS(suffix)
  id <- ns("Brushing")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datBrush <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Brushing", value = 3/13)
      
      values$data <- generate_datBrush(input, prefix, datHarvest)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datBrush")
      prevLotsServer("prev_lots_brushing",                data=datFn)
      prevUnitsServer("prev_units_brushing",              data=datFn)
      mcstatsLotsServer("lots_mcstats_brushing",          data=datFn)
      mcstatsUnitsServer("units_mcstats_brushing",        data=datFn)
      countsLotsDistServer("counts_lots_dist_brushing",   data=datFn)
      countsUnitsDistServer("counts_units_dist_brushing", data=datFn)
      ecdfLotsServer("ecdf_prob_brushing",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCABrush <- c("log_dec_brush")
    lapply(inputListCABrush , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datBrush()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Brushing_ui(id)
    }
  })
  
  return(datBrush)
}

generate_datBrush <- function(input, prefix, datHarvest) {
  set.seed(get_input_value(input, prefix, "seed") + 456)
  df <- caBrush(
    datHarvest(),
    logDecBrush  = get_input_value(input, prefix, "log_dec_brush")
    )
  return(df)
}

ca_BrushingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Brushing"),
#  tagList(
    sliderInput(ns("log_dec_brush"),
                label = makeHelp("Mean log10 reduction attained by brushing (<i>logDecBrush</i>)", "caBrush"),
                value = 0, min = 0, max = 3, step=1)
#  )
  )
}

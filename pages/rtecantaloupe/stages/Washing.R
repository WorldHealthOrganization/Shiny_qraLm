ca_Washing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_washing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_washing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_washing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_washing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_washing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_washing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_washing")
    )
  )
}

ca_Washing_server <- function(input, output, session, suffix, datStoring) {
  ns <- NS(suffix)
  id <- ns("Washing")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datWashing <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Washing", value = 5/13)
      
      values$data <- generate_datWashing(input, prefix, datStoring)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datWashing")
      prevLotsServer("prev_lots_washing",                data=datFn)
      prevUnitsServer("prev_units_washing",              data=datFn)
      mcstatsLotsServer("lots_mcstats_washing",          data=datFn)
      mcstatsUnitsServer("units_mcstats_washing",        data=datFn)
      countsLotsDistServer("counts_lots_dist_washing",   data=datFn)
      countsUnitsDistServer("counts_units_dist_washing", data=datFn)
      ecdfLotsServer("ecdf_prob_washing",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCAWashing , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datWashing()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Washing_ui(id)
    }
  })
  
  return(datWashing)
}

generate_datWashing <- function(input, prefix, datStoring) {
  set.seed(get_input_value(input, prefix, "seed") + 240974)
  df <- caFlumeTankCC(
    datStoring(),
    probCCW      = get_input_value(input, prefix, "prob_ccw"),
    logWaterMin  = get_input_value(input, prefix, "log_water_min"),
    logWaterMode = get_input_value(input, prefix, "log_water_mode"),
    logWaterMax  = get_input_value(input, prefix, "log_water_max"),
    pWaterGain   = get_input_value(input, prefix, "p_water_gain"),
    bWater       = get_input_value(input, prefix, "b_water")
    )
  return(df)
}

ca_WashingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Washing"),
#  tagList(
    sliderInput(ns("prob_ccw"),
                label = makeHelp("Probability that water of flume tank is contaminated (<i>probCCW</i>)", "caFlumeTankCC"),
                value = 0, min = 0, max = 1, step=0.1),
    sliderInput(ns("log_water_min"),
                label = makeHelp("Minimal concentration of LM in water of flume tank (<i>logWaterMin</i>)", "caFlumeTankCC"),
                value = 1, min = 0, max = 2, step=0.25),
    sliderInput(ns("log_water_mode"),
                label = makeHelp("Mode concentration of LM in water of flume tank (<i>logWaterMode</i>)", "caFlumeTankCC"),
                value = 1, min = 0, max = 2, step=0.25),
    sliderInput(ns("log_water_max"),
                label = makeHelp("Maximum concentration of LM in water of flume tank (<i>logWaterMax</i>)", "caFlumeTankCC"),
                value = 5, min = 2, max = 10, step=0.25),
    sliderInput(ns("p_water_gain"),
                label = makeHelp("Fraction of water gain (ml) relative to the cantaloupe weight (g) (<i>pWaterGain</i>)", "caFlumeTankCC"),
                value = 0.004, min = 0.0, max = 0.009, step=0.001),
    sliderInput(ns("b_water"),
                label = makeHelp("Dispersion factor - clustering of cells during washing (<i>bWater</i>)", "caFlumeTankCC"),
                value = 1, min = 0, max = 2, step=0.2)
#    )
   )
}

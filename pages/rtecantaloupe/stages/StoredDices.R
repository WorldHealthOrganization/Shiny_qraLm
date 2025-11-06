ca_StoredDices_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_storeddices"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_storeddices")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_storeddices")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_storeddices"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_storeddices")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_storeddices")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_storeddices")
    )
  )
}

ca_StoredDices_server <- function(input, output, session, suffix, datConsumersTransport) {
  ns <- NS(suffix)
  id <- ns("StoredDices")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datStoredDices <- eventReactive(input$updateCA, {
    
    is_valid <- checkPert(input, prefix, "temp_min_h", "temp_mode_h", "temp_max_h") &
      checkPert(input, prefix, "time_min_h", "time_mode_h", "time_max_h")
    if (!is_valid) {
      return(NULL)
    }
    
    
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Stored dices", value = 12/13)
      
      values$data <-  generate_datStoredDices(input, prefix, datConsumersTransport)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datStoredDices")
      prevLotsServer("prev_lots_storeddices",                data=datFn)
      prevUnitsServer("prev_units_storeddices",              data=datFn)
      mcstatsLotsServer("lots_mcstats_storeddices",          data=datFn)
      mcstatsUnitsServer("units_mcstats_storeddices",        data=datFn)
      countsLotsDistServer("counts_lots_dist_storeddices",   data=datFn)
      countsUnitsDistServer("counts_units_dist_storeddices", data=datFn)
      ecdfLotsServer("ecdf_prob_storeddices",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCAStoredDices , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datStoredDices()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_StoredDices_ui(id)
    }
  })
  
  return(datStoredDices)
}

generate_datStoredDices <- function(input, prefix, datConsumersTransport) {
  set.seed(get_input_value(input, prefix, "seed") + 3186)
  req(datConsumersTransport())
  df <- caHomeRTE(
                      datConsumersTransport(),
                      Tmin      = -2.0196,
                      tempMin   = get_input_value(input, prefix, "temp_min_h"),
                      tempMode  = get_input_value(input, prefix, "temp_mode_h"),
                      tempMax   = get_input_value(input, prefix, "temp_max_h"),
                      timeMin  = get_input_value(input, prefix, "time_min_h"),
                      timeMod  = get_input_value(input, prefix, "time_mode_h"),
                      timeMax  = get_input_value(input, prefix, "time_max_h")    )
  return(df)
}

ca_StoredDicesInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("StoredDices"),
#  tagList(
    sliderInput(ns("temp_min_h"),
                label = makeHelp("Minimum home temperature (ºC) (<i>tempMin</i>)", "caHomeRTE"),
                value = 3.1, min = 0, max = 30, step=0.5),
    sliderInput(ns("temp_mode_h"),
                label = makeHelp("Mode home temperature (ºC) (<i>tempMode</i>)", "caHomeRTE"),
                value = 6.64, min = 0, max = 30, step=0.5),
    sliderInput(ns("temp_max_h"),
                label = makeHelp("Maximum home temperature (ºC) (<i>tempMax</i>)", "caHomeRTE"),
                value = 11.1, min = 0, max = 30, step=0.5),
    sliderInput(ns("time_min_h"),
                label = makeHelp("Minimum home time (h) (<i>timeMin</i>)", "caHomeRTE"),
                value = 3, min = 0.0, max = 150, step=0.5),
    sliderInput(ns("time_mode_h"),
                label = makeHelp("Mode home time (h) (<i>timeMode</i>)", "caHomeRTE"),
                value = 24, min = 0.0, max = 150, step=0.5),
    sliderInput(ns("time_max_h"),
                label = makeHelp("Max home time (h) (<i>timeMax</i>)", "caHomeRTE"),
                value = 120, min = 0.0, max = 150, step=0.5)
#    )
  )
}

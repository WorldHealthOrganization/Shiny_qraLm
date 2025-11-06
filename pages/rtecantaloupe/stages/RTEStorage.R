ca_RTEStorage_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_rtestorage"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_rtestorage")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_rtestorage")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_rtestorage"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_rtestorage")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_rtestorage")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_rtestorage")
    )
  )
}

ca_RTEStorage_server <- function(input, output, session, suffix, datTransport) {
  ns <- NS(suffix)
  id <- ns("Storage")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datRTEStorage <- eventReactive(input$updateCA, {
    
    is_valid <- checkPert(input, prefix, "temp_min_rte", "temp_mod_rte", "temp_max_rte") &
      checkPert(input, prefix, "time_min_rte", "time_mod_rte", "time_max_rte")
    if (!is_valid) {
      return(NULL)
    }
    
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Storage", value = 10/13)
      
      values$data <- generate_datRTEStorage(input, prefix, datTransport)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datRTEStorage")
      prevLotsServer("prev_lots_rtestorage",                data=datFn)
      prevUnitsServer("prev_units_rtestorage",              data=datFn)
      mcstatsLotsServer("lots_mcstats_rtestorage",          data=datFn)
      mcstatsUnitsServer("units_mcstats_rtestorage",        data=datFn)
      countsLotsDistServer("counts_lots_dist_rtestorage",   data=datFn)
      countsUnitsDistServer("counts_units_dist_rtestorage", data=datFn)
      ecdfLotsServer("ecdf_prob_rtestorage",                data=datFn)
      
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCARTEStorage , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datRTEStorage()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_RTEStorage_ui(id)
    }
  })
  
  return(datRTEStorage)
}

generate_datRTEStorage <- function(input, prefix, datRTEStorage) {
  set.seed(get_input_value(input, prefix, "seed") + 12365)
  req(datRTEStorage())
  df <- caRetRTE(
                 datRTEStorage(),
                 Tmin     = -2.0196,
                 tempMin  = get_input_value(input, prefix, "temp_min_rte"),
                 tempMode = get_input_value(input, prefix, "temp_mod_rte"),
                 tempMax  = get_input_value(input, prefix, "temp_max_rte"),
                 timeMin  = get_input_value(input, prefix, "time_min_rte"),
                 timeMod  = get_input_value(input, prefix, "time_mod_rte"),
                 timeMax  = get_input_value(input, prefix, "time_max_rte")
                 )
  return(df)
}

ca_RTEStorageInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Storage"),
#  tagList(
    sliderInput(ns("temp_min_rte"), 
                label = makeHelp("Minimum retail temperature (ºC) (<i>tempMin</i>)", "caRetRTE"),
                value = 3, min = 0, max = 15, step=0.5),
    sliderInput(ns("temp_mod_rte"), 
                label = makeHelp("Mode retail temperature (ºC) (<i>tempMode</i>)", "caRetRTE"),
                value = 5, min = 0, max = 15, step=0.5),
    sliderInput(ns("temp_max_rte"), 
                label = makeHelp("Maximum retail temperature (ºC) (<i>tempMax</i>)", "caRetRTE"),
                value = 10.3, min = 0, max = 15, step=0.5),
    sliderInput(ns("time_min_rte"), 
                label = makeHelp("Minimum retail time (h) (<i>timeMin</i>)", "caRetRTE"),
                value = 2, min = 0, max = 15, step=0.5),
    sliderInput(ns("time_mod_rte"), 
                label = makeHelp("Mode retail time (h) (<i>timeMode</i>)", "caRetRTE"),
                value = 5, min = 0, max = 15, step=0.5),
    sliderInput(ns("time_max_rte"), 
                label = makeHelp("Max retail time (h) (<i>timeMax</i>)", "caRetRTE"),
                value = 9, min = 0, max = 15, step=0.5)
#    )
  )
}

ca_ConsumersTransport_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_consumerstransport"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_consumerstransport")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_consumerstransport")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_consumerstransport"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_consumerstransport")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_consumerstransport")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_consumerstransport")
    )
  )
}

ca_ConsumersTransport_server <- function(input, output, session, suffix, datRTEStorage) {
  ns <- NS(suffix)
  id <- ns("Consumers Transport")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datConsumersTransport <- eventReactive(input$updateCA, {
    
    is_valid <- checkPert(input, prefix, "temp_min_con", "temp_mod_con", "temp_max_con")
    if (!is_valid) {
      return(NULL)
    }
    
    
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Transport consumer", value = 11/13)
      
      values$data <-  generate_datConsumersTransport(input, prefix, datRTEStorage)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datConsumersTransport")
      prevLotsServer("prev_lots_consumerstransport",                data=datFn)
      prevUnitsServer("prev_units_consumerstransport",              data=datFn)
      mcstatsLotsServer("lots_mcstats_consumerstransport",          data=datFn)
      mcstatsUnitsServer("units_mcstats_consumerstransport",        data=datFn)
      countsLotsDistServer("counts_lots_dist_consumerstransport",   data=datFn)
      countsUnitsDistServer("counts_units_dist_consumerstransport", data=datFn)
      ecdfLotsServer("ecdf_prob_consumerstransport",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCATransportCon , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datConsumersTransport()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_ConsumersTransport_ui(id)
    }
  })
  
  return(datConsumersTransport)
}

generate_datConsumersTransport <- function(input, prefix, datConsumersTransport) {
  set.seed(get_input_value(input, prefix, "seed") + 123)
  req(datConsumersTransport())
  df <- caRet2HomeRTE(
                      datConsumersTransport(),
                      Tmin      = -2.0196,
                      tempMin   = get_input_value(input, prefix, "temp_min_con"),
                      tempMode  = get_input_value(input, prefix, "temp_mod_con"),
                      tempMax   = get_input_value(input, prefix, "temp_max_con"),
                      timeShape = get_input_value(input, prefix, "time_shape"),
                      timeScale = get_input_value(input, prefix, "time_scale")
    )
  return(df)
}

ca_ConsumersTransportInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Consumers Transport"),
#  tagList(
    sliderInput(ns("temp_min_con"),
                label = makeHelp("Minimum transportation temperature (ºC) (<i>tempMin</i>)", "caRet2HomeRTE"),
                value = 7, min = 5, max = 35, step=0.5),
    sliderInput(ns("temp_mod_con"),
                label = makeHelp("Mode transportation temperature (ºC) (<i>tempMode</i>)", "caRet2HomeRTE"),
                value = 15, min = 5, max = 35, step=0.5),
    sliderInput(ns("temp_max_con"),
                label = makeHelp("Maximum transportation temperature (ºC) (<i>tempMax</i>)", "caRet2HomeRTE"),
                value = 30, min = 5, max = 35, step=0.5),
    sliderInput(ns("time_shape"),
                label = makeHelp("Shape parameter of the gamma distribution (<i>timeShape</i>)", "caRet2HomeRTE"),
                value = 6.2, min = 5.0, max = 10, step=0.2),
    sliderInput(ns("time_scale"),
                label = makeHelp("Scale parameter of the gamma distribution (<i>timeScale</i>)", "caRet2HomeRTE"),
                value = 8.2, min = 6.0, max = 12, step=0.2)
#   )
  )
}

ca_Transport_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_transport"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_transport")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_transport")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_transport"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_transport")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_transport")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_transport")
    )
  )
}

ca_Transport_server <- function(input, output, session, suffix, datTesting) {
  ns <- NS(suffix)
  id <- ns("Transport")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datTransport <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Transport", value = 9/13)
      
      values$data <- generate_datTransport(input, prefix, datTesting)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datTransport")
      prevLotsServer("prev_lots_transport",                data=datFn)
      prevUnitsServer("prev_units_transport",              data=datFn)
      mcstatsLotsServer("lots_mcstats_transport",          data=datFn)
      mcstatsUnitsServer("units_mcstats_transport",        data=datFn)
      countsLotsDistServer("counts_lots_dist_transport",   data=datFn)
      countsUnitsDistServer("counts_units_dist_transport", data=datFn)
      ecdfLotsServer("ecdf_prob_transport",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCATransport <- c("temp_min", "temp_mod", "temp_max",
                              "time_min", "time_mod", "time_max")
    lapply(inputListCATransport , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datTransport()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Transport_ui(id)
    }
  })
  
  return(datTransport)
}

generate_datTransport <- function(input, prefix, datTransport) {
  set.seed(get_input_value(input, prefix, "seed") + 42)
  df <- caTrans2RetRTE(
    datTransport(),
    MPD = 8,
    lnQ0Mean = -0.096728,
    lnQ0Sd = 0.063930,
    meanEGR5 = 0.03557288,
    seEGR5 = 0.004,
    Tmin = -2.0196,
    tempMin  = get_input_value(input, prefix, "temp_min"),
    tempMode = get_input_value(input, prefix, "temp_mod"),
    tempMax  = get_input_value(input, prefix, "temp_max"),
    timeMin  = get_input_value(input, prefix, "time_min"),
    timeMod  = get_input_value(input, prefix, "time_mod"),
    timeMax  = get_input_value(input, prefix, "time_max")
    )
  return(df)
}

ca_TransportInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Transport"),
#  tagList(
    sliderInput(ns("temp_min"),
                label = makeHelp("Minimum transportation temperature (ºC) (<i>tempMin</i>)", "caTrans2RetRTE"),
                value = 3, min = 0.0, max = 5, step=0.5),
    sliderInput(ns("temp_mod"),
                label = makeHelp("Mode transportation temperature (ºC) (<i>tempMode</i>)", "caTrans2RetRTE"),
                value = 5, min = 2.0, max = 10, step=0.5),
    sliderInput(ns("temp_max"),
                label = makeHelp("Max transportation temperature (ºC) (<i>tempMax</i>)", "caTrans2RetRTE"),
                value = 10.3, min = 5, max = 15, step=0.5),
    sliderInput(ns("time_min"),
                label = makeHelp("Minimum transportation time (h) (<i>timeMin</i>)", "caTrans2RetRTE"),
                value = 2, min = 0.0, max = 5, step=0.5),
    sliderInput(ns("time_mod"),
                label = makeHelp("Mode transportation time (h) (<i>timeMode</i>)", "caTrans2RetRTE"),
                value = 5, min = 2.0, max = 10, step=0.5),
    sliderInput(ns("time_max"),
                label = makeHelp("Max transportation time (h) (<i>timeMax</i>)", "caTrans2RetRTE"),
                value = 9, min = 5, max = 15, step=0.5)
#    )
  )
}

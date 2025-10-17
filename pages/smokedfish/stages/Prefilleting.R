sf_Prefilleting_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_prefill"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_mcstats_prefill"),  
             h5("Summary statistics - Mean between units"), countsLotsDistUI("sf_lot_counts_prefill")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_prefill"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_mcstats_prefill"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts_prefill")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prefill")
      )
    )
  )
}

sf_Prefilleting_server <- function(input, output, session, suffix, datFish) {
  ns <- NS(suffix)
  id <- ns("Prefilleting")

  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)

  # Define a reactive expression that updates and returns the data
  datPrefill <- eventReactive(input$updateSF, {

    is_valid <- checkPert(input, prefix, "temp_min", "temp_mode", "temp_max") &
      checkPert(input, prefix, "time_min", "time_mode", "time_max")  
    if (!is_valid) {
      return(NULL)
    }

    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Prefilleting", value = 2/13)

        values$data <- generate_datPrefill(input, prefix, datFish)
        # Need a function in the following functions
        datFn <- function() values$data
        if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPrefill")
        prevLotsServer("sf_prev_lots_prefill", data = datFn)
        prevUnitsServer("sf_prev_units_prefill", data = datFn)
        mcstatsLotsServer("sf_lot_mcstats_prefill", data = datFn)
        mcstatsUnitsServer("sf_unit_mcstats_prefill", data = datFn)
        countsLotsDistServer("sf_lot_counts_prefill", data = datFn)
        countsUnitsDistServer("sf_unit_counts_prefill", data = datFn)
        ecdfLotsServer("sf_ecdf_prefill", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })

  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListSFPrefill , function(x) input[[paste0(prefix,x)]])

    # Reset data to NULL to invalidate it
    values$data <- NULL
  })

  observe({ datPrefill()})

  output[[id]] <- renderUI({
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Prefilleting_ui(id)
    }
  })

  return(datPrefill)
}


generate_datPrefill <- function(input, prefix, datFish) {
  set.seed(get_input_value(input, prefix, "seed") + 9732)
  req(datFish())
  df <- sfRawFishStorage(
    datFish(),
    MPD = get_input_value(input, prefix, "mpd"),
    tempMin = get_input_value(input, prefix, "temp_min"),
    tempMode = get_input_value(input, prefix, "temp_mode"),
    tempMax = get_input_value(input, prefix, "temp_max"),
    timeMin = get_input_value(input, prefix, "time_min"),
    timeMode = get_input_value(input, prefix, "time_mode"),
    timeMax = get_input_value(input, prefix, "time_max")
  )
  return(df)
}

sf_PrefilletingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("Prefilleting"),
    #    tagList(
    sliderInput(ns("mpd"), 
                label = makeHelp("MPD of LM in raw fish (log10 CFU/g) (<i>MPD</i>)", 'sfRawFishStorage'),
                value = 9.2, min = 5, max = 10, step = 0.1),
    sliderInput(ns("temp_min"), 
                label = makeHelp("Minimum holding temperature (ºC) (<i>tempMin</i>)", 'sfRawFishStorage'),
                value = -2, min = -4, max = 10, step =0.20),
    sliderInput(ns("temp_mode"), 
                label = makeHelp("Mode holding temperature (ºC) (<i>tempMode</i>)", 'sfRawFishStorage'),
                value = 0, min = -4, max = 10, step = 0.20),
    sliderInput(ns("temp_max"), 
                label = makeHelp("Maximum holding temperature (ºC) (<i>tempMax.</i>)", 'sfRawFishStorage'),
                value = 4, min = -4, max = 10, step = 0.20),
    sliderInput(ns("time_min"), 
                label = makeHelp("Minimum holding time (h) (<i>timeMin</i>)", 'sfRawFishStorage'),
                value = 0.5, min = 0.0, max = 24, step = 0.5),
    sliderInput(ns("time_mode"), 
                label = makeHelp("Mode of the holding time (h) (<i>timeMode</i>)", 'sfRawFishStorage'),
                value = 2, min = 0.0, max = 24, step = 0.5),
    sliderInput(ns("time_max"), 
                label = makeHelp("Maximum holding time (h) (<i>timeMax</i>)", 'sfRawFishStorage'),
                value = 6, min = 0.0, max = 24, step = 0.5)
    #    )
  )
}

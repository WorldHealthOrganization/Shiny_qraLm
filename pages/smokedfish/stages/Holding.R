sf_Holding_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_hold"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_prod_lots_mcstats_hold"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_prod_counts_lots_dist_hold")
           ),
    column(6,
            h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_hold"),
            h5("Counts in contaminated units"), mcstatsUnitsUI("sf_prod_units_mcstats_hold"),
            h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_prod_counts_units_dist_hold")
            ),
    column(12,
          h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_hold")
          )
     )
  )
}

sf_Holding_server <- function(input, output, session, suffix, datFill) {
  ns <- NS(suffix)
  id <- ns("Holding")

  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datHold <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Filleting", value = 4/13)

      values$data <- generate_datHold(input, prefix, datFill)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datHold")
      prevLotsServer("sf_prev_lots_hold", data = datFn)
      prevUnitsServer("sf_prev_units_hold", data = datFn)
      mcstatsLotsServer("sf_prod_lots_mcstats_hold", data = datFn)
      mcstatsUnitsServer("sf_prod_units_mcstats_hold", data = datFn)
      countsLotsDistServer("sf_prod_counts_lots_dist_hold", data = datFn)
      countsUnitsDistServer("sf_prod_counts_units_dist_hold", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_hold", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListSFHold , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datHold()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Holding_ui(id)
    }
  })
  
  return(datHold)
}

generate_datHold <- function(input, prefix, datFill) {
  set.seed(get_input_value(input, prefix, "seed") + 240342)
  df <- sfRawFishStorage(
    datFill(),
#    MPD = get_input_value(input, prefix, "mpd"), #  read from data
#    unitSize = get_input_value(input, prefix, "unit_size_hold"), #  read from data
    tempMin  = get_input_value(input, prefix, "temp_min_hold"),
    tempMode = get_input_value(input, prefix, "temp_mode_hold"),
    tempMax  = get_input_value(input, prefix, "temp_max_hold"),
    timeMin  = get_input_value(input, prefix, "time_min_hold"),
    timeMode = get_input_value(input, prefix, "time_mode_hold"),
    timeMax  = get_input_value(input, prefix, "time_max_hold")
    )
  return(df)
}

sf_HoldingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("Holding"),  
#    tagList(
       sliderInput(ns("temp_min_hold"),  
                   label = makeHelp("Minimum holding temperature (ºC) (<i>tempMin</i>)", 'sfRawFishStorage'),
                    value = -2, min = -4, max = 5, step =0.20),
      sliderInput(ns("temp_mode_hold"), 
                  label = makeHelp("Mode of holding temperature (ºC) (<i>tempMode</i>)", 'sfRawFishStorage'),
                  value = 0, min = -3, max = 8, step = 0.20),
      sliderInput(ns("temp_max_hold"),  
                  label = makeHelp("Maximum holding temperature (ºC) (<i>tempMax</i>)", 'sfRawFishStorage'),
                  value = 4, min = 1, max = 10, step = 0.20),
      sliderInput(ns("time_min_hold"),  
                  label = makeHelp("Minimum holding time (h) (<i>timeMin</i>)", 'sfRawFishStorage'),
                  value = 1.0, min = 0.0, max = 12.0, step = 0.5),
      sliderInput(ns("time_mode_hold"), 
                  label = makeHelp("Mode of holding time (h) (<i>timeMode</i>)", 'sfRawFishStorage'),
                  value = 2, min = 1, max = 16, step = 0.5),
      sliderInput(ns("time_max_hold"),  
                  label = makeHelp("Maximum holding time (h) (<i>timeMax.</i>)", 'sfRawFishStorage'),
                  value = 6, min = 5, max = 24, step = 0.5)
#    )
  )   
}  

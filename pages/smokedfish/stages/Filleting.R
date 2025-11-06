sf_Filleting_ui <- function(id) {
  ns <- NS(id)
  fluidPage(  
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_fill"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_stats_fill"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_lot_counts_fill")
           ),
     column(6,      
           h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_fill"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_stats_fill"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts_fill")
           ),
     column(12,       
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_fill")
           )
    )
  )
}

sf_Filleting_server <- function(input, output, session, suffix, datPrefill) {
  ns <- NS(suffix)
  id <- ns("Filleting")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datFill <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Filleting", value = 3/13)
      values$data <- generate_datFill(input, prefix, datPrefill) 
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datFill")
      prevLotsServer("sf_prev_lots_fill", data = datFn)
      prevUnitsServer("sf_prev_units_fill", data = datFn)
      mcstatsLotsServer("sf_lot_stats_fill", data = datFn)
      mcstatsUnitsServer("sf_unit_stats_fill", data = datFn)
      countsLotsDistServer("sf_lot_counts_fill", data = datFn)
      countsUnitsDistServer("sf_unit_counts_fill", data = datFn)
      ecdfLotsServer("sf_ecdf_fill", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListSFFill , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datFill()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Filleting_ui(id)
    }
  })
  
  return(datFill)
}


generate_datFill <- function(input, prefix, datPrefill) {
  set.seed(get_input_value(input, prefix, "seed") + 61658)
  req(datPrefill())
  df <- sfSlicer(datPrefill(),
                 wSlices     = get_input_value(input, prefix, "w_slices_f"),
                 initSlicer  = get_input_value(input, prefix, "init_slicer_f"),
                 aParamLoc   = 0.07,
                 aParamScale = 0.03,
                 aParamMax   = 0.50,
                 eMean       = -2.12,
                 eSd         = 0.85
                 )
  return(df)
}

sf_FilletingInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
     id = ns("Filleting"),
#    tagList(  
      sliderInput(ns("w_slices_f"), 
                  label = makeHelp("Slice weight (g) (<i>wSlices</i>)", 'sfSlicer'),
                  value = 1300, min = 500, max = 2000, step = 100),
      sliderInput(ns("init_slicer_f"), 
                  label = makeHelp("Slicer initial contamination (CFU) (<i>initSlicer</i>)", 'sfSlicer'),
                  value = 1000, min = 0, max = 100000, step = 10)
#      )  
   )
}

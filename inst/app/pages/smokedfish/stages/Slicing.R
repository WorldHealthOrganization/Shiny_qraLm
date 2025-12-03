sf_Slicing_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_slice"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_slice"),
          h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_slice")
           ),
    column(6,
          h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_slice"),
          h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_slice"),
          h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_slice")
          ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_slice")
    )
  )
  )
}

sf_Slicing_server <- function(input, output, session, suffix, datSmoke) {
  ns <- NS(suffix)
  id <- ns("Slicing")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datSlice <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Slicing", value = 7/13)
      
      values$data <- generate_datSlice(input, prefix, datSmoke) 
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datSlice")
      prevLotsServer("sf_prev_lots_slice", data = datFn)
      prevUnitsServer("sf_prev_units_slice", data = datFn)
      mcstatsLotsServer("sf_mcstats_lots_slice", data = datFn)
      mcstatsUnitsServer("sf_mcstats_units_slice", data = datFn)
      countsLotsDistServer("sf_counts_lots_dist_slice", data = datFn)
      countsUnitsDistServer("sf_counts_units_dist_slice", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_slice", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFSlice <- c("w_slices_s", "init_slicer_s")
    lapply(inputListSFSlice , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datSlice()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Slicing_ui(id)
    }
  })
  
  return(datSlice)
}

generate_datSlice <- function(input, prefix, datSmoke) {
  set.seed(get_input_value(input, prefix, "seed") + 9833)
  
  df <- sfSlicer(datSmoke(),
                 wSlices     = get_input_value(input, prefix, "w_slices_s"),
                 initSlicer  = get_input_value(input, prefix, "init_slicer_s"),
                 aParamLoc   = 0.07,
                 aParamScale = 0.03,
                 aParamMax   = 0.50,
                 eMean       = -2.12,
                 eSd         = 0.85
                 )
  
  return(df)
}

sf_SlicingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Slicing"),   
#    tagList(  
      sliderInput(ns("w_slices_s"), 
                  label = makeHelp("Slices weight (g) (<i>wSlices</i>)", 'sfSlicer'),
                  value = 32.5, min = 10, max = 50, step = 2.5),
      sliderInput(ns("init_slicer_s"), 
                  label = makeHelp("Slicer initial contamination (CFU) (<i>initSlicer</i>)", 'sfSlicer'),
                  value = 0, min = 0, max = 10000, step = 100)
#    )  
   )
}

sf_Smoking_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_smoke"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_smoke"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_smoke")
      ),
      column(6, 
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_smoke"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_smoke"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_smoke")
             ),
      column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_smoke")
    )
   )
  )
}

sf_Smoking_server <- function(input, output, session, suffix, datBrinesalt) {
  ns <- NS(suffix)
  id <- ns("Smoking")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datSmoke <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Smoking", value = 6/13)
      
      values$data <- generate_datSmoke(input, prefix, datBrinesalt)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datSmoke")
      prevLotsServer("sf_prev_lots_smoke", data = datFn)
      prevUnitsServer("sf_prev_units_smoke", data = datFn)
      mcstatsLotsServer("sf_mcstats_lots_smoke", data = datFn)
      mcstatsUnitsServer("sf_mcstats_units_smoke", data = datFn)
      countsLotsDistServer("sf_counts_lots_dist_smoke", data = datFn)
      countsUnitsDistServer("sf_counts_units_dist_smoke", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_smoke", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListSFSmoke , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datSmoke()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Smoking_ui(id)
    }
  })
  
  return(datSmoke)
}

generate_datSmoke <- function(input, prefix, datBrinesalt) {
  set.seed(get_input_value(input, prefix, "seed") + 69874)
  
  df <- sfSmoking(
                  datBrinesalt(),
                  rBrineMean   = get_input_value(input, prefix, "r_brine_mean"),   # 0.871
                  rBrineSd     = get_input_value(input, prefix, "r_brine_sd"),     # 0.807
                  rDrysaltMean = get_input_value(input, prefix, "r_drysalt_mean"), # 1.093
                  rDrysaltSd   = get_input_value(input, prefix, "r_drysalt_sd"),   # 0.532
                  )
  return(df)
}

sf_SmokingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Smoking"),   
#    tagList(
sliderInput(ns("r_brine_mean"), 
            label = makeHelp("Mean of the normal distribution about log10 reduction in LM in brined fillets (log10) (<i>rBrineMean</i>)", 'sfSmoking'),
            value = 0.871, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_brine_sd"), 
            label = makeHelp("Std of the normal distribution about the log10 reduction in LM in brineed fillets (log10) (<i>rBrineSd</i>)", 'sfSmoking'),
            value = 0.807, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_drysalt_mean"), 
            label = makeHelp("Mean of the normal distribution about log10 reduction in LM in dry-salted fillets (log10) (<i>rDrysaltMean</i>)", 'sfSmoking'),
            value = 1.093, min = 0.25, max = 1.50, step = 0.01),
sliderInput(ns("r_drysalt_sd"), 
            label = makeHelp("Std of the normal distribution about the log10 reduction in LM in dry-salted fillets (log10) (<i>rDrysaltSd</i>)", 'sfSmoking'),
            value = 0.532, min = 0.25, max = 1.50, step = 0.01)
)
}

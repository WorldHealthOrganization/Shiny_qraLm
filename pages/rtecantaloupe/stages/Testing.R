ca_Testing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_testing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_testing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_testing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_testing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_testing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_testing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_testing")
    )
  )
}

ca_Testing_server <- function(input, output, session, suffix, datPartitioning) {
  ns <- NS(suffix)
  id <- ns("Testing")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datTesting <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Testing", value = 8/13)
      
      values$data <- generate_datTesting(input, prefix, datPartitioning)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datTesting")
      prevLotsServer("prev_lots_testing",                data=datFn)
      prevUnitsServer("prev_units_testing",              data=datFn)
      mcstatsLotsServer("lots_mcstats_testing",          data=datFn)
      mcstatsUnitsServer("units_mcstats_testing",        data=datFn)
      countsLotsDistServer("counts_lots_dist_testing",   data=datFn)
      countsUnitsDistServer("counts_units_dist_testing", data=datFn)
      ecdfLotsServer("ecdf_prob_testing",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCATesting , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datTesting()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Testing_ui(id)
    }
  })
  
  return(datTesting)
}

generate_datTesting <- function(input, prefix, datPartitioning) {
  set.seed(get_input_value(input, prefix, "seed") + 666)
  df <- caTesting(
    datPartitioning(),
    nTested     = get_input_value(input, prefix, "n_tested"),
    gTested	    = get_input_value(input, prefix, "g_tested"),
    MTested     = get_input_value(input, prefix, "m_tested"),
    cTested     = get_input_value(input, prefix, "c_tested"),
    pLotTested  = get_input_value(input, prefix, "p_lot_tested"),
    Se          = get_input_value(input, prefix, "se"),
#    unitSize    = get_input_value(input, prefix, "unit_size"),
#    sizeLot     = get_input_value(input, prefix, "size_lot"),
    gTestedEnum = get_input_value(input, prefix, "g_tested_enum"),
    backToSublot = TRUE,
    iterSub=NULL
    )
  return(df)
}

ca_TestingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Testing"),
#  tagList(
    sliderInput(ns("n_tested"),
                label = makeHelp("Sample size or number of units tested (<i>nTested</i>)", "caTesting"),
                value = 10, min = 0.0, max = 20, step=5),
    sliderInput(ns("g_tested"),
                label = makeHelp("Sample weight tested per unit (<i>gTested</i>)", "caTesting"),
                value = 10, min = 5, max = 50, step=5),
    sliderInput(ns("m_tested"),
                label = makeHelp("Maximum concentration accepted in a sample (<i>mTested</i>)", "caTesting"),
                value = 2, min = 0, max = 5, step=1),
    sliderInput(ns("c_tested"),
                label = makeHelp("Maximum number of samples accepted (<i>cTested</i>)", "caTesting"),
                value = 1, min = 0, max = 10, step=1),
    sliderInput(ns("p_lot_tested"),
                label = makeHelp("Proportion of lots subjected to sampling and testing (<i>pLotTested</i>)", "caTesting"),
                value = 0.1, min = 0, max = 1, step=0.1),
    sliderInput(ns("se"),
                label = makeHelp("Sensibility of the test or probability to detect (<i>Se</i>)", "caTesting"),
                value = 0.1, min = 0, max = 1, step=0.050),
    sliderInput(ns("g_tested_enum"),
                label = makeHelp("Sample weight tested for enumeration (<i>gTestedEnum</i>)", "caTesting"),
                value = 10, min = 5, max = 20, step=5)
#    )
  )
}

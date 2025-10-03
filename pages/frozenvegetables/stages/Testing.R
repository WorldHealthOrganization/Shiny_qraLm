fv_Testing_ui <- function(id) {
  ns = NS(id)
  fluidPage(
    #    h3("Variability of contamination in lots"),
    
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_test"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_test"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_test")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_test"),
             h5("Counts in contaminated units"),  mcstatsUnitsUI("fv_units_mcstats_test"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_test")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_test")
      )
    )
  )
}

fv_Testing_server <- function(input, output, session, suffix, datPart) {
  ns <- NS(suffix)
  id <- ns("Testing")
  
  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"
  
  datTest <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Testing", value = 4/8)
      
      values$data <- generate_datTest(input, prefix, datPart)
      # Need a function in the following functions
      datPartFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datTest")
      prevLotsServer("fv_prev_lots_test",                data=datPartFn)
      prevUnitsServer("fv_prev_units_test",              data=datPartFn)
      mcstatsLotsServer("fv_lots_mcstats_test",          data=datPartFn)
      mcstatsUnitsServer("fv_units_mcstats_test",        data=datPartFn)
      countsLotsDistServer("fv_counts_lots_dist_test",   data=datPartFn)
      countsUnitsDistServer("fv_counts_units_dist_test", data=datPartFn)
      ecdfLotsServer("fv_ecdf_prob_test",                data=datPartFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in these inputs
    lapply(inputListFVTest, function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  # This line is needed
  observe({ datTest() })
  
  # UI, empty if invalid
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Testing_ui(id)
    }
  })
  
  return(datTest)
}


generate_datTest <- function(input, prefix, datPart) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datTest")
  set.seed(get_input_value(input, prefix, "seed") + 789)
  df <- fvTesting(
    datPart(),
    nTested = get_input_value(input, prefix, "n_tested"),
    gTested = get_input_value(input, prefix, "g_tested"),
    MTested = get_input_value(input, prefix, "m_tested"),
    cTested = get_input_value(input, prefix, "c_tested"),
    pLotTested = get_input_value(input, prefix, "p_lot_tested"),
    Se = get_input_value(input, prefix, "se"),
    gTestedEnum = get_input_value(input, prefix, "g_tested_enumeration"),
    iterSub = NULL
  )
  return(df)
}

fv_TestingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("Testing"),
    #  tagList(
    sliderInput(ns("n_tested"), 
                label = makeHelp("Sample size (<i>nTested</i>)", 'fvTesting'),
                value=5, min=0, max=20, step=5),
    sliderInput(ns("g_tested"),
                label = makeHelp("Sample weight tested (g) (<i>gTested</i>)", 'fvTesting'),
                value=25, min=5, max=50, step=5),
    sliderInput(ns("m_tested"), 
                label = makeHelp("Maximum concentration accepted (CFU/g) (<i>MTested</i>)", 'fvTesting'),
                value=0, min=0, max=10, step=1),
    sliderInput(ns("c_tested"), 
                label = makeHelp("Maximum nº samples accepted (<i>cTested</i>)", 'fvTesting'),
                value=0, min=0, max=5, step=1),
    sliderInput(ns("p_lot_tested"), 
                label = makeHelp("Proportion of lots tested (<i>pLotTested</i>)", 'fvTesting'),
                value=0, min=0, max=1, step=0.01),
    sliderInput(ns("se"), 
                label = makeHelp("Sensibility of the test (<i>Se</i>)", 'fvTesting'),
                value=1, min=0.7, max=1.0, step=0.1),
    sliderInput(ns("g_tested_enumeration"),
                label = makeHelp("Sample weight tested for enumeration (g) (<i>gTestedEnum</i>)", 'fvTesting'),
                value=10, min=5, max=50, step=5),
    # )
  )
}


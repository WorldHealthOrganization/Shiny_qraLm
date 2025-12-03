ca_Storing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_storing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_storing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_storing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_storing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_storing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_storing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_storing")
    )
  )
}

ca_Storing_server <- function(input, output, session, suffix, datBrush) {
  ns <- NS(suffix)
  id <- ns("Storing")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datStoring <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Storing", value = 4/13)
      
      values$data <- generate_datStoring(input, prefix, datBrush) 
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datStoring")
      prevLotsServer("prev_lots_storing",                data=datFn)
      prevUnitsServer("prev_units_storing",              data=datFn)
      mcstatsLotsServer("lots_mcstats_storing",          data=datFn)
      mcstatsUnitsServer("units_mcstats_storing",        data=datFn)
      countsLotsDistServer("counts_lots_dist_storing",   data=datFn)
      countsUnitsDistServer("counts_units_dist_storing", data=datFn)
      ecdfLotsServer("ecdf_prob_storing",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCAStoring <- c("p_cooled", "time_sto")
    lapply(inputListCAStoring , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datStoring()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Storing_ui(id)
    }
  })
  
  return(datStoring)
}

generate_datStoring <- function(input, prefix, datBrush) {
  set.seed(get_input_value(input, prefix, "seed") + 98255)
  df <- caHoldingTime(
    datBrush(),
    pCooled  = get_input_value(input, prefix, "p_cooled"),
    time     = get_input_value(input, prefix, "time_sto"),
    shape = 0.6271,
    meanD410 = 1.1309,
    sdD410 = 1.770711e-06, 
    meanD25 = 2.890015,
    sdD25 = 0.2288748
    )
  return(df)
}

ca_StoringInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Storing"),
#  tagList(
    sliderInput(ns("p_cooled"),
                label = makeHelp("Probability that a lot of cantaloupes is cooled (<i>pCooled</i>)", "caHoldingTime"),
                value = 0, min = 0, max = 1, step=0.1),
    sliderInput(ns("time_sto"),
                label = makeHelp("Storage time of a lot of cantaloupes (h) (<i>time</i>)", "caHoldingTime"),
                value = 6, min = 0, max = 10, step=1)
#    )
 )
}

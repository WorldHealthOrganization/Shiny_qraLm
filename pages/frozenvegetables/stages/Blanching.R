fv_Blanching_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_blanch"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_blanch"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_blanch")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_blanch"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_blanch"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_blanch")
    ),
    column(12, 
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("ecdf_prob_blanch")
    )
  )
}

fv_Blanching_server <- function(input, output, session, suffix, datProd) {
  ns <- NS(suffix)
  id <- ns("Blanching")
  
  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"
  
  # Define a reactive expression that updates and returns the data
  datBlanch <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Blanching", value = 2/8)

      values$data <- generate_datBlanch(input, prefix, datProd)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datBlanch")
      prevLotsServer("prev_lots_blanch",                data=datFn)
      prevUnitsServer("prev_units_blanch",              data=datFn)
      mcstatsLotsServer("lots_mcstats_blanch",          data=datFn)
      mcstatsUnitsServer("units_mcstats_blanch",        data=datFn)
      countsLotsDistServer("counts_lots_dist_blanch",   data=datFn)
      countsUnitsDistServer("counts_units_dist_blanch", data=datFn)
      ecdfLotsServer("ecdf_prob_blanch",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListFVBlanch, function(x) input[[paste0(prefix,x)]])
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datBlanch()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Blanching_ui(id)
    }
  })
  
  return(datBlanch)
}

generate_datBlanch <- function(input, prefix, datProd) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datBlanch")
  set.seed(get_input_value(input, prefix, "seed") + 123)
  req(datProd())
  df <- fvBlanching(
    datProd(),
    tempBlanch  = get_input_value(input, prefix, "temp_blanch"),
    timeBlanch  = get_input_value(input, prefix, "time_blanch"),
    logDrefMean =  -1.78,
    logDrefSd   =  0.252,
    zT          = 6.06
  )
  return(df)
}

fv_BlanchingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("Blanching"),
    #  tagList(
    sliderInput(ns("temp_blanch"),
                label = makeHelp("Temperature of blanching (ºC) (<i>tempBlanch</i>)", 'fvBlanching'),
                value = 83.0, min = 70.0, max = 100.0, step=1.0),
    sliderInput(ns("time_blanch"),
                label = makeHelp("Duration of blanching (min) (<i>timeBlanch</i>)", 'fvBlanching'),
                value = 0.75, min = 0.0, max = 3.0, step=0.05)
    #     )
  )
}
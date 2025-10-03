ca_Dicing_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_dicing"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_dicing")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_dicing")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_dicing"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_dicing")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_dicing")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_dicing")
    )
  )
}

ca_Dicing_server <- function(input, output, session, suffix, datWashing) {
  ns <- NS(suffix)
  id <- ns("Dicing")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datDicing <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Dicing", value = 6/13)
      
      values$data <- generate_datDicing(input, prefix, datWashing)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datDicing")
      prevLotsServer("prev_lots_dicing",                data=datFn)
      prevUnitsServer("prev_units_dicing",              data=datFn)
      mcstatsLotsServer("lots_mcstats_dicing",          data=datFn)
      mcstatsUnitsServer("units_mcstats_dicing",        data=datFn)
      countsLotsDistServer("counts_lots_dist_dicing",   data=datFn)
      countsUnitsDistServer("counts_units_dist_dicing", data=datFn)
      ecdfLotsServer("ecdf_prob_dicing",                data=datFn)
      
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCADicing , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datDicing()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Dicing_ui(id)
    }
  })
  
  return(datDicing)
}

generate_datDicing <- function(input, prefix, datWashing) {
  set.seed(get_input_value(input, prefix, "seed") + 159)
  df <- caDicing(
    datWashing(),
    minTR = 0.087,
    modeTR = 0.55,
    maxTR = 2.82,
    cantaSurface  = 580,
    cantaRindFree = get_input_value(input, prefix, "canta_rind_free"),
    sizeSublot = get_input_value(input, prefix, "size_sublot")
    )
  return(df)
}

ca_DicingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Dicing"),
#  tagList(
sliderInput(ns("canta_rind_free"),
            label = makeHelp("Weight of a seedless rind-free cantaloupe (%) (<i>cantaRindFree</i>)", "caDicing"),
            value = 950, min = 600, max = 1000, step=50),
    sliderInput(ns("size_sublot"),
                label = makeHelp("Number of cantaloupes to be diced (<i>sizeSublot</i>)", "caDicing"),
                value = 500, min = 50, max = 1000, step=50)
#    )
  )
}

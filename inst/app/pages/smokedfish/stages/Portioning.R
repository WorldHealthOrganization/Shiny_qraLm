sf_Portioning_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(6,
      h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_port"),
      h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_mcstats_port") , 
      h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_lot_counts_port")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_port"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_mcstats_port"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts_port")
    ),
    column(12,
      h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_port")
    )
  )
  )
}

sf_Portioning_server <- function(input, output, session, suffix, datHome) {
  ns <- NS(suffix)
  id <- ns("Portioning")

  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datPort <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Portioning", value = 12/13)
      
      values$data <- generate_datPort(input, prefix, datHome)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPort")
      prevLotsServer("sf_prev_lots_port", data = datFn)
      prevUnitsServer("sf_prev_units_port", data = datFn)
      mcstatsLotsServer("sf_lot_mcstats_port", data = datFn)
      mcstatsUnitsServer("sf_unit_mcstats_port", data = datFn)
      countsLotsDistServer("sf_lot_counts_port", data = datFn)
      countsUnitsDistServer("sf_unit_counts_port", data = datFn)
      ecdfLotsServer("sf_ecdf_port", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFPort <- c("serving_size", "b_port_sf")
    lapply(inputListSFPort , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datPort()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Portioning_ui(id)
    }
  })
  
  return(datPort)
}

generate_datPort <- function(input, prefix, datHome) {
  set.seed(get_input_value(input, prefix, "seed") + 1973)
  df <- sfPortioning(
    datHome(),
    servingSize = get_input_value(input, prefix, "serving_size"),
    bPortSF = get_input_value(input, prefix, "b_port_sf")
  )
  return(df)
}

sf_PortioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Portioning"),
#    tagList(
      sliderInput(ns("serving_size"), 
                  label = makeHelp("Portion taken from a pack (g) (<i>servingSize</i>)", 'sfPortioning'),
                  value = 32.5, min = 32.5, max = 260, step = 32.5),
      sliderInput(ns("b_port_sf"), 
                  label = makeHelp("Dispersion factor of cells within the package (<i>bPortSF</i>)", 'sfPortioning'),
                  value = 1, min = 0.2, max = 2, step = 0.1)
#      )
  )
}

fv_Portioning_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(
    #    h3("Variability of contamination in lots"),
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_port"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_port"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_port")
      ),
      column(6,
             h5("Prevalence of contaminated units"),     prevUnitsUI("fv_prev_units_port"),
             h5("Counts in contaminated units"),  mcstatsUnitsUI("fv_units_mcstats_port"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_port")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_port")
      )
    )
  )
}
fv_Portioning_server <- function(input, output, session, suffix, datTest) {
  ns <- NS(suffix)
  id <- ns("Portioning")
  
  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"

  datPortfv <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Portioning", value = 5/8)
      
      values$data <- generate_datPortfv(input, prefix, datTest) 
      # Need a function in the following functions
      datPortfvFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPortfv")
      prevLotsServer("fv_prev_lots_port",                data=datPortfvFn)
      prevUnitsServer("fv_prev_units_port",              data=datPortfvFn)
      mcstatsLotsServer("fv_lots_mcstats_port",          data=datPortfvFn)
      mcstatsUnitsServer("fv_units_mcstats_port",        data=datPortfvFn)
      countsLotsDistServer("fv_counts_lots_dist_port",   data=datPortfvFn)
      countsUnitsDistServer("fv_counts_units_dist_port", data=datPortfvFn)
      ecdfLotsServer("fv_ecdf_prob_port",                data=datPortfvFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListFVPort, function(x) input[[paste0(prefix,x)]])
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datPortfv() })
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Portioning_ui(id)
    }
  })
  
  return(datPortfv)
}

generate_datPortfv <- function(input, prefix, datTest) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datPortfv")
  set.seed(get_input_value(input, prefix, "seed") + 369)
  df <- fvPortioning(
    datTest(),
    servingSize = get_input_value(input, prefix, "serving_size_port"),
    bPort = get_input_value(input, prefix, "b_port")
  )
  return(df)
}

fv_PortioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("Portioning"),
    # tagList(
    sliderInput(ns("serving_size_port"), 
                label = makeHelp("Weight portion taken from a pack (g) (<i>servingSize</i>)", 'fvPortioning'),
                value=50, min=25, max=500, step=25),
    sliderInput(ns("b_port"), 
                label = makeHelp("Dispersion in the pack: (<i>bPort</i>)", 'fvPortioning'),
                value=1, min=0, max=3, step=0.05) 
    #    )
  )
}
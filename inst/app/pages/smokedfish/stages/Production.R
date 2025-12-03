sf_Production_ui <- function(id) {
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_lot_stats"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_lot_counts")
#           h5("Variability in the prevalence of contaminated lots"), betaDistUI("sf_beta")
    ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("sf_unit_stats"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_unit_counts")
#           h5("Variability in within-lot prevalence"), prevWithinlotsUI("sf_within_lots")
    ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf")
    )
  )
}

sf_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")

  values <- reactiveValues(data = NULL)
  prefix <- "smokedfish-sidebar-inputs-"
  
  # Define a reactive expression that updates and returns the data
  datFish <- eventReactive(input$updateSF, {
    
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Production", value = 1/13)

      values$data <- generate_datFish(input, prefix,session) #session added RP 2025-0-04
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datFish")
      prevLotsServer("sf_prev_lots", data = datFn)
      prevUnitsServer("sf_prev_units", data = datFn)
      mcstatsLotsServer("sf_lot_stats", data = datFn)
      mcstatsUnitsServer("sf_unit_stats", data = datFish)
      countsLotsDistServer("sf_lot_counts", data = datFn)
      countsUnitsDistServer("sf_unit_counts", data = datFn)
      #betaDistServer("sf_beta", data = datBeta)
      #prevWithinlotsServer("sf_within_lots", data = datFish)
      ecdfLotsServer("sf_ecdf", data = datFish)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFProd <- c("seed", "n_lots", "size_lot", "unit_size_prod",
                         "beta_alpha", "beta_beta", "prop_var_inter",
                         "log_mean_c", "log_sd_c", "poisson")
    lapply(inputListSFProd, function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datFish()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Production_ui(id)
    }
  })
  
  return(datFish)
}

generate_datFish <- function(input, prefix, session) { #first stage, no input
  set.seed(get_input_value(input, prefix, "seed"))
  
  # Limitation for size: Added RP 2025-0-04
  # Are we in shinyapps.io?
  is_shinyapps <- grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"))
  #showNotification(paste("Running on shinyapps.io:", is_shinyapps), type = "message")
  
  nLots        <- get_input_value(input, prefix, "n_lots")
  sizeLot      <- get_input_value(input, prefix, "size_lot")
  
  if(is_shinyapps && (nLots > 500 || sizeLot > 100)){
    showModal(modalDialog(
      title = "Simulation size is exceeded",
      HTML("Due to storage limitations on this server, the size of the simulation for
            smoked fish is restricted to a maximum of 500 lots and a maximum of 100 fish per lot.
            To fully explore the functionality and potential of the models, 
            we recommend installing the application locally on your own machine. 
            The source code is openly accessible in the following GitHub repository:  
            <a href='https://github.com/WorldHealthOrganization/Shiny_qraLM' target = '_blank'>
            https://github.com/WorldHealthOrganization/Shiny_qraLm</a>."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    if(nLots > 500){
      updateNumericInput(session, "smokedfish-sidebar-inputs-n_lots", value = 500)
      validate(need(FALSE, "Simulation size exceeded"))
    }
    
    if(sizeLot > 100){
      updateNumericInput(session, "smokedfish-sidebar-inputs-size_lot", value = 100)
      validate(need(FALSE, "Simulation size exceeded"))
    }
  }
  ###
  
  df <- Lot2LotGen(
    nLots        = nLots,
    sizeLot      = sizeLot,
    unitSize     = get_input_value(input, prefix, "unit_size_prod"),
    betaAlpha    = get_input_value(input, prefix, "beta_alpha"),
    betaBeta     = get_input_value(input, prefix, "beta_beta"),
    C0MeanLog    = get_input_value(input, prefix, "log_mean_c"),
    C0SdLog      = get_input_value(input, prefix, "log_sd_c"),
    propVarInter = get_input_value(input, prefix, "prop_var_inter"),
    Poisson      = get_input_value(input, prefix, "poisson")
  )
  return(df)
}

generate_datBeta <- function(input, prefix) {
    prev <- seq(0, 1, length = get_input_value(input, prefix, "size_lot"))
    prob <- stats::dbeta(prev, 
                         shape1 = get_input_value(input, prefix, "beta_alpha"), 
                         shape2 = get_input_value(input, prefix, "beta_beta"))
  data.frame(prev = prev, prob = prob)
}

sf_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
   id = ns("Production"),  
#  tagList(  
    numericInput(ns("seed"), 
                 "Set a random seed", value = 12345),  
    numericInput(ns("n_lots"), 
                 label = makeHelp("Number of lots (<i>nLots</i>)", 'Lot2LotGen'),
                 value = 500,  min = 500, max = 5000, step = 50),
    numericInput(ns("size_lot"), 
                 label = makeHelp("Number of fish (<i>sizeLot</i>)", 'Lot2LotGen'),
                 value = 100,  min = 100, max = 1000, step = 10),
    numericInput(ns("unit_size_prod"),
                 label = makeHelp("Size of the fish (g) (<i>unitSize</i>)", 'Lot2LotGen'),
                 value = 4000,  min = 3000, max = 5000, step = 500),
    sliderInput(ns("beta_alpha"), 
                label = makeHelp("Beta distribution parameter (<i>betaAlpha</i>)", 'Lot2LotGen'),
                value = 0.874, min = 0.1, max = 3.0, step = 0.01),  
    sliderInput(ns("beta_beta"), 
                label = makeHelp("Beta distribution parameter (<i>betaBeta</i>)", 'Lot2LotGen'),
                 value = 5.88, min = 1.0, max = 15.0, step = 0.02),  
    sliderInput(ns("prop_var_inter"),
                label = makeHelp("Between-lot variance (%) (<i>propVarInter</i>)", 'Lot2LotGen'),
                value = 0.5, min = 0.0, max = 1.0, step = 0.05),  
    sliderInput(ns("log_mean_c"), 
                label = makeHelp("Mean of counts (log10 CFU/g) (<i>C0MeanLog</i>)", 'Lot2LotGen'),
                value = -1.0, min = -3, max = 3, step = 0.02),  
    sliderInput(ns("log_sd_c"),
                label = makeHelp("Std of counts (log10 CFU/g) (<i>C0SdLog</i>)", 'Lot2LotGen'),
                value = 1.0, min = 0.2, max = 3.00, step = 0.01),
    checkboxInput(ns("poisson"), 
                  label = makeHelp("Poisson distribution? (<i>Poisson</i>)", 'Lot2LotGen'),
                  value = TRUE)
#    ) 
   )
}

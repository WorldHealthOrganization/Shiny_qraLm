ca_Production_ui <- function(id) {
  fluidRow(
    column(6,
      h5("Prevalence of contaminated lots"), prevLotsUI("ca_prev_lots"),
      h5("Mean counts in contaminated lots"), mcstatsLotsUI("ca_lot_stats"),
      h5("Distribution of between-lot mean counts"), countsLotsDistUI("ca_lot_counts")
    ),
    column(6,
      h5("Prevalence of contaminated units"), prevUnitsUI("ca_prev_units"),
      h5("Counts in contaminated units"), mcstatsUnitsUI("ca_unit_stats"),
      h5("Distribution of between-unit counts"), countsUnitsDistUI("ca_unit_counts")
    ),
    column(12,
      h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("ca_ecdf")
    )
  )
}

ca_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")

  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datCanta <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Production", value = 1/13)
      
      values$data <-  generate_datCanta(input, prefix, session)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datCanta")
      prevLotsServer("ca_prev_lots",          data = datFn)
      prevUnitsServer("ca_prev_units",        data = datFn)
      mcstatsLotsServer("ca_lot_stats",       data = datFn)
      mcstatsUnitsServer("ca_unit_stats",     data = datFn)
      countsLotsDistServer("ca_lot_counts",   data = datFn)
      countsUnitsDistServer("ca_unit_counts", data = datFn)
      prevWithinlotsServer("ca_within_lots",  data = datFn)
      ecdfLotsServer("ca_ecdf",               data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListCaProd , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datCanta()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Production_ui(id)
    }
  })
  
  return(datCanta)
}

generate_datCanta <- function(input, prefix, session) { #first stage, no input
  set.seed(get_input_value(input, prefix, "seed"))
  
  # Limitation for size: Added RP 2025-0-04
  # Are we in shinyapps.io?
  is_shinyapps <- grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"))
  #showNotification(paste("Running on shinyapps.io:", is_shinyapps), type = "message")
  
  nLots        <- get_input_value(input, prefix, "n_lots")
  sizeLot      <- get_input_value(input, prefix, "size_lot")
  
  if(is_shinyapps && (nLots > 500 || sizeLot > 500)){
    showModal(modalDialog(
      title = "Simulation size is exceeded",
      HTML("Due to storage limitations on this server, the size of the simulation for
            cantaloupe is restricted to a maximum of 500 lots and a maximum of 500 cantaloupes per lot.
            To fully explore the functionality and potential of the models, 
            we recommend installing the application locally on your own machine. 
            The source code is openly accessible in the following GitHub repository:  
            <a href='https://github.com/WorldHealthOrganization/Shiny_qraLM' target = '_blank'>
            https://github.com/WorldHealthOrganization/Shiny_qraLm</a>."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    if(nLots > 500){
      updateNumericInput(session, "rtecantaloupe-sidebar-inputs-n_lots", value = 500)
      validate(need(FALSE, "Simulation size exceeded"))
    }
    
    if(sizeLot > 500){
      updateNumericInput(session, "rtecantaloupe-sidebar-inputs-size_lot", value = 500)
      validate(need(FALSE, "Simulation size exceeded"))
    }
  }
  ###
  
  
  df <- caPrimaryProduction(
    nLots        = get_input_value(input, prefix, "n_lots"),
    sizeLot      = get_input_value(input, prefix, "size_lot"),
    cantaWeight  = get_input_value(input, prefix, "canta_weight"),
    pSoil        = get_input_value(input, prefix, "p_soil"),
    fManure      = get_input_value(input, prefix, "f_manure"),
    pManure      = get_input_value(input, prefix, "p_manure"),
    fIrrigRaining= get_input_value(input, prefix, "f_irrig_raining"),
    pIrrigRaining= get_input_value(input, prefix, "p_irrig_raining"),
    cSoilLogMin  = get_input_value(input, prefix, "c_soil_log_min"),
    cSoilLogMode = get_input_value(input, prefix, "c_soil_log_mode"),
    cSoilLogMax  = get_input_value(input, prefix, "c_soil_log_max"),
    qSoilMin     = get_input_value(input, prefix, "q_soil_min"),
    qSoilMode    = get_input_value(input, prefix, "q_soil_mode"),
    qSoilMax     = get_input_value(input, prefix, "q_soil_max"),
    pFoil        = get_input_value(input, prefix, "p_foil"),
    rFoil        = get_input_value(input, prefix, "r_foil"),
    pIrrig       = get_input_value(input, prefix, "p_irrig"),
    cIrrigLogMin = get_input_value(input, prefix, "c_irrig_log_min"),
    cIrrigLogMax = get_input_value(input, prefix, "c_irrig_log_max"),
    pWaterGainMin= get_input_value(input, prefix, "p_water_gain_min"),
    pWaterGainMax= get_input_value(input, prefix, "p_water_gain_max")
    )
  return(df)
}

ca_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Production"),  
#    tagList(  
      numericInput(ns("seed"), "Set a random seed", 
                   value = 12345),  
      numericInput(ns("n_lots"), 
                   label = makeHelp("Number of lots (<i>nLots</i>)", "caPrimaryProduction"),
                   value = 500,  min = 500, max = 5000, step = 500),
      numericInput(ns("size_lot"), 
                   label = makeHelp("Number of units in a lot (<i>lotSize</i>)", "caPrimaryProduction"),
                   value = 500,  min = 500, max = 5000, step = 500),
      numericInput(ns("canta_weight"),  
                   label = makeHelp("Cantaloupe weight (g) (<i>cantaweight</i>)", "caPrimaryProduction"), 
                   value = 1000,  min = 500, max = 1500, step = 100),
      sliderInput(ns("p_soil"),  
                  label = makeHelp("Prevalence of contamination of soil (<i>pSoil</i>)", "caPrimaryProduction"),
                  value = 0.089, min = 0.0, max = 1.0, step = 0.01),
      sliderInput(ns("p_manure"),  
                  label = makeHelp("Proportion of fields using organic amendments (<i>pManure</i>)", "caPrimaryProduction"),
                  value = 0.5, min = 0.0, max = 1.0, step = 0.10),
      sliderInput(ns("f_manure"),  
                  label = makeHelp("Odds-ratio estimate associated to organic amendment (<i>fManure</i>)", "caPrimaryProduction"), 
            value = 7, min = 0.0, max = 10.0, step = 1.0),
      sliderInput(ns("c_soil_log_min"),  
                  label = makeHelp("Minimum value variability of concentration (<i>cSoilLogMin</i>)", "caPrimaryProduction"),
                  value = -1, min = -2, max = 2, step = 0.25),
      sliderInput(ns("c_soil_log_mode"),  
                  label = makeHelp("Mode value of the variability of concentration (<i>cSoilLogMode</i>)", "caPrimaryProduction"),
                  value = 0.6, min = -1, max = 2, step = 0.25),
      sliderInput(ns("c_soil_log_max"),  
                  label = makeHelp("Maximum value of the variability of concentration (<i>cSoilLogMax</i>)", "caPrimaryProduction"),
                  value = 1.48, min = -1, max = 3, step = 0.25),
      sliderInput(ns("q_soil_min"),  
                  label = makeHelp("Minimum value of the variability of quantity of soil deposited on cantaloupe (<i>qSoilMin</i>)", "caPrimaryProduction"),
                  value = 0.05, min = 0, max = 3, step = 0.05),
      sliderInput(ns("q_soil_mode"),  
                  label = makeHelp("Mode value of the variability of quantity of soil deposited on cantaloupe (<i>qSoilMode</i>)", "caPrimaryProduction"),
                  value = 0.5, min = 0, max = 3, step = 0.05),
      sliderInput(ns("q_soil_max"),  
                  label = makeHelp("Maximum value of the variability of quantity of soil deposited on cantaloupe (<i>qSoilMax</i>)", "caPrimaryProduction"),
                  value = 5, min = 0, max = 10, step = 0.05),
      sliderInput(ns("p_foil"),  
                  label = makeHelp("Proportion of fields grown in foil (e.g. plastic mulch) (<i>pFoil</i>)", "caPrimaryProduction"),
                  value = 0.5, min = 0, max = 1, step = 0.1),      
      sliderInput(ns("r_foil"),  
                  label = makeHelp("Reduction fraction of the quantity of soil transferred to rind (<i>rFoil</i>)", "caPrimaryProduction"),
                  value = 0.9, min = 0, max = 1, step = 0.1),
      sliderInput(ns("f_irrig_raining"),  
                  label = makeHelp("Odds-ratio irrigation and raining events (<i>fIrrigRaining</i>)", "caPrimaryProduction"), 
                  value = 25, min = 0.0, max = 50, step = 1.0),
      sliderInput(ns("p_irrig_raining"),   
                  label = makeHelp("Proportion of fields irrigated or raining just previous harvest (<i>pIrrigRaining</i>)", "caPrimaryProduction"),
                  value = 0.1, min = 0.0, max = 1, step = 0.1),
      sliderInput(ns("p_irrig"),  
                  label = makeHelp("Prevalence of contamination in irrigation water (<i>pIrrig</i>)", "caPrimaryProduction"),
                  value = 0.131, min = 0, max = 1, step = 0.1),
      sliderInput(ns("c_irrig_log_min"),  
                  label = makeHelp("Minimum value of the uniform distribution (<i>cIrrigLogMin</i>)", "caPrimaryProduction"),
                  value = -0.152, min = -1, max = 1, step = 0.1),
      sliderInput(ns("c_irrig_log_max"),  
                  label = makeHelp("Maximum value of the uniform distribution (<i>cIrrigLogMax</i>)", "caPrimaryProduction"),
                  value = 1.04, min = -1, max = 2, step = 0.1),
      sliderInput(ns("p_water_gain_min"),  
                  label = makeHelp("Minimum value of the fraction of water gain (ml) (<i>pWaterGainMin</i>)", "caPrimaryProduction"),
                  value = 0, min = 0, max = 0.25, step = 0.01),
      sliderInput(ns("p_water_gain_max"),  
                  label = makeHelp("Maximum value of the fraction of water gain (ml) (<i>pWaterGainMax</i>)", "caPrimaryProduction"),
                  value = 0.004, min = 0, max = 0.5, step = 0.01)
#      )  
  )   
}

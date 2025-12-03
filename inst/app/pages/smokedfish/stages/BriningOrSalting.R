sf_BriningOrSalting_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_brine_salt"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_brine_salt"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_brine_salt")
    ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_brine_salt"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_brine_salt"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_brine_salt")
    ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_brine_salt")
    )
  )
}

sf_BriningOrSalting_server <- function(input, output, session, suffix, datHold) {
  ns <- NS(suffix)
  id <- ns("BriningOrSalting")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datBrineSalt <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Brinning or Salting", value = 5/13)
      
      values$data <- generate_datBrineSalt(input, prefix, datHold)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datBrineSalt")
      prevLotsServer("sf_prev_lots_brine_salt", data = datFn)
      prevUnitsServer("sf_prev_units_brine_salt", data = datFn)
      mcstatsLotsServer("sf_mcstats_lots_brine_salt", data = datFn)
      mcstatsUnitsServer("sf_mcstats_units_brine_salt", data = datFn)
      countsLotsDistServer("sf_counts_lots_dist_brine_salt", data = datFn)
      countsUnitsDistServer("sf_counts_units_dist_brine_salt", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_brine_salt", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFBrineSalt <- c("p_brine", "pcc_brine", "vol_inj_min", "vol_inj_mode",
                              "vol_inj_max", "conc_brine_min", "conc_brine_mode",
                              "conc_brine_max", "pcc_smearing", "n_surface")
    lapply(inputListSFBrineSalt , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datBrineSalt()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_BriningOrSalting_ui(id)
    }
  })
  
  return(datBrineSalt)
}


generate_datBrineSalt <- function(input, prefix, datHold) {
  set.seed(get_input_value(input, prefix, "seed") + 936)
  
  df <- sfBrineORsaltCC(datHold(),
                        pBrine         = get_input_value(input, prefix, "p_brine"),
                        pccBrine       = get_input_value(input, prefix, "pcc_brine"),
                        volInjMin      = get_input_value(input, prefix, "vol_inj_min"),
                        volInjMode     = get_input_value(input, prefix, "vol_inj_mode"),
                        volInjMax      = get_input_value(input, prefix, "vol_inj_max"),
                        concBrineMin   = get_input_value(input, prefix, "conc_brine_min"),
                        concBrineMode  = get_input_value(input, prefix, "conc_brine_mode"),
                        concBrineMax   = get_input_value(input, prefix, "conc_brine_max"),
                        pccSmearing    = get_input_value(input, prefix, "pcc_smearing"),
                        trSmearingMean = -0.29,
                        trSmearingSd   = 0.31,
                        nSurface       = get_input_value(input, prefix, "n_surface"))
  
  return(df)
}

sf_BriningOrSaltingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("BriningOrSalting"),   
    #    tagList(  
    sliderInput(ns("p_brine"), 
                label = makeHelp("Prob. that a lot is salted by brining (<i>pBrine</i>)", 'sfBrineORsaltCC'),
                value = 1, min = 0, max = 1, step = 0.05),
    sliderInput(ns("pcc_brine"), 
                label = makeHelp("Prob. that the brine is contaminated with LM (<i>pccBrine</i>)", 'sfBrineORsaltCC'),
                value = 0.135, min = 0, max = 1, step = 0.005),
    sliderInput(ns("vol_inj_min"), 
                label = makeHelp("Minimum volume of brine (ml) (<i>volInjMin</i>)", 'sfBrineORsaltCC'),
                value = 25, min = 1, max = 30, step = 1),
    sliderInput(ns("vol_inj_mode"), 
                label = makeHelp("Mode of the volume of brine (ml) (<i>volInjMode</i>)", 'sfBrineORsaltCC'),
                value = 35, min = 15, max = 50, step = 5),
    sliderInput(ns("vol_inj_max"), 
                label = makeHelp("Maximum volume of brine (ml) (<i>volInjMax</i>)", 'sfBrineORsaltCC'),
                value = 100, min = 50, max = 150, step = 10),
    sliderInput(ns("conc_brine_min"), 
                label = makeHelp("Minimum concentration of LM in brine (CFU/ml) (<i>concBrineMin</i>)", 'sfBrineORsaltCC'),
                value = 0, min = 0, max = 5, step = 0.01),
    sliderInput(ns("conc_brine_mode"),
                label = makeHelp("Mode of concentration of LM in brine (CFU/ml) (<i>concBrineMode</i>)", 'sfBrineORsaltCC'),
                value = 0.015, min = 0.0, max = 10, step = 0.005),
    sliderInput(ns("conc_brine_max"),
                label = makeHelp("Maximum concentration of LM in brine (CFU/ml) (<i>concBrineMax</i>)", 'sfBrineORsaltCC'),
                value = 0.060, min = 0.05, max = 20, step = 0.05),
    sliderInput(ns("pcc_smearing"), 
                label = makeHelp("Probability of cross-contamination (<i>pccSmearing</i>)", 'sfBrineORsaltCC'),
                value = 0.03, min = 0.0, max = 1, step = 0.01),
    sliderInput(ns("n_surface"), 
                label = makeHelp("Numbers of LM on surfaces in contact with a fillet (CFU) (<i>nSurface</i>)", 'sfBrineORsaltCC'),
                value = 10, min = 10, max = 1000, step = 10)
    #    )  
  )
}
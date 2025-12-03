sf_Home_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_home"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_prod_lots_mcstats_home"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_prod_counts_lots_dist_home")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_home"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_prod_units_mcstats_home"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_prod_counts_units_dist_home")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_home")
      )
    )
  )
}

sf_Home_server <- function(input, output, session, suffix, datColdchain, RTE) {
  ns <- NS(suffix)
  id <- ns("Home")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datHome <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Home", value = 11/13)
      
      values$data <- generate_datHome(input, prefix, datColdchain, RTE)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datHome")
      prevLotsServer("sf_prev_lots_home",                      data=datFn)
      prevUnitsServer("sf_prev_units_home",                    data=datFn)
      mcstatsLotsServer("sf_prod_lots_mcstats_home",           data=datFn)
      mcstatsUnitsServer("sf_prod_units_mcstats_home",         data=datFn)
      countsLotsDistServer("sf_prod_counts_lots_dist_home",    data=datFn)
      countsUnitsDistServer("sf_prod_counts_units_dist_home",  data=datFn)
      ecdfLotsServer("sf_ecdf_prob_home",                      data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFHome <- c("temp_min_h", "temp_mode_h", "temp_max_h",
                         "time_min_h", "time_mode_h", "time_max_h",
                         "Variability_h", "cor_time_temp_h")
    lapply(inputListSFHome , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datHome()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Home_ui(id)
    }
  })
  
  return(datHome)
}


generate_datHome <- function(input, prefix, datColdchain, RTE) { 
  set.seed(get_input_value(input, prefix, "seed") + 65877)
  
  df <- sfColdChain(
    datColdchain(),
    # RTE characteristics
    RTE = RTE(),
    #    unitSize =  datColdchain()$unitSize,
    tempMin  = get_input_value(input, prefix, "temp_min_h"),
    tempMode = get_input_value(input, prefix, "temp_mode_h"),
    tempMax  = get_input_value(input, prefix, "temp_max_h"),
    timeMin  = get_input_value(input, prefix, "time_min_h"),
    timeMode = get_input_value(input, prefix, "time_mode_h"),
    timeMax  = get_input_value(input, prefix, "time_max_h"),
    variability =  get_input_value(input, prefix, "Variability_h"),
    corTimeTemp = get_input_value(input, prefix, "cor_time_temp_h"),
    intralotSdN0LAB = 0,
    lnQ0LABmin      = -12,
    lnQ0LABmode     = -2.73,
    lnQ0LABmax      = 1.26,
    mumaxrefLm      = 0.419, # mumaxrefLm
    TminLm          = -2.83,
    TrefLm          = 25,
    awminLm         = 0.923,
    pHminLm         = 4.97,
    pheMaxLm        = 32,
    NITmaxLm        = 350,
    CO2maxLm        = 3140,
    micLACuLm       = 3.79,
    micDACuLm       = 4.8,
    micAACuLm       = 10.3,
    micBACuLm       = 0.349,
    micCACuLm       = 2.119,
    micSACuLm       = 1.896,
    mumaxrefLAB     = 0.583,
    TminLAB         = -5.25,
    TrefLAB         = 25,
    awminLAB        = 0.928,
    pHminLAB        = 4.24,
    pheMaxLAB       = 40.3,
    NITmaxLAB       = 2780,
    CO2maxLAB       = 6691,
    micLACuLAB      = 12,
    micDACuLAB      = 33.3,
    micAACuLAB     = 10.3,
    micBACuLAB     = 1.51,
    micCACuLAB   = 10.3,
    micSACuLAB   = 12.6,
    gamma        = 1,
    lim          = 1,
    step         = 1
  )
  return(df)
}

sf_HomeInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("Home"),   
    #  tagList(
    sliderInput(ns("temp_min_h"),     
                label = makeHelp("Minimum storage temperature (ºC) (<i>tempMin</i>)", 'sfColdChain'),
                value = 1.12, min = 0, max = 5, step=0.02),
    sliderInput(ns("temp_mode_h"),     
                label = makeHelp("Mode storage temperature (ºC) (<i>tempMode</i>)", 'sfColdChain'),
                value = 7, min = 0, max = 7, step=0.25),
    sliderInput(ns("temp_max_h"),     
                label = makeHelp("Maximum storage temperature (ºC) (<i>tempMax</i>)", 'sfColdChain'),
                value = 13, min = 5, max = 15, step=0.25),
    sliderInput(ns("time_min_h"),     
                label = makeHelp("Minimum storage time (h) (<i>timeMin</i>)", 'sfColdChain'),
                value = 17.5, min = 0, max = 20, step=0.5),
    sliderInput(ns("time_mode_h"),     
                label = makeHelp("Mode storage time (h) (<i>timeMode</i>)", 'sfColdChain'),
                value = 70, min = 10, max = 100, step=2.0),
    sliderInput(ns("time_max_h"),     
                label = makeHelp("Maximum storage time (h) (<i>timeMax</i>)", 'sfColdChain'),
                value = 840, min = 100, max = 1000, step=10),
    selectInput(ns("Variability_h"),     
                label = makeHelp("Variability for time and temperature (<i>variability</i>)", 'sfColdChain'),
                choices = c("lot", "column", "portion"), selected=c("column")),
    sliderInput(ns("cor_time_temp_h"),     
                label = makeHelp("Correlation time/temperature (<i>corTimeTemp</i>)", 'sfColdChain'),
                value = -0.12, min = -1.0, max = 1.0, step=0.02)
    #  ) 
  )
}

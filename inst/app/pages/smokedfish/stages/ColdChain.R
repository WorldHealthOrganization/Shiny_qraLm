sf_ColdChain_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,
             h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_coldchain"),
             h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_coldchain"),
             h5("Distribution of between-lot mean counts"), countsLotsDistUI("sf_counts_lots_dist_coldchain")
      ),
      column(6,
             h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_coldchain"),
             h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_coldchain"),
             h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_coldchain")
      ),
      column(12,
             h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_coldchain")
      )
    )
  )
}

sf_ColdChain_server <- function(input, output, session, suffix, datPack, RTE) {
  ns <- NS(suffix)
  id <- ns("ColdChain")
  
  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datColdChain <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Cold chain", value = 10/13)
      
      values$data <- generate_datColdChain(input, prefix, datPack, RTE)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datColdChain")
      prevLotsServer("sf_prev_lots_coldchain", data = datFn)
      prevUnitsServer("sf_prev_units_coldchain", data = datFn)
      mcstatsLotsServer("sf_mcstats_lots_coldchain", data = datFn)
      mcstatsUnitsServer("sf_mcstats_units_coldchain", data = datFn)
      countsLotsDistServer("sf_counts_lots_dist_coldchain", data = datFn)
      countsUnitsDistServer("sf_counts_units_dist_coldchain", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_coldchain", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFColdChain <- c("temp_min_cc", "temp_mode_cc", "temp_max_cc",
                              "time_min_cc", "time_mode_cc", "time_max_cc",
                              "Variability_cc", "cor_time_temp",
                              "N0_LAB_min", "N0_LAB_mode", "N0_LAB_max",
                              "MPD_LAB_min", "MPD_LAB_mode", "MPD_LAB_max",
                              "MPD_Lm_min", "MPD_Lm_mode", "MPD_Lm_max")
    lapply(inputListSFColdChain , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datColdChain()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_ColdChain_ui(id)
    }
  })
  
  return(datColdChain)
}

generate_datColdChain <- function(input, prefix, datPack, RTE) {
  set.seed(get_input_value(input, prefix, "seed") + 18062006)
  df1 <- sfColdChain(
    datPack(),
    RTE = RTE(),
    #    unitSize =  datPack()$unitSize,
    tempMin  = get_input_value(input, prefix, "temp_min_cc"),
    tempMode = get_input_value(input, prefix, "temp_mode_cc"),
    tempMax  = get_input_value(input, prefix, "temp_max_cc"),
    timeMin  = get_input_value(input, prefix, "time_min_cc"),
    timeMode = get_input_value(input, prefix, "time_mode_cc"),
    timeMax  = get_input_value(input, prefix, "time_max_cc"),
    variability =  get_input_value(input, prefix, "Variability_cc"),
    corTimeTemp = get_input_value(input, prefix, "cor_time_temp"),
    N0LABmin  = get_input_value(input, prefix, "N0_LAB_min"),
    N0LABmode = get_input_value(input, prefix, "N0_LAB_mode"),
    N0LABmax  = get_input_value(input, prefix, "N0_LAB_max"),
    intralotSdN0LAB = 0,
    lnQ0LABmin  = -12,
    lnQ0LABmode = -2.73,
    lnQ0LABmax  = 1.26,
    MPDLABmin   = get_input_value(input, prefix, "MPD_LAB_min"),
    MPDLABmode  = get_input_value(input, prefix, "MPD_LAB_mode"),
    MPDLABmax   = get_input_value(input, prefix, "MPD_LAB_max"),
    MPDLmmin    = get_input_value(input, prefix, "MPD_Lm_min"),
    MPDLmmode   = get_input_value(input, prefix, "MPD_Lm_mode"),
    MPDLmmax    = get_input_value(input, prefix, "MPD_Lm_max"),
    mumaxrefLm  = 0.419,
    TminLm      = -2.83,
    TrefLm      = 25,
    awminLm = 0.923,
    pHminLm = 4.97,
    pheMaxLm = 32,
    NITmaxLm = 350,
    CO2maxLm = 3140,
    micLACuLm = 3.79,
    micDACuLm = 4.8,
    micAACuLm = 10.3,
    micBACuLm = 0.349,
    micCACuLm = 2.119,
    micSACuLm = 1.896,
    mumaxrefLAB = 0.583,
    TminLAB   = -5.25,
    TrefLAB    = 25,
    awminLAB   = 0.928,
    pHminLAB   = 4.24,
    pheMaxLAB  = 40.3,
    NITmaxLAB  = 2780,
    CO2maxLAB  = 6691,
    micLACuLAB = 12,
    micDACuLAB = 33.3,
    micAACuLAB = 10.3,
    micBACuLAB = 1.51,
    micCACuLAB = 10.3,
    micSACuLAB = 12.6,
    gamma       = 1,
    lim         = 1,
    step        = 1
  )
  return(df1)
}


sf_ColdChainInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("ColdChain"),   
    #  tagList(
    sliderInput(ns("temp_min_cc"),     
                label = makeHelp("Minimum storage temperature (ºC) (<i>tempMin</i>)", 'sfColdChain'),
                value = 0.28, min = 0, max = 4, step=0.02),
    sliderInput(ns("temp_mode_cc"),     
                label = makeHelp("Mode storage temperature (ºC) (<i>tempMode</i>)", 'sfColdChain'),
                value = 4.6, min = 0, max = 7, step=0.1),
    sliderInput(ns("temp_max_cc"),     
                label = makeHelp("Maximum storage temperature (ºC) (<i>tempMax</i>)", 'sfColdChain'),
                value = 7, min = 4, max = 12, step=0.1),
    sliderInput(ns("time_min_cc"),     
                label = makeHelp("Minimum storage time (h) (<i>timeMin</i>)", 'sfColdChain'),
                value = 12, min = 0, max = 24, step=2),
    sliderInput(ns("time_mode_cc"),     
                label = makeHelp("Mode storage time (h) (<i>timeMode</i>)", 'sfColdChain'),
                value = 144, min = 0, max = 200, step=2),
    sliderInput(ns("time_max_cc"),     
                label = makeHelp("Maximum storage time (h) (<i>timeMax</i>)", 'sfColdChain'),
                value = 720, min = 0, max = 1000, step=10),
    selectInput(ns("Variability_cc"),     
                label = makeHelp("Variability for time and temperature (<i>variability</i>)", 'sfColdChain'),
                choices = c("lot", "column", "portion"), selected=c("lot")),
    sliderInput(ns("cor_time_temp"),     
                label = makeHelp("Correlation time/temperature (<i>corTimeTemp</i>)", 'sfColdChain'),
                "corTimeTemp: Correlation time/temperature",
                value = -0.16, min = -1.0, max = 1.0, step=0.02),
    sliderInput(ns("N0_LAB_min"),     
                label = makeHelp("Minimum LAB counts (log10 CFU/g) (<i>N0LABmin</i>)", 'sfColdChain'),
                value = -1.0, min = -2, max = 4, step=0.1),
    sliderInput(ns("N0_LAB_mode"),     
                label = makeHelp("Mode of LAB counts (log10 CFU/g) (<i>N0LABmode</i>)", 'sfColdChain'),
                value = 0.28, min = -2 , max = 6, step=0.1),
    sliderInput(ns("N0_LAB_max"),     
                label = makeHelp("Maximum LAB counts (log10 CFU/g) (<i>N0LABmax</i>)", 'sfColdChain'),
                value = 1.6, min = 1.0 , max = 10, step=0.1),
    sliderInput(ns("MPD_LAB_min"),     
                label = makeHelp("Minimum MPD of LAB (⁠log10 CFU/g (<i>MPDLABmin</i>)", 'sfColdChain'),
                value = 8, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_LAB_mode"),     
                label = makeHelp("Mode of MPD of LAB (⁠log10 CFU/g (<i>MPDLABmode</i>)", 'sfColdChain'),
                value = 8.5, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_LAB_max"),     
                label = makeHelp("Maximum  MPD of LAB (⁠log10 CFU/g (<i>MPDLABmax</i>)", 'sfColdChain'),
                value = 9, min = 4 , max = 10, step=0.25),
    sliderInput(ns("MPD_Lm_min"),     
                label = makeHelp("Minimum  MPD of Lm (⁠log10 CFU/g (<i>MPDLmmin</i>)", 'sfColdChain'),
                value = 6.6, min = 2 , max = 10, step=0.2),
    sliderInput(ns("MPD_Lm_mode"),    
                label = makeHelp("Mode of MPD of Lm (⁠log10 CFU/g (<i>MPDLmmode</i>)", 'sfColdChain'),
                value = 7.4, min = 2 , max = 10, step=0.2),
    sliderInput(ns("MPD_Lm_max"),     
                label = makeHelp("Maximum MPD of Lm (⁠log10 CFU/g (<i>MPDLmmax</i>)", 'sfColdChain'),
                value = 8.2, min = 2 , max = 10, step=0.2)
    #  ) 
  )
}

sf_Characteristics_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow( h5("Smoked fish characteristics"),
      column(12,
            # h5("Smoked fish characteristics"), dataUI("rte_chars")
            h5(histUI("histograms"))
             )
      )
    )
}

sf_Characteristics_server <- function(input, output, session, suffix, datPack) {

  ns <- NS(suffix)
  id <- ns("Characteristics")

  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  RTE <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Characteristics", value = 9/13)

      values$data <- generate_RTE(input, prefix, datPack)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat RTE")
      histServer("histograms", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListSFRTE <- c("NaCl_min_SF", "NaCl_mode_SF", "NaCl_max_SF",
                        "P_min_SF", "P_mode_SF", "P_max_SF",
                        "pH_min_SF", "pH_mode_SF", "pH_max_SF",
                        "CO2equi_min_SF", "CO2equi_mode_SF", "CO2equi_max_SF",
                        "daWph_min_SF", "daWph_mode_SF", "daWph_max_SF",
                        "laWph_min_SF", "laWph_mode_SF", "laWph_max_SF")
    lapply(inputListSFRTE , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ RTE()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Characteristics_ui(id)
    }
  })
  
  return(RTE)
  
}

generate_RTE <- function(input, prefix, datPack) {
  set.seed(get_input_value(input, prefix, "seed") + 12022004)
    df <- sfCharacteristics(
    nLots=datPack()$nLots,
    awminSF=NULL,
    awmodeSF=NULL,
    awmaxSF=NULL,
    NaClminSF  = get_input_value(input, prefix, "NaCl_min_SF"),  # 1.5 
    NaClmodeSF = get_input_value(input, prefix, "NaCl_mode_SF"), # 3.4 
    NaClmaxSF  = get_input_value(input, prefix, "NaCl_max_SF"),  # 5.3 
    PminSF     = get_input_value(input, prefix, "P_min_SF"),  # 5.0 
    PmodeSF    = get_input_value(input, prefix, "P_mode_SF"),  # 10 
    PmaxSF     = get_input_value(input, prefix, "P_max_SF"),  # 22 
    pHminSF    = get_input_value(input, prefix, "pH_min_SF"),  # 5.8 
    pHmodeSF   = get_input_value(input, prefix, "pH_mode_SF"), #  6.1 
    pHmaxSF    = get_input_value(input, prefix, "pH_max_SF"), # 6.5 
    CO2equilibriumminSF  = get_input_value(input, prefix, "CO2equi_min_SF"), # 0.25
    CO2equilibriummodeSF = get_input_value(input, prefix, "CO2equi_mode_SF"), # 0.25
    CO2equilibriummaxSF  = get_input_value(input, prefix, "CO2equi_max_SF"), # 0.30
    NITminSF=0,
    NITmodeSF=0,
    NITmaxSF=0,
    aaWphminSF=0,
    aaWphmodeSF=0,
    aaWphmaxSF=0,
    baWphminSF=0,
    baWphmodeSF=0,
    baWphmaxSF=0,
    caWphminSF=0,
    caWphmodeSF=0,
    caWphmaxSF=0,
    daWphminSF  = get_input_value(input, prefix, "daWph_min_SF"), # 500
    daWphmodeSF = get_input_value(input, prefix, "daWph_mode_SF"), # 1500
    daWphmaxSF  = get_input_value(input, prefix, "daWph_max_SF"), # 1900
    laWphminSF  = get_input_value(input, prefix, "laWph_min_SF"), # 6000
    laWphmodeSF = get_input_value(input, prefix, "laWph_mode_SF"), # 12000
    laWphmaxSF  = get_input_value(input, prefix, "laWph_max_SF"), # 28000
    saWphminSF=0,
    saWphmodeSF=0,
    saWphmaxSF=0
  )
  return(df)
}


sf_CharacteristicsInputs_ui <- function(id) {  
  ns <- NS(id)  
   div(  
   id = ns("Characteristics"),   
#  tagList(
# SF characteristics    
    sliderInput(ns("NaCl_min_SF"),     
                label = makeHelp("Minimum NaCl concentration (%) (<i>NaClminSF</i>)", 'sfCharacteristics'),
                value = 1.5, min = 0, max = 3, step=0.1),
    sliderInput(ns("NaCl_mode_SF"),     
                label = makeHelp("Mode of NaCl concentration(%) (<i>NaClmodeSF</i>)", 'sfCharacteristics'),
                value = 3.4, min = 2.0, max = 4.0, step=0.1),
    sliderInput(ns("NaCl_max_SF"),     
                label = makeHelp("Maximum NaCl concentration (%) (<i>NaClmaxSF</i>)", 'sfCharacteristics'),
                value = 5.3, min = 4.0, max = 7.0, step=0.1),
    sliderInput(ns("P_min_SF"),     
                label = makeHelp("Minimum phenol concentration (ppm) (<i>PminSF</i>)", 'sfCharacteristics'),
                value = 5, min = 0, max = 10, step=1),
    sliderInput(ns("P_mode_SF"),     
                label = makeHelp("Mode of phenol concentration (ppm) (<i>PmodeSF</i>)", 'sfCharacteristics'),
                value = 10, min = 5, max = 10, step=1),
    sliderInput(ns("P_max_SF"),     
                label = makeHelp("Maximum phenol concentration (ppm) (<i>PmaxSF</i>)", 'sfCharacteristics'),
                 value = 22, min = 10, max = 40, step=1),
    sliderInput(ns("pH_min_SF"),     
                label = makeHelp("Minimum pH (<i>pHminSF</i>)", 'sfCharacteristics'),
                value = 5.8, min = 4.0, max = 6.0, step=0.1),
    sliderInput(ns("pH_mode_SF"),     
                label = makeHelp("Mode of pH (<i>pHmodeSF</i>)", 'sfCharacteristics'),
                value = 6.1, min = 4.5, max = 6.5, step=0.1),
    sliderInput(ns("pH_max_SF"),     
                label = makeHelp("Maximum pH (<i>pHmaxSF</i>)", 'sfCharacteristics'),
                value = 6.5, min = 5.0, max = 7.0, step=0.1),
    sliderInput(ns("CO2equi_min_SF"),     
                label = makeHelp("Minimum CO2 equilibrium (%) (<i>CO2equiminSF</i>)", 'sfCharacteristics'),
                value = 0.25, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("CO2equi_mode_SF"),     
                label = makeHelp("Mode CO2 equilibrium (%) (<i>CO2equimodeSF</i>)", 'sfCharacteristics'),
                value = 0.25, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("CO2equi_max_SF"),     
                label = makeHelp("Maximum CO2 equilibrium (%) (<i>CO2equimaxSF</i>)", 'sfCharacteristics'),
                value = 0.30, min = 0, max = 1.0, step=0.05),
    sliderInput(ns("daWph_min_SF"),     
                label = makeHelp("Minimum diacetate concentration (ppm) (<i>daWphminSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 1500, step=100),
    sliderInput(ns("daWph_mode_SF"),     
                label = makeHelp("Mode of diacetate concentration (ppm) (<i>daWphmodeSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 2000, step=100),
    sliderInput(ns("daWph_max_SF"),     
                label = makeHelp("Maximum diacetate concentration (ppm) (<i>daWphmaxSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 3000, step=100),
    sliderInput(ns("laWph_min_SF"),     
                label = makeHelp("Minimum lactic acid concentration (ppm) (<i>laWphminSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 10000, step=100),
    sliderInput(ns("laWph_mode_SF"),     
                label = makeHelp("Mode of lactic acid concentration (ppm) (<i>laWphmodeSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 20000, step=100),
    sliderInput(ns("laWph_max_SF"),     
                label = makeHelp("Maximum lactic acid concentration (ppm) (<i>laWphmaxSF</i>)", 'sfCharacteristics'),
                value = 0, min = 0, max = 35000, step=100)
#    ) 
  )
}
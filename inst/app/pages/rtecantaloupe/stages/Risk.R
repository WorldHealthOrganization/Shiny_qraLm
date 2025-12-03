ca_Risk_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
#    h5("Risk computation"),
    fluidRow(
    column(8,
           h5("Risk summary"), riskStatsUI("ca_risk_stats")),
    column(8,
           h5("Risk plots"), riskDistUI("ca_risk_dist")
    )
  )
  )
}

ca_Risk_server <- function(input, output, session, suffix, datStoredDices) {
  ns <- NS(suffix)
  id <- ns("Risk")

  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datRisk <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Risk", value = 13/13)
      
      values$data <- caGenerate_datRisk(input, prefix, datStoredDices)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datRisk")
      riskStatsServer("ca_risk_stats",  data = datFn)
      riskDistServer("ca_risk_dist",    data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCARisk <- c("Model", "PopulationJEMRA", "PopulationPouillot",
                         "PopulationEFSA", "PopulationEFSALV", "PopulationEFSAV",
                         "PopulationEFSAMV", "PopulationFritsch")
    lapply(inputListCARisk , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datRisk()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Risk_ui(id)
    }
  })
  
  return(datRisk)
}

caGenerate_datRisk <- function(input, prefix, datCook) {
  model <- get_input_value(input, prefix, "Model")
  population <- switch(model,
                       "JEMRA" = get_input_value(input, prefix, "PopulationJEMRA"),
                       "EFSA" = get_input_value(input, prefix, "PopulationEFSA"),
                       "EFSALV" = get_input_value(input, prefix, "PopulationEFSALV"),
                       "EFSAV" = get_input_value(input, prefix, "PopulationEFSAV"),
                       "EFSAMV" = get_input_value(input, prefix, "PopulationEFSAMV"),
                       "Pouillot" = get_input_value(input, prefix, "PopulationPouillot"),
                       "Fritsch" = get_input_value(input, prefix, "PopulationFritsch"),
                       stop("Invalid model selected")
  )
  df <- DRForModel(
    datCook(),
    model = model,
    population = as.numeric(population),
    Poisson = FALSE
  )
  return(df)
}


ca_RiskInputs_ui <- function(id) {  
  ns <- NS(id)  
  model_choices <- get_dose_models()
  default_model <- if (length(model_choices) > 0) unname(model_choices[1]) else NULL
  div(  
  id = ns("Risk"),   
#    tagList(
      selectInput(ns("Model"), 
                  label = makeHelp("Select DR Model:", 'DRForModel'),
                  choices = model_choices,
                  selected = default_model
      ),
      conditionalPanel(sprintf("input['%s'] == 'JEMRA'", ns("Model")),
                       selectInput(ns("PopulationJEMRA"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("JEMRA"),
                                   selected = 0
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'Pouillot'", ns("Model")),
                       selectInput(ns("PopulationPouillot"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("Pouillot"),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSA'", ns("Model")),
                       selectInput(ns("PopulationEFSA"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("EFSA"),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSALV'", ns("Model")),
                       selectInput(ns("PopulationEFSALV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("EFSALV"),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSAV'", ns("Model")),
                       selectInput(ns("PopulationEFSAV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("EFSAV"),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSAMV'", ns("Model")),
                       selectInput(ns("PopulationEFSAMV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("EFSAMV"),
                                   selected = 0
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'Fritsch'", ns("Model")),
                       selectInput(ns("PopulationFritsch"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = get_population_choices("Fritsch"),
                                   selected = 0
                                   )
                       )
#    ) 
  )
}

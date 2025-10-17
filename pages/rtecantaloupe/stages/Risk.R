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
  div(  
  id = ns("Risk"),   
#    tagList(
      selectInput(ns("Model"), 
                  label = makeHelp("Select DR Model:", 'DRForModel'),
                  choices = list("JEMRA 2004" = "JEMRA", 
                                 "EFSA, 2018" = "EFSA",
                                 "EFSA-Less Virulent" = "EFSALV",
                                 "EFSA-Virulent" = "EFSAV",
                                 "EFSA-More Virulent" = "EFSAMV",
                                 "Pouillot et al, 2015" = "Pouillot",
                                 "Fritsch et al, 2018" = "Fritsch"),
                  selected = "JEMRA"
      ),
      conditionalPanel(sprintf("input['%s'] == 'JEMRA'", ns("Model")),
                       selectInput(ns("PopulationJEMRA"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Healthy population" = 1,
                                                  "Increased susceptibility" = 2),
                                   selected = 2
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'Pouillot'", ns("Model")),
                       selectInput(ns("PopulationPouillot"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Less than 65 years old" = 1,
                                                  "More than 65 years old" = 2,
                                                  "Pregnancy" = 3,
                                                  "Nonhematological Cancer" = 4,
                                                  "Hematological cancer" = 5,
                                                  "Renal or Liver failure" = 6,
                                                  "Solid organ transplant" = 7,
                                                  "Inflammatory diseases" = 8,
                                                  "HIV/AIDS" = 9,
                                                  "Diabetes" = 10,
                                                  "Hear diseases" = 11),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSA'", ns("Model")),
                       selectInput(ns("PopulationEFSA"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Female 1-4 yo" = 1,
                                                  "Male 1-4 yo" = 2,
                                                  "Female 5-14 yo" = 3,
                                                  "Male 5-14 yo" = 4,
                                                  "Female 15-24 yo" = 5,
                                                  "Male 15-24 yo" = 6,
                                                  "Female 25-44 yo" = 7,
                                                  "Male 25-44 yo" = 8,
                                                  "Female 45-64 yo" = 9,
                                                  "Male 45-64 yo" = 10,
                                                  "Female 65-74 yo" = 11,
                                                  "Male 65-74 yo" = 12,
                                                  "Female >75 yo" = 13,
                                                  "Male >75 yo" = 14),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSALV'", ns("Model")),
                       selectInput(ns("PopulationEFSALV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Female 1-4 yo" = 1,
                                                  "Male 1-4 yo" = 2,
                                                  "Female 5-14 yo" = 3,
                                                  "Male 5-14 yo" = 4,
                                                  "Female 15-24 yo" = 5,
                                                  "Male 15-24 yo" = 6,
                                                  "Female 25-44 yo" = 7,
                                                  "Male 25-44 yo" = 8,
                                                  "Female 45-64 yo" = 9,
                                                  "Male 45-64 yo" = 10,
                                                  "Female 65-74 yo" = 11,
                                                  "Male 65-74 yo" = 12,
                                                  "Female >75 yo" = 13,
                                                  "Male >75 yo" = 14),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSAV'", ns("Model")),
                       selectInput(ns("PopulationEFSAV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Female 1-4 yo" = 1,
                                                  "Male 1-4 yo" = 2,
                                                  "Female 5-14 yo" = 3,
                                                  "Male 5-14 yo" = 4,
                                                  "Female 15-24 yo" = 5,
                                                  "Male 15-24 yo" = 6,
                                                  "Female 25-44 yo" = 7,
                                                  "Male 25-44 yo" = 8,
                                                  "Female 45-64 yo" = 9,
                                                  "Male 45-64 yo" = 10,
                                                  "Female 65-74 yo" = 11,
                                                  "Male 65-74 yo" = 12,
                                                  "Female >75 yo" = 13,
                                                  "Male >75 yo" = 14),
                                   selected = 0
                       )
      ),
      
      conditionalPanel(sprintf("input['%s'] == 'EFSAMV'", ns("Model")),
                       selectInput(ns("PopulationEFSAMV"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over subpopulations" = 0,
                                                  "Female 1-4 yo" = 1,
                                                  "Male 1-4 yo" = 2,
                                                  "Female 5-14 yo" = 3,
                                                  "Male 5-14 yo" = 4,
                                                  "Female 15-24 yo" = 5,
                                                  "Male 15-24 yo" = 6,
                                                  "Female 25-44 yo" = 7,
                                                  "Male 25-44 yo" = 8,
                                                  "Female 45-64 yo" = 9,
                                                  "Male 45-64 yo" = 10,
                                                  "Female 65-74 yo" = 11,
                                                  "Male 65-74 yo" = 12,
                                                  "Female >75 yo" = 13,
                                                  "Male >75 yo" = 14),
                                   selected = 0
                       )
      ),
      conditionalPanel(sprintf("input['%s'] == 'Fritsch'", ns("Model")),
                       selectInput(ns("PopulationFritsch"), 
                                   label = makeHelp("Select population:", 'DRForModel'),
                                   choices = list("Marginal over virulence" = 0,
                                                  "Highly virulent" = 1,
                                                  "Medium virulent" = 2,
                                                  "Hypovirulent" = 3),
                                   selected = 0
                                   )
                       )
#    ) 
  )
}

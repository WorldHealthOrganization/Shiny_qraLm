ca_Partitioning_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("prev_lots_partitioning"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("lots_mcstats_partitioning")
           #           h5("Distribution of between-lot mean counts"), countsLotsDistUI("counts_lots_dist_partitioning")
    ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("prev_units_partitioning"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("units_mcstats_partitioning")
           #           h5("Distribution of between-unit counts"), countsUnitsDistUI("counts_units_dist_partitioning")
    ),
    column(12, 
           h5("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("ecdf_prob_partitioning")
    )
  )
}

ca_Partitioning_server <- function(input, output, session, suffix, datDicing) {
  ns <- NS(suffix)
  id <- ns("Partitioning")
  
  prefix <- "rtecantaloupe-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datPartitioning <- eventReactive(input$updateCA, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Partitioning", value = 7/13)
      
      values$data <- generate_datPartitioning(input, prefix, datDicing)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPartitioning")
      prevLotsServer("prev_lots_partitioning",                data=datFn)
      prevUnitsServer("prev_units_partitioning",              data=datFn)
      mcstatsLotsServer("lots_mcstats_partitioning",          data=datFn)
      mcstatsUnitsServer("units_mcstats_partitioning",        data=datFn)
      countsLotsDistServer("counts_lots_dist_partitioning",   data=datFn)
      countsUnitsDistServer("counts_units_dist_partitioning", data=datFn)
      ecdfLotsServer("ecdf_prob_partitioning",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListCAPartitioning <- c("prob_cc_dice", "n_dicer", "unit_size_dic", "b_canta")
    lapply(inputListCAPartitioning , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datPartitioning()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      ca_Partitioning_ui(id)
    }
  })
  
  return(datPartitioning)
}

generate_datPartitioning <- function(input, prefix, datDicing) {
  set.seed(get_input_value(input, prefix, "seed") + 2486)
  df <- caPartitioningCC(
    datDicing(),
    probCCDice    = get_input_value(input, prefix, "prob_cc_dice"),
    trDicerMean	  = -1.42,
    trDicerSd     = 0.52,
    nDicer        = get_input_value(input, prefix, "n_dicer"),
    b             = get_input_value(input, prefix, "b_canta"),
    unitSize      = get_input_value(input, prefix, "unit_size_dic")
    )
  return(df)
}


ca_PartitioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Partitioning"),
#  tagList(
    sliderInput(ns("prob_cc_dice"),
                label = makeHelp("Probability of cross-contamination from the dicing machine (<i>probCCDice</i>)", "caPartitioningCC"),
                value = 0.5, min = 0.0, max = 1.0, step=0.1),
    sliderInput(ns("n_dicer"),
                label = makeHelp("Numbers of LM on the surface of the dicing machine (<i>nDicer</i>)", "caPartitioningCC"),
                value = 100, min = 0, max = 500, step=100),
     sliderInput(ns("unit_size_dic"),
                 label = makeHelp("Weight of a pack of cantaloupe dices (g) (<i>unitSize</i>)", "caPartitioningCC"),
                 value = 200, min = 100, max = 500, step=50),
    sliderInput(ns("b_canta"),
                label = makeHelp("Dispersion factor of the beta distribution (<i>b</i>)", "caPartitioningCC"),
                value = 1, min = 1, max = 2, step=0.05)
#    )
  )
}

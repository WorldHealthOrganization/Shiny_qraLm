fv_Production_ui <- function(id) {
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_prod"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_prod"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_prod")
           #           h5("Between-lot variability in the prevalence"), betaDistUI("fv_beta_dist")
    ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_prod"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_prod"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_prod")
           #           h5("Variability in within-lot prevalence"), prevWithinlotsUI("fv_prob_dist")
    ),
    column(12, h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob"))
  )
}

fv_Production_server <- function(input, output, session, suffix) {
  ns <- NS(suffix)
  id <- ns("Production")
 
  # Reactive value to trigger updates
  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"

  # Define a reactive expression that updates and returns the data
  datProd <- eventReactive(input[["updateFV"]], {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Production", value = 1/8)

      values$data <- generate_datProd(input, prefix)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datProd")
      prevLotsServer("fv_prev_lots_prod",                data=datFn)
      prevUnitsServer("fv_prev_units_prod",              data=datFn)
      mcstatsLotsServer("fv_lots_mcstats_prod",          data=datFn)
      mcstatsUnitsServer("fv_units_mcstats_prod",        data=datFn)
      countsLotsDistServer("fv_counts_lots_dist_prod",   data=datFn)
      countsUnitsDistServer("fv_counts_units_dist_prod", data=datFn)
      #betaDistServer("fv_beta_dist", data=datFn)
      #prevWithinlotsServer("fv_prob_dist", data=datFn)
      ecdfLotsServer("fv_ecdf_prob", data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListFVProd, function(x) input[[paste0(prefix,x)]])
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })

  # This line is needed
  observe({ datProd() })
  
  # UI, empty if invalid
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Production_ui(id)
    }
  })
  
  return(datProd)
}

generate_datProd <- function(input, prefix) { #first stage, no input
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datProd")
  set.seed(get_input_value(input, prefix, "seed"))
  df <- Lot2LotGen(
    nLots        = get_input_value(input, prefix, "n_lots"),
    sizeLot      = get_input_value(input, prefix, "size_lot"),
    unitSize     = get_input_value(input, prefix, "unit_size"),
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

fv_ProductionInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = ns("Production"),  
    #  tagList( 
    numericInput(ns("seed"), "Set a random seed", value = 12345),  
    numericInput(ns("n_lots"),
                 label = makeHelp("Number of lots (<i>nLots</i>)", 'Lot2LotGen'),
                 value = 500,  min = 500, max = 5000, step = 500),
    numericInput(ns("size_lot"), 
                 label = makeHelp("Number of units (<i>sizeLot</i>)", 'Lot2LotGen'),
                 value = 500,  min = 500, max = 5000, step = 500),
    numericInput(ns("unit_size"), 
                 label = makeHelp("Size of the units (g) (<i>unitSize</i>)", 'Lot2LotGen'),
                 value = 500,  min = 200, max = 1000, step = 100),
    sliderInput(ns("beta_alpha"), 
                label = makeHelp("Alpha parameter of the Beta distribution (<i>betaAlpha</i>)", 'Lot2LotGen'),
                value = 0.5112, min = 0.100, max = 3.0, step = 0.050),  
    sliderInput(ns("beta_beta"), 
                label = makeHelp("Beta parameter of the Beta distribution (<i>betaBeta</i>)", 'Lot2LotGen'),
                value = 2.000, min = 0.100, max = 3.0, step = 0.050),  
    sliderInput(ns("prop_var_inter"), 
                label = makeHelp("Prop. of between-lot variance (<i>propVarInter</i>)", 'Lot2LotGen'),
                value = 0.7, min = 0.01, max = 0.99, step = 0.05),  
    sliderInput(ns("log_mean_c"), 
                label = makeHelp("Mean of Counts (log10 CFU/g) (<i>C0MeanLog</i>)", 'Lot2LotGen'),
                value = 1.023, min = -3, max = 3, step = 0.010),  
    sliderInput(ns("log_sd_c"), 
                label = makeHelp("Standard deviation of Counts (log10 CFU/g) (<i>C0SdLog</i>)", 'Lot2LotGen'),
                value = 0.33, min = 0, max = 3.00, step = 0.010),
    checkboxInput(ns("poisson"), 
                  label = makeHelp("Poisson distribution? (<i>Poisson</i>)", 'Lot2LotGen'),
                  value = FALSE)
    #  )  
  )   
}
fv_Cooking_ui <- function(id) {
  ns = NS(id)

  fluidPage(
#    h3("Variability of contamination in lots"),
    fluidRow(
    column(6, 
           h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_cook"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_cook")#,
#           h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_cook"),
           ),
    column(6, 
           h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_cook"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_cook"),
#           h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_cook")
           ),
    column(12, 
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_cook")
           )
    )
    )
}

fv_Cooking_server <- function(input, output, session, suffix, datDefrost) {
  ns <- NS(suffix)
  id <- ns("Cooking")

  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"
  
  datCook <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Cooking", value = 7/8)
      
      values$data <- generate_datCook(input, prefix, datDefrost)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datCook")
      prevLotsServer("fv_prev_lots_cook",                data=datFn)
      prevUnitsServer("fv_prev_units_cook",              data=datFn)
      mcstatsLotsServer("fv_lots_mcstats_cook",          data=datFn)
      mcstatsUnitsServer("fv_units_mcstats_cook",        data=datFn)
      countsLotsDistServer("fv_counts_lots_dist_cook",   data=datFn)
      countsUnitsDistServer("fv_counts_units_dist_cook", data=datFn)
      ecdfLotsServer("fv_ecdf_prob_cook",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListFVCook, function(x) input[[paste0(prefix,x)]])
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datCook()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Cooking_ui(id)
    }
  })
  
  return(datCook)
}

generate_datCook <- function(input, prefix, datDefrost) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datCook")
  set.seed(get_input_value(input, prefix, "seed") + 160470)
  df <- fvCooking(
                 datDefrost(),
                 pCooked = get_input_value(input, prefix, "p_cooked"),
                 minCook = get_input_value(input, prefix, "min_cook"),
                 modeCook = get_input_value(input, prefix, "mode_cook"),
                 maxCook = get_input_value(input, prefix, "max_cook")
  )
  return(df)
}

fv_CookingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Cooking"),
#  tagList(
    sliderInput(ns("p_cooked"), 
                label = makeHelp("Probability of cooking (<i>pCooked</i>)", 'fvCooking'),
                value=1.0, min=0.00, max=1.00, step=0.05),
    sliderInput(ns("min_cook"), 
                label = makeHelp("Minimum LM log reduction (<i>minCook</i>)", 'fvCooking'),
                value=1, min=0, max=5, step=0.25),
    sliderInput(ns("mode_cook"), 
                label = makeHelp("Mode of LM log reduction (<i>modeCook</i>)", 'fvCooking'),
                value=5, min=2, max=8, step=0.25),
    sliderInput(ns("max_cook"), 
                label = makeHelp("Maximum LM log reduction (<i>maxCook</i>)", 'fvCooking'),
                value=9, min=5, max=9, step=0.25) 
#    )
  )
}




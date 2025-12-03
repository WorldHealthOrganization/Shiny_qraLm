fv_Defrosting_ui <- function(id) {
  ns = NS(id)
  fluidPage(
#    h3("Variability of contamination in lots"),
  fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("fv_prev_lots_defrost"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_defrost"),
           h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_defrost")
           ),
    column(6,
           h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_defrost"),
           h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_defrost"),
           h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_defrost")
           ),
    column(12,
           h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_defrost")
           )
    )
  )
  }

fv_Defrosting_server <- function(input, output, session, suffix, datPort) {
  ns <- NS(suffix)
  id <- ns("Defrosting")

  
  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"
  
  datDefrost <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Defrosting", value = 6/8)
      
      values$data <- generate_datDefrost(input, prefix, datPort)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datDefrost")
      prevLotsServer("fv_prev_lots_defrost",                data=datFn)
      prevUnitsServer("fv_prev_units_defrost",              data=datFn)
      mcstatsLotsServer("fv_lots_mcstats_defrost",          data=datFn)
      mcstatsUnitsServer("fv_units_mcstats_defrost",        data=datFn)
      countsLotsDistServer("fv_counts_lots_dist_defrost",   data=datFn)
      countsUnitsDistServer("fv_counts_units_dist_defrost", data=datFn)
      ecdfLotsServer("fv_ecdf_prob_defrost",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    inputListFVDefrost <- c("temp_defrost", "time_defrost", "mpd", "p_defrost")
    lapply(inputListFVDefrost, function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datDefrost()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Defrosting_ui(id)
    }
  })
  
  return(datDefrost)
  
}

generate_datDefrost <- function(input, prefix, datPort) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datDefrost")
  set.seed(get_input_value(input, prefix, "seed") + 147)
  df <- fvDefrost(
                  datPort(),
                  Temp     = get_input_value(input, prefix, "temp_defrost"),
                  time     = get_input_value(input, prefix, "time_defrost"),
                  MPD      = get_input_value(input, prefix, "mpd"), # 8.00
                  Tmin     = -1.18,
                  meanEGR5 = 0.0117,
                  sdEGR5   = 0.00816,
                  pDefrost = get_input_value(input, prefix, "p_defrost")
  )
  return(df)
}

fv_DefrostingInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Defrosting"),
#  tagList(
    sliderInput(ns("temp_defrost"), 
                label = makeHelp("Defrosting temperature (ºC) (<i>Temp</i>)", 'fvDefrost'),
                value=25, min=5, max=35, step=1),
    sliderInput(ns("time_defrost"), 
                label = makeHelp("Defrosting time (h) (<i>time</i>)", 'fvDefrost'),
                value=2,  min=0, max=24, step=1.0),
    sliderInput(ns("mpd"), 
                label = makeHelp("MPD of LM in blanched vegetables (<i>MPD</i>)", 'fvDefrost'),
                value=8,  min=5, max=10, step=0.2),
    sliderInput(ns("p_defrost"), 
                label = makeHelp("Probability of defrosting (<i>pDefrost</i>)", 'fvDefrost'),
                value=0.20,  min=0, max=1, step=0.05) 
   #)
)
}
fv_Partitioning_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(
  fluidRow(
           column(6,
                  h5("Prevalence of contaminated lots"),prevLotsUI("fv_prev_lots_part"),
                  h5("Mean counts in contaminated lots"), mcstatsLotsUI("fv_lots_mcstats_part"),
                  h5("Distribution of between-lot mean counts"), countsLotsDistUI("fv_counts_lots_dist_part")
                  ),
           column(6,
                  h5("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_part"),
                  h5("Counts in contaminated units"), mcstatsUnitsUI("fv_units_mcstats_part"),
                  h5("Distribution of between-unit counts"), countsUnitsDistUI("fv_counts_units_dist_part")
                  ),
           column(12, 
                  h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("fv_ecdf_prob_part")
                  )
           )
  )
}

fv_Partitioning_server <- function(input, output, session, suffix, datBlanch) {
  ns <- NS(suffix)
  id <- ns("Partitioning")

  values <- reactiveValues(data = NULL)
  prefix <- "frozenvegetables-sidebar-inputs-"
  
  observe({ datBlanch()})

  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListFVPart, function(x) input[[paste0(prefix,x)]])
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })

  # Define a reactive expression that updates and returns the data
  datPart <- eventReactive(input$updateFV, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Partitioning", value = 3/8)
      
      values$data <- generate_datPart(input, prefix, datBlanch)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPart")
      prevLotsServer("fv_prev_lots_part",                data=datFn)
      prevUnitsServer("fv_prev_units_part",              data=datFn)
      mcstatsLotsServer("fv_lots_mcstats_part",          data=datFn)
      mcstatsUnitsServer("fv_units_mcstats_part",        data=datFn)
      countsLotsDistServer("fv_counts_lots_dist_part",   data=datFn)
      countsUnitsDistServer("fv_counts_units_dist_part", data=datFn)
      ecdfLotsServer("fv_ecdf_prob_part",                data=datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  
  observe({ datPart()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      fv_Partitioning_ui(id)
    }
  })
  
  return(datPart)
}


generate_datPart <- function(input, prefix, datBlanch) {
  if (isTRUE(getOption("myVerbose"))) print("reevaluate datPart")
  set.seed(get_input_value(input, prefix, "seed") + 852)
  df <- fvPartitioningCC(
                     datBlanch(),
                     probCC = get_input_value(input, prefix, "pcc"),
                      nEquip = get_input_value(input, prefix, "n_equip"),
                     bCCFV  = get_input_value(input, prefix, "bccfv")
  )
  return(df)
}

fv_PartitioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Partitioning"),
#  tagList(
    sliderInput(ns("pcc"), 
                label = makeHelp("Prob. cross-contamination (<i>probCC</i>)", 'fvPartitioningCC'),
                value=0.25, min=0.0, max=1.0, step=0.01),
    sliderInput(ns("n_equip"), 
                label = makeHelp("Numbers of cells on the surface (<i>nEquip</i>)", 'fvPartitioningCC'),
                value=45000, min=10000, max=100000, step=5000),
    sliderInput(ns("bccfv"), 
                label = makeHelp("Dispersion factor of cells (<i>bCCFV</i>)", 'fvPartitioningCC'),
                value=1.0, min=0, max=3, step=0.10)
 #   )
 )
}

probCC <- 0.125
trMean <- -0.44
trSd <- 0.40
nEquip <- 10000 # not running with 9 or 25
bCCFV <- 1

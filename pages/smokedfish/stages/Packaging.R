sf_Packaging_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
    column(6,
           h5("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_pack"),
           h5("Mean counts in contaminated lots"), mcstatsLotsUI("sf_mcstats_lots_pack"),
           h5("Between lots LM Counts distribution"), countsLotsDistUI("sf_counts_lots_dist_pack")
           ),
     column(6,
            h5("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_pack"),
            h5("Counts in contaminated units"), mcstatsUnitsUI("sf_mcstats_units_pack"),
            h5("Distribution of between-unit counts"), countsUnitsDistUI("sf_counts_units_dist_pack")
            ),
     column(12, h5("Cumulative distribution of mean counts in contaminated lots"), ecdfLotsUI("sf_ecdf_prob_pack")
            )
    )
    )
}

sf_Packaging_server <- function(input, output, session, suffix, datSlice) {
  ns <- NS(suffix)
  id <- ns("Packaging")

  prefix <- "smokedfish-sidebar-inputs-"
  values <- reactiveValues(data = NULL)
  
  # Define a reactive expression that updates and returns the data
  datPack <- eventReactive(input$updateSF, {
    # Generate data and store it in reactive values if NULL
    if (is.null(values$data)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Packaging", value = 8/13)
      
      values$data <- generate_datPack(input, prefix, datSlice)
      # Need a function in the following functions
      datFn <- function() values$data
      if (isTRUE(getOption("myVerbose"))) print("reevaluate stat datPack")
      prevLotsServer("sf_prev_lots_pack", data = datFn)
      prevUnitsServer("sf_prev_units_pack", data = datFn)
      mcstatsLotsServer("sf_mcstats_lots_pack", data = datFn)
      mcstatsUnitsServer("sf_mcstats_units_pack", data = datFn)
      countsLotsDistServer("sf_counts_lots_dist_pack", data = datFn)
      countsUnitsDistServer("sf_counts_units_dist_pack", data = datFn)
      ecdfLotsServer("sf_ecdf_prob_pack", data = datFn)
    }
    # Return the data for downstream use
    values$data
  })
  
  # Invalidate data when any input changes
  observe({
    # Create a reactive expression to track changes in the inputs
    lapply(inputListSFPack , function(x) input[[paste0(prefix,x)]])
    
    # Reset data to NULL to invalidate it
    values$data <- NULL
  })
  
  observe({ datPack()})
  
  output[[id]] <- renderUI({    
    if (is.null(values$data)) {
      HTML("<p><b>Click on the button to update the results.</b></p>")
    } else {
      sf_Packaging_ui(id)
    }
  })
  
  return(datPack)
}

generate_datPack <- function(input, prefix, datSlice) {
  set.seed(get_input_value(input, prefix, "seed") + 9879)
  
  df <- sfPackaging(
                    datSlice(),
                    slicesPerPack = get_input_value(input, prefix, "slices_per_pack"))

  return(df)
}

sf_PackagingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Packaging"),   
#    tagList(  
      sliderInput(ns("slices_per_pack"), 
                  label = makeHelp("Number of slices per pack (<i>slicesPerPack</i>)", 'sfPackaging'),
                  value = 8, min = 4, max = 20, step = 2)
#    )  
  )
}

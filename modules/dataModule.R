dataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$fishCharacteristics <- DT::renderDataTable({
      # Validate dataset
      if (is.null(data()) || !is.list(data())) {
        return(DT::datatable(data.frame(), 
                             caption = "Error: Invalid dataset",
                             extensions = 'Buttons',
                             options = list(dom = 'B')))
      }
      
      # Convert data to data frame
      raw_data <- do.call(cbind, data())
      if (ncol(raw_data) == 0) {
        return(DT::datatable(data.frame(), 
                             caption = "No data available",
                             extensions = 'Buttons',
                             options = list(dom = 'B')))
      }
      
      df <- as.data.frame(raw_data)
      
      # Assign column names dynamically if necessary
      col_names <- c("NaCl", "pH", "P", "CO2equi", "NIT", "aaWph", 
                     "baWph", "caWph", "daWph", "laWph", "saWph")
      if (ncol(df) != length(col_names)) {
        col_names <- paste0("Col_", seq_len(ncol(df)))
      }
      names(df) <- col_names
      
      # Create data table
      dt <- DT::datatable(df,
                          caption = "Fish Characteristics",
                          class = "cell-border stripe", 
                          extensions = 'Buttons',
                          rownames = FALSE,
                          options = list(
                            dom = 'Blfrtip',  # Include Buttons, length menu, filter, info, and pagination
                            pageLength = 10, # Default number of rows per page
                            scrollX = TRUE   # Enable horizontal scrolling for wide tables
                          )) |>
        DT::formatSignif(1:ncol(df), digits = 4) # Format all columns with 4 significant digits
      
      return(dt)
    })
  })
}
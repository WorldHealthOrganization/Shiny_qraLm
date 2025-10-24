# Utility function to retrieve input value with better error handling
get_input_value <- function(input, prefix, key, default = NULL) {
  value <- input[[paste0(prefix, key)]]
  if (is.null(value)) return(default)  # Return default value if key does not exist
  return(value)
}

makeHelp <- function(text, link){
  HTML(paste0(text, 
              '<a href="',
              getOption("myRepos"),
              '/qraLm/reference/',
              link,
              '.html" target="_blank" style="margin-left:5px;">
              <i class="fa fa-info-circle" aria-hidden="true"></i>
              </a>'))
}

checkPert <- function(input, prefix, min, mode, max, type = 'pert'){
  ### Watchout: launch with min, max, max for uniform
  minVal <- get_input_value(input, prefix, min)    
  modeVal <- get_input_value(input, prefix, mode)    
  maxVal <- get_input_value(input, prefix, max) 

  if(type == "uniform") {
    mess <- HTML(paste("The minimum<i>", min, "</i>must be lower or equal to the maximum<i>", mode,"</i>"))
  } else {
    mess <-  HTML(paste("The minimum<i>", min, "</i>must be lower or equal to the mode<i>", mode,"</i>"))
  }
    
  if(minVal > modeVal) {
    showModal(modalDialog(
      title = "Error in parameter specification",
      mess,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    return(FALSE)
  }
  
  if(modeVal > maxVal) {
    showModal(modalDialog(
      title = "Error in parameter specification",
      HTML(paste("The mode<i>",mode, "</i>must be lower or equal to the maximum<i>", max, "</i>")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    return(FALSE)
  }
  
  return(TRUE)
}
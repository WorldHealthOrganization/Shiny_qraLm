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
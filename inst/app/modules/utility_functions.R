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

get_dose_response_metadata <- function() {
  metadata <- getOption("qraLm.dose_response_metadata")
  if (is.null(metadata)) {
    stop("Dose-response metadata not loaded. Ensure load_dose_response_metadata() runs at startup.")
  }
  metadata
}

get_dose_models <- function() {
  models <- get_dose_response_metadata()$models
  stats::setNames(models, models)
}

get_population_choices <- function(model) {
  if (is.null(model)) return(character())
  
  metadata <- get_dose_response_metadata()
  populations <- metadata$populations_by_model[[model]]
  if (is.null(populations)) return(character())
  
  choices <- stats::setNames(populations$Population, populations$Characteristics)
  margin_label <- metadata$margin_labels[[model]]
  if (!is.null(margin_label)) {
    choices <- c(stats::setNames(0, margin_label), choices)
  }
  choices
}

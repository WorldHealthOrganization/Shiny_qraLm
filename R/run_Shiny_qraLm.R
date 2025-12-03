#' Launch the Shiny_qraLm application
#'
#' @param ... Additional parameters passed to `shiny::runApp()`.
#' @export
run_Shiny_qraLm <- function(...) {
  app_dir <- system.file("app", package = "ShinyqraLm")
  if (app_dir == "") {
    stop("Unable to locate the Shiny_qraLm app directory inside the package. ",
         "Ensure that `inst/app/` exists before installing.")
  }
  shiny::runApp(app_dir, ...)
}


app_dir <- system.file("app", package = "ShinyqraLm")

if (!nzchar(app_dir) || !dir.exists(app_dir)) {
  stop("Unable to locate the bundled Shiny app directory for testing.")
}

source(file.path(app_dir, "data", "data.R"))
source(file.path(app_dir, "modules", "utility_functions.R"))

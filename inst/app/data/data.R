# Helper to expose dose-response metadata from qraLm/sysdata.rda
load_dose_response_metadata <- function() {
  dr_param <- NULL
  
  if (requireNamespace("qraLm", quietly = TRUE) &&
      exists("DRParam", envir = asNamespace("qraLm"))) {
    dr_param <- get("DRParam", envir = asNamespace("qraLm"))
  } else {
    dr_env <- new.env(parent = emptyenv())
    load("data/sysdata.rda", envir = dr_env)
    if (!exists("DRParam", envir = dr_env)) {
      stop("DRParam not found in sysdata.rda")
    }
    dr_param <- get("DRParam", envir = dr_env)
  }
  
  dr_param <- dr_param[order(dr_param$Model, dr_param$Population), ]
  populations_by_model <- split(dr_param, dr_param$Model)
  models <- names(populations_by_model)
  
  margin_labels <- list(
    JEMRA = "Marginal over subpopulations",
    Pouillot = "Marginal over subpopulations",
    EFSA = "Marginal over subpopulations",
    EFSALV = "Marginal over subpopulations",
    EFSAV = "Marginal over subpopulations",
    EFSAMV = "Marginal over subpopulations",
    Fritsch = "Marginal over virulence"
  )
  
  list(
    dr_param = dr_param,
    models = models,
    populations_by_model = populations_by_model,
    margin_labels = margin_labels
  )
}

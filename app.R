library(shiny)

source("R/calc_scores.R")
source("R/validate_inputs.R")
source("R/imputation.R")
source("R/import_uploads.R")
source("R/transform_inputs.R")
source("R/preflight.R")
source("R/plots.R")
source("R/app_shiny.R")

shinyApp(iah_app_ui(), iah_app_server)

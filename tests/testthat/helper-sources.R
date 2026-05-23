`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

find_project_root <- function(start = getwd(), required = TRUE) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "R", "calc_scores.R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      if (!isTRUE(required)) {
        return(NULL)
      }
      stop("Could not find project root from ", start, call. = FALSE)
    }
    path <- parent
  }
}

SOURCE_ROOT <- find_project_root(required = FALSE)
PROJECT_ROOT <- SOURCE_ROOT %||% normalizePath(getwd(), winslash = "/", mustWork = TRUE)

has_source_checkout <- function() {
  !is.null(SOURCE_ROOT)
}

skip_if_no_source_checkout <- function() {
  testthat::skip_if_not(
    has_source_checkout(),
    "Source checkout files are not available during installed package checks."
  )
}

project_file <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

reference_data_file <- function(...) {
  project_file("Reference-Data", ...)
}

if (has_source_checkout()) {
  source(file.path(PROJECT_ROOT, "R", "calc_scores.R"))
  source(file.path(PROJECT_ROOT, "R", "validate_inputs.R"))
  source(file.path(PROJECT_ROOT, "R", "imputation.R"))
  source(file.path(PROJECT_ROOT, "R", "import_uploads.R"))
  source(file.path(PROJECT_ROOT, "R", "transform_inputs.R"))
  source(file.path(PROJECT_ROOT, "R", "preflight.R"))
  source(file.path(PROJECT_ROOT, "R", "plots.R"))
} else {
  ns <- asNamespace("IAHRiskCalc")
  for (name in ls(ns, all.names = TRUE)) {
    if (!startsWith(name, ".")) {
      assign(name, get(name, envir = ns), envir = environment())
    }
  }
}

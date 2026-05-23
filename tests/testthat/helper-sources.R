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

running_r_cmd_check <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", "")) ||
    grepl("\\.Rcheck", normalizePath(getwd(), winslash = "/", mustWork = TRUE), fixed = FALSE)
}

SOURCE_ROOT <- find_project_root(required = FALSE)
PROJECT_ROOT <- SOURCE_ROOT %||% normalizePath(getwd(), winslash = "/", mustWork = TRUE)

has_source_checkout <- function() {
  !running_r_cmd_check() && !is.null(SOURCE_ROOT)
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

score_fixture <- function(value_45 = 4, value_90 = 1) {
  df <- as.data.frame(
    as.list(stats::setNames(rep(NA_real_, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  for (var in CLAMP_VARIABLES) {
    df[[paste0(var, "_45")]] <- value_45
    df[[paste0(var, "_90")]] <- value_90
  }
  df
}

raw_grouped_fixture <- function() {
  header_1 <- c("Participant ID")
  header_2 <- c("Participant ID")
  row_1 <- c("P001")
  row_2 <- c("P002")

  raw_labels <- CLAMP_VARIABLES
  raw_labels[raw_labels == "Heart"] <- "Heart Pounding"
  raw_labels[raw_labels == "Shaky"] <- "Shaky/Tremulous"
  raw_labels[raw_labels == "FreeFattyAcids"] <- "Free Fatty Acids"
  raw_labels[raw_labels == "PancreaticP"] <- "Pancreatic P"

  for (i in seq_along(CLAMP_VARIABLES)) {
    var <- CLAMP_VARIABLES[[i]]
    label <- raw_labels[[i]]
    header_1 <- c(header_1, label, "", "", "", "")
    header_2 <- c(header_2, "Baseline", "90 mg/dL", "65 mg/dl", "55", "45 mg/dl")

    value_90 <- 1
    value_45 <- 4
    if (var == "Cortisol") {
      value_90 <- 8
      value_45 <- 32
    }

    row_1 <- c(row_1, 999, value_90, 2, 3, value_45)
    row_2 <- c(row_2, 999, 2, 3, 4, 5)
  }

  as.data.frame(
    rbind(header_1, header_2, row_1, row_2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_wide_csv_fixture <- function(df = NULL, path = tempfile(fileext = ".csv")) {
  if (is.null(df)) {
    df <- score_fixture()
    df <- data.frame("Subject ID" = "S001", df, check.names = FALSE)
  }
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

write_raw_grouped_xlsx_fixture <- function(path = tempfile(fileext = ".xlsx")) {
  testthat::skip_if_not_installed("writexl")
  writexl::write_xlsx(
    list(Clamp = raw_grouped_fixture()),
    path = path,
    col_names = FALSE
  )
  path
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

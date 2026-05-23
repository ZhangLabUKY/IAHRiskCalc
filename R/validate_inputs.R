validate_required_columns <- function(df, vars = CLAMP_VARIABLES) {
  missing_cols <- setdiff(required_score_cols(vars), names(df))

  list(
    ok = length(missing_cols) == 0,
    missing_cols = missing_cols,
    message = if (length(missing_cols) == 0) {
      "All required scoring columns are present."
    } else {
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    }
  )
}

validate_numeric_columns <- function(df, cols = intersect(names(df), clamp_cols())) {
  invalid_cols <- character(0)

  for (col in cols) {
    raw_values <- df[[col]]
    converted <- suppressWarnings(as.numeric(raw_values))
    non_missing_raw <- !(is.na(raw_values) | as.character(raw_values) == "")

    if (any(non_missing_raw & is.na(converted))) {
      invalid_cols <- c(invalid_cols, col)
    }
  }

  list(
    ok = length(invalid_cols) == 0,
    invalid_cols = invalid_cols,
    message = if (length(invalid_cols) == 0) {
      "All clamp columns are numeric or blank."
    } else {
      paste("Columns with non-numeric values:", paste(invalid_cols, collapse = ", "))
    }
  )
}

missing_required_values <- function(df, vars = CLAMP_VARIABLES) {
  cols <- intersect(required_score_cols(vars), names(df))
  if (length(cols) == 0) {
    return(data.frame())
  }

  df <- coerce_clamp_numeric(df, cols)
  missing_matrix <- is.na(df[, cols, drop = FALSE])
  missing_count <- rowSums(missing_matrix)

  data.frame(
    participant_id = participant_ids_for(df),
    missing_value_count = missing_count,
    missing_variables = apply(missing_matrix, 1, function(x) paste(cols[x], collapse = ", ")),
    check.names = FALSE
  )
}

format_missing_values_for_display <- function(missing_values) {
  if (is.null(missing_values) || nrow(missing_values) == 0) {
    return(data.frame(
      "Subject ID" = character(0),
      "Missing value count" = character(0),
      "Missing variables" = character(0),
      check.names = FALSE
    ))
  }

  data.frame(
    "Subject ID" = missing_values$participant_id,
    "Missing value count" = format(as.integer(missing_values$missing_value_count), scientific = FALSE, trim = TRUE),
    "Missing variables" = missing_values$missing_variables,
    check.names = FALSE
  )
}

validate_scoring_input <- function(df, vars = CLAMP_VARIABLES) {
  column_check <- validate_required_columns(df, vars)
  numeric_check <- validate_numeric_columns(df)

  if (!column_check$ok || !numeric_check$ok) {
    return(list(
      ok = FALSE,
      column_check = column_check,
      numeric_check = numeric_check,
      missing_values = data.frame()
    ))
  }

  missing_values <- missing_required_values(df, vars)

  list(
    ok = TRUE,
    column_check = column_check,
    numeric_check = numeric_check,
    missing_values = missing_values
  )
}

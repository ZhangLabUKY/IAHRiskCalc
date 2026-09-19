validate_required_columns <- function(df, vars = CLAMP_VARIABLES) {
  missing_cols <- setdiff(required_45_cols(vars), names(df))

  list(
    ok = length(missing_cols) == 0,
    missing_cols = missing_cols,
    message = if (length(missing_cols) == 0) {
      "All required 45 mg/dL scoring columns are present."
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

validate_symptom_ratings <- function(
  df,
  cols = intersect(symptom_rating_cols(), names(df))
) {
  invalid_values <- data.frame(
    participant_id = character(0),
    symptom_field = character(0),
    entered_value = character(0),
    issue = character(0),
    check.names = FALSE
  )

  for (col in cols) {
    raw_values <- df[[col]]
    raw_text <- trimws(as.character(raw_values))
    numeric_values <- suppressWarnings(as.numeric(raw_text))
    supplied <- !(is.na(raw_values) | raw_text == "")
    invalid <- supplied & (
      is.na(numeric_values) |
        numeric_values < 0 |
        numeric_values > 6 |
        numeric_values != floor(numeric_values)
    )

    if (!any(invalid)) {
      next
    }

    issues <- ifelse(
      is.na(numeric_values[invalid]),
      "Not numeric",
      ifelse(
        numeric_values[invalid] < 0,
        "Below 0",
        ifelse(
          numeric_values[invalid] > 6,
          "Above 6",
          "Not a whole number"
        )
      )
    )

    invalid_values <- rbind(
      invalid_values,
      data.frame(
        participant_id = participant_ids_for(df)[invalid],
        symptom_field = rep(col, sum(invalid)),
        entered_value = raw_text[invalid],
        issue = issues,
        check.names = FALSE
      )
    )
  }

  list(
    ok = nrow(invalid_values) == 0,
    invalid_values = invalid_values,
    message = if (nrow(invalid_values) == 0) {
      "All supplied symptom ratings are whole numbers from 0 to 6."
    } else {
      paste(
        "Invalid symptom ratings: values must be whole numbers from 0 to 6.",
        "Affected values:",
        paste(
          paste0(
            invalid_values$participant_id,
            " ",
            invalid_values$symptom_field,
            " (",
            invalid_values$entered_value,
            ")"
          ),
          collapse = ", "
        )
      )
    }
  )
}

format_invalid_symptom_ratings_for_display <- function(invalid_values) {
  if (is.null(invalid_values) || nrow(invalid_values) == 0) {
    return(data.frame(
      "Subject ID" = character(0),
      "Symptom field" = character(0),
      "Entered value" = character(0),
      "Issue" = character(0),
      check.names = FALSE
    ))
  }

  data.frame(
    "Subject ID" = invalid_values$participant_id,
    "Symptom field" = invalid_values$symptom_field,
    "Entered value" = invalid_values$entered_value,
    "Issue" = invalid_values$issue,
    check.names = FALSE
  )
}

missing_required_values <- function(df, vars = CLAMP_VARIABLES) {
  cols <- intersect(required_45_cols(vars), names(df))
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
  symptom_check <- validate_symptom_ratings(df)

  if (!column_check$ok || !numeric_check$ok || !symptom_check$ok) {
    messages <- c(
      if (!column_check$ok) column_check$message,
      if (!numeric_check$ok) numeric_check$message,
      if (!symptom_check$ok) symptom_check$message
    )
    return(list(
      ok = FALSE,
      column_check = column_check,
      numeric_check = numeric_check,
      symptom_check = symptom_check,
      missing_values = data.frame(),
      message = paste(messages, collapse = " ")
    ))
  }

  missing_values <- missing_required_values(df, vars)

  list(
    ok = TRUE,
    column_check = column_check,
    numeric_check = numeric_check,
    symptom_check = symptom_check,
    missing_values = missing_values,
    message = "Input validation passed."
  )
}

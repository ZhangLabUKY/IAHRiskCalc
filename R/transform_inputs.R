PHYSIOLOGICAL_OFFSET_MULTIPLIER <- 0.8

physiological_cols <- function(
  vars = COUNTERREGULATORY_VARIABLES,
  levels = c(90, 45)
) {
  as.vector(outer(vars, levels, paste, sep = "_"))
}

physiological_offset_fields <- function(
  df,
  cols = intersect(physiological_cols(), names(df)),
  offset_multiplier = PHYSIOLOGICAL_OFFSET_MULTIPLIER
) {
  if (length(cols) == 0 || nrow(df) == 0) {
    return(data.frame(
      participant_id = character(0),
      variable = character(0),
      raw_value = numeric(0),
      offset_value = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- coerce_clamp_numeric(df, cols)
  offset_values <- physiological_offset_values(df, cols, offset_multiplier)
  out <- data.frame(
    participant_id = character(0),
    variable = character(0),
    raw_value = numeric(0),
    offset_value = numeric(0),
    stringsAsFactors = FALSE
  )

  for (col in cols) {
    values <- df[[col]]
    bad_rows <- which(!is.na(values) & values <= 0)
    if (length(bad_rows) > 0) {
      out <- rbind(
        out,
        data.frame(
          participant_id = participant_ids_for(df)[bad_rows],
          variable = col,
          raw_value = values[bad_rows],
          offset_value = offset_values[[col]],
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out
}

physiological_offset_values <- function(
  df,
  cols = intersect(physiological_cols(), names(df)),
  offset_multiplier = PHYSIOLOGICAL_OFFSET_MULTIPLIER
) {
  if (length(cols) == 0) {
    return(stats::setNames(numeric(0), character(0)))
  }

  df <- coerce_clamp_numeric(df, cols)
  stats::setNames(vapply(cols, function(col) {
    values <- df[[col]]
    positive_values <- values[!is.na(values) & values > 0]
    if (length(positive_values) == 0) {
      return(NA_real_)
    }
    min(positive_values) * offset_multiplier
  }, numeric(1)), cols)
}

transform_physiological_responses <- function(
  df,
  allow_offset = FALSE,
  offset_multiplier = PHYSIOLOGICAL_OFFSET_MULTIPLIER
) {
  cols <- intersect(physiological_cols(), names(df))
  df <- coerce_clamp_numeric(df, cols)
  offset_fields <- physiological_offset_fields(df, cols, offset_multiplier)
  offset_values <- physiological_offset_values(df, cols, offset_multiplier)

  if (nrow(offset_fields) > 0 && any(is.na(offset_fields$offset_value))) {
    missing_offset_cols <- unique(offset_fields$variable[is.na(offset_fields$offset_value)])
    message <- paste(
      "Unable to apply log2 offset because no positive raw values are available for:",
      paste(missing_offset_cols, collapse = ", ")
    )
    return(list(
      ok = FALSE,
      data = df,
      needs_offset_confirmation = FALSE,
      offset_applied = FALSE,
      offset_fields = offset_fields,
      transform_warnings = message,
      message = message
    ))
  }

  if (nrow(offset_fields) > 0 && !allow_offset) {
    return(list(
      ok = FALSE,
      data = df,
      needs_offset_confirmation = TRUE,
      offset_applied = FALSE,
      offset_fields = offset_fields,
      transform_warnings = paste(
        nrow(offset_fields),
        "physiological value(s) are <= 0 and need offset confirmation before log2 transformation."
      )
    ))
  }

  for (col in cols) {
    values <- df[[col]]
    needs_offset <- !is.na(values) & values <= 0
    if (any(needs_offset)) {
      values[needs_offset] <- offset_values[[col]]
    }
    df[[col]] <- ifelse(is.na(values), NA_real_, log2(values))
  }

  attr(df, "offset_fields") <- offset_fields
  attr(df, "offset_applied") <- nrow(offset_fields) > 0

  list(
    ok = TRUE,
    data = df,
    needs_offset_confirmation = FALSE,
    offset_applied = nrow(offset_fields) > 0,
    offset_fields = offset_fields,
    transform_warnings = if (nrow(offset_fields) > 0) {
      paste(
        nrow(offset_fields),
        "physiological value(s) were set to 80% of the minimum positive raw value for their variable",
        "before log2 transformation."
      )
    } else {
      character(0)
    }
  )
}

apply_transform_metadata <- function(scores, transform_result) {
  scores$phys_log2_transformed <- TRUE
  scores$phys_offset_applied <- transform_result$offset_applied

  if (nrow(transform_result$offset_fields) == 0) {
    scores$phys_offset_variables <- ""
    return(scores)
  }

  offset_by_participant <- split(
    transform_result$offset_fields$variable,
    transform_result$offset_fields$participant_id
  )
  scores$phys_offset_variables <- vapply(scores$participant_id, function(id) {
    paste(offset_by_participant[[id]] %||% character(0), collapse = ", ")
  }, character(1))

  scores
}

format_offset_fields_for_display <- function(offset_fields) {
  if (is.null(offset_fields) || nrow(offset_fields) == 0) {
    return(data.frame(
      "Subject ID" = character(0),
      "Variable" = character(0),
      "Raw Value" = character(0),
      "Offset Value" = character(0),
      check.names = FALSE
    ))
  }

  data.frame(
    "Subject ID" = offset_fields$participant_id,
    "Variable" = offset_fields$variable,
    "Raw Value" = format(round(offset_fields$raw_value, 2), nsmall = 2, trim = TRUE),
    "Offset Value" = format(round(offset_fields$offset_value, 2), nsmall = 2, trim = TRUE),
    check.names = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

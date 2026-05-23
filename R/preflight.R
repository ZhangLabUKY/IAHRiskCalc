preflight_normalized <- function(normalized) {
  missing <- missing_required_values(normalized$data)
  offset_fields <- physiological_offset_fields(normalized$data)

  transform_result <- NULL
  if (nrow(offset_fields) == 0) {
    transform_result <- transform_physiological_responses(normalized$data, allow_offset = FALSE)
  }

  list(
    ok = TRUE,
    data = normalized$data,
    audit = normalized$audit,
    missing_values = missing,
    has_missing_required = any(missing$missing_value_count > 0),
    offset_fields = offset_fields,
    has_offset_warnings = nrow(offset_fields) > 0,
    transform_result = transform_result
  )
}

preflight_upload <- function(path, original_name = basename(path)) {
  preflight_normalized(normalize_uploaded_clamp(path, original_name = original_name))
}

apply_subject_id_selection_to_preflight <- function(preflight, selected_key = NULL) {
  if (is.null(preflight) || is.null(preflight$audit) || is.null(preflight$audit$subject_id)) {
    return(preflight)
  }

  normalized <- list(
    data = preflight$data,
    audit = preflight$audit
  )
  preflight_normalized(apply_subject_ids_to_normalized(normalized, selected_key))
}

preflight_manual <- function(df) {
  missing <- missing_required_values(df)
  offset_fields <- physiological_offset_fields(df)
  transform_result <- NULL

  if (nrow(offset_fields) == 0) {
    transform_result <- transform_physiological_responses(df, allow_offset = FALSE)
  }

  list(
    ok = TRUE,
    data = df,
    audit = NULL,
    missing_values = missing,
    has_missing_required = any(missing$missing_value_count > 0),
    offset_fields = offset_fields,
    has_offset_warnings = nrow(offset_fields) > 0,
    transform_result = transform_result
  )
}

preflight_status_frame <- function(preflight) {
  if (is.null(preflight)) {
    return(data.frame())
  }

  data.frame(
    rows = nrow(preflight$data),
    missing_rows = sum(preflight$missing_values$missing_value_count > 0),
    offset_warning_count = nrow(preflight$offset_fields),
    parser_warnings = if (is.null(preflight$audit)) {
      ""
    } else {
      paste(preflight$audit$parser_warnings, collapse = " | ")
    },
    check.names = FALSE
  )
}

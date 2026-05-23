column_means_from_reference <- function(reference_df, cols = intersect(names(reference_df), clamp_cols())) {
  reference_df <- coerce_clamp_numeric(reference_df, cols)
  stats::setNames(
    vapply(reference_df[, cols, drop = FALSE], function(x) mean(x, na.rm = TRUE), numeric(1)),
    cols
  )
}

impute_missing_with_means <- function(df, reference_df, vars = CLAMP_VARIABLES) {
  required_cols <- required_score_cols(vars)
  available_cols <- intersect(required_cols, names(df))
  means <- column_means_from_reference(reference_df, intersect(required_cols, names(reference_df)))

  df <- coerce_clamp_numeric(df, available_cols)
  imputed_by_row <- vector("list", nrow(df))

  for (row_index in seq_len(nrow(df))) {
    imputed_cols <- character(0)

    for (col in available_cols) {
      if (is.na(df[row_index, col]) && col %in% names(means) && !is.nan(means[[col]])) {
        df[row_index, col] <- means[[col]]
        imputed_cols <- c(imputed_cols, col)
      }
    }

    imputed_by_row[[row_index]] <- imputed_cols
  }

  attr(df, "imputed_variables") <- vapply(imputed_by_row, paste, collapse = ", ", FUN.VALUE = character(1))
  attr(df, "imputation_used") <- lengths(imputed_by_row) > 0
  df
}

apply_imputation_metadata <- function(scores, imputed_df) {
  imputation_used <- attr(imputed_df, "imputation_used")
  imputed_variables <- attr(imputed_df, "imputed_variables")

  if (!is.null(imputation_used)) {
    scores$imputation_used <- imputation_used
  }
  if (!is.null(imputed_variables)) {
    scores$imputed_variables <- imputed_variables
  }

  scores
}

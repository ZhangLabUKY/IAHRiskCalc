manual_imputation_reference <- function() {
  means <- c(
    Heart_45 = 1.31578947368421,
    Shaky_45 = 2.05263157894737,
    Sweaty_45 = 2,
    Hungry_45 = 3.47368421052632,
    Tingling_45 = 1.42105263157895,
    Confused_45 = 1.10526315789474,
    Tired_45 = 2.10526315789474,
    Weak_45 = 1.68421052631579,
    Warm_45 = 1.89473684210526,
    Faint_45 = 1.10526315789474,
    Dizzy_45 = 0.631578947368421,
    Cortisol_45 = 1.92464620293339,
    Glucagon_45 = 2.36808906764302,
    Dopamine_45 = 2.80277631463586,
    Epinephrine_45 = 3.30407256864743,
    Norepinephrine_45 = 3.4011120916548,
    FreeFattyAcids_45 = 2.34274134193362,
    HGH_45 = 1.6600153558202,
    PancreaticP_45 = 2.99420585349726,
    Insulin_45 = 2.81294974944426,
    Heart_90 = 0.315789473684211,
    Shaky_90 = 0.421052631578947,
    Sweaty_90 = 0,
    Hungry_90 = 1.52631578947368,
    Tingling_90 = 0.789473684210526,
    Confused_90 = 0.105263157894737,
    Tired_90 = 1.26315789473684,
    Weak_90 = 0.421052631578947,
    Warm_90 = 0.631578947368421,
    Faint_90 = 0,
    Dizzy_90 = 0,
    Cortisol_90 = 1.67002276345961,
    Glucagon_90 = 2.31796082663163,
    Dopamine_90 = 2.77212246994653,
    Epinephrine_90 = 2.79578003718674,
    Norepinephrine_90 = 3.39223751495342,
    FreeFattyAcids_90 = 1.2809192999942,
    HGH_90 = -0.853801000878464,
    PancreaticP_90 = 2.43950072245786,
    Insulin_90 = 2.59690056207438
  )

  as.data.frame(as.list(means[required_score_cols()]), check.names = FALSE)
}

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

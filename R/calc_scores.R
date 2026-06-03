CLAMP_VARIABLES <- c(
  "Heart", "Shaky", "Sweaty", "Hungry", "Tingling",
  "Confused", "Tired", "Weak", "Warm", "Faint", "Dizzy",
  "Cortisol", "Glucagon", "Dopamine", "Epinephrine",
  "Norepinephrine", "FreeFattyAcids", "HGH", "PancreaticP", "Insulin"
)

CLAMP_GLUCOSE_LEVELS <- c(111, 90, 65, 55, 45)
UNADJUSTED_45_CUTOFF <- 66.5
ADJUSTED_45_VS_90_CUTOFF <- 25

SYMPTOM_VARIABLES <- c(
  "Heart", "Shaky", "Sweaty", "Hungry", "Tingling",
  "Confused", "Tired", "Weak", "Warm", "Faint", "Dizzy"
)

COUNTERREGULATORY_VARIABLES <- setdiff(CLAMP_VARIABLES, SYMPTOM_VARIABLES)

CLAMP_VARIABLE_LABELS <- c(
  Heart = "Heart Pounding",
  Shaky = "Shaky/Tremulous",
  Sweaty = "Sweaty",
  Hungry = "Hungry",
  Tingling = "Tingling",
  Confused = "Confused",
  Tired = "Tired/Drowsy",
  Weak = "Weak",
  Warm = "Warm",
  Faint = "Faint",
  Dizzy = "Dizzy",
  Cortisol = "Cortisol",
  Glucagon = "Glucagon",
  Dopamine = "Dopamine",
  Epinephrine = "Epinephrine",
  Norepinephrine = "Norepinephrine",
  FreeFattyAcids = "Free fatty acids",
  HGH = "HGH",
  PancreaticP = "Pancreatic Polypeptide",
  Insulin = "Insulin"
)

clamp_variable_label <- function(var) {
  label <- CLAMP_VARIABLE_LABELS[[var]]
  if (is.null(label) || is.na(label)) var else label
}

clamp_cols <- function(vars = CLAMP_VARIABLES, levels = CLAMP_GLUCOSE_LEVELS) {
  as.vector(outer(vars, levels, paste, sep = "_"))
}

required_score_cols <- function(vars = CLAMP_VARIABLES) {
  c(paste0(vars, "_45"), paste0(vars, "_90"))
}

required_45_cols <- function(vars = CLAMP_VARIABLES) {
  paste0(vars, "_45")
}

required_90_cols <- function(vars = CLAMP_VARIABLES) {
  paste0(vars, "_90")
}

score_method_label <- function(method) {
  labels <- c(
    adjusted_45_vs_90 = "Adjusted clamp response",
    unadjusted_45 = "Unadjusted 45 mg/dL response",
    unable_to_calculate = "Unable to calculate"
  )
  out <- unname(labels[as.character(method)])
  ifelse(is.na(out), as.character(method), out)
}

score_method_detail <- function(method) {
  details <- c(
    adjusted_45_vs_90 = "45 mg/dL response minus 90 mg/dL response",
    unadjusted_45 = "Total response at 45 mg/dL",
    unable_to_calculate = "Complete 45 mg/dL values are required"
  )
  out <- unname(details[as.character(method)])
  ifelse(is.na(out), "", out)
}

primary_cutoff_result <- function(score, cutoff) {
  ifelse(
    is.na(score) | is.na(cutoff),
    "Unable to calculate",
    ifelse(
      score >= cutoff,
      "Meets cutoff: NAH",
      "Below cutoff: IAH"
    )
  )
}

read_clamp_csv <- function(path) {
  utils::read.csv(
    path,
    row.names = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

participant_ids_for <- function(df, participant_ids = NULL) {
  if (!is.null(participant_ids)) {
    return(as.character(participant_ids))
  }

  rn <- rownames(df)
  default_rn <- as.character(seq_len(nrow(df)))
  if (!is.null(rn) && !identical(rn, default_rn)) {
    return(rn)
  }

  paste0("Participant_", seq_len(nrow(df)))
}

coerce_clamp_numeric <- function(df, cols = intersect(names(df), clamp_cols())) {
  for (col in cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df
}

calc_clamp_scores <- function(
  df,
  participant_ids = NULL,
  vars = CLAMP_VARIABLES,
  unadjusted_cutoff = UNADJUSTED_45_CUTOFF,
  adjusted_cutoff = ADJUSTED_45_VS_90_CUTOFF
) {
  cols_45 <- required_45_cols(vars)
  cols_90 <- required_90_cols(vars)
  required_cols <- c(cols_45, cols_90)
  missing_cols <- setdiff(cols_45, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  missing_90_cols <- setdiff(cols_90, names(df))
  for (col in missing_90_cols) {
    df[[col]] <- NA_real_
  }

  df <- coerce_clamp_numeric(df, required_cols)

  missing_value_count <- rowSums(is.na(df[, required_cols, drop = FALSE]))
  missing_45_count <- rowSums(is.na(df[, cols_45, drop = FALSE]))
  missing_90_count <- rowSums(is.na(df[, cols_90, drop = FALSE]))
  complete_45 <- missing_45_count == 0
  complete_90 <- missing_90_count == 0

  unadjusted_45_sum <- rowSums(df[, cols_45, drop = FALSE], na.rm = FALSE)
  adjusted_values <- as.matrix(df[, cols_45, drop = FALSE]) -
    as.matrix(df[, cols_90, drop = FALSE])
  adjusted_45_vs_90_sum <- rowSums(adjusted_values, na.rm = FALSE)

  unadjusted_normal_awareness <- ifelse(
    is.na(unadjusted_45_sum),
    NA,
    unadjusted_45_sum >= unadjusted_cutoff
  )
  adjusted_normal_awareness <- ifelse(
    is.na(adjusted_45_vs_90_sum),
    NA,
    adjusted_45_vs_90_sum >= adjusted_cutoff
  )
  unadjusted_impaired_awareness <- ifelse(
    is.na(unadjusted_normal_awareness),
    NA,
    !unadjusted_normal_awareness
  )
  adjusted_impaired_awareness <- ifelse(
    is.na(adjusted_normal_awareness),
    NA,
    !adjusted_normal_awareness
  )
  discordant_flag <- ifelse(
    is.na(unadjusted_impaired_awareness) | is.na(adjusted_impaired_awareness),
    NA,
    xor(unadjusted_impaired_awareness, adjusted_impaired_awareness)
  )

  score_method <- ifelse(
    complete_45 & complete_90,
    "adjusted_45_vs_90",
    ifelse(complete_45, "unadjusted_45", "unable_to_calculate")
  )
  primary_score <- ifelse(
    score_method == "adjusted_45_vs_90",
    adjusted_45_vs_90_sum,
    ifelse(score_method == "unadjusted_45", unadjusted_45_sum, NA_real_)
  )
  primary_cutoff <- ifelse(
    score_method == "adjusted_45_vs_90",
    adjusted_cutoff,
    ifelse(score_method == "unadjusted_45", unadjusted_cutoff, NA_real_)
  )
  primary_normal_awareness <- ifelse(
    is.na(primary_score),
    NA,
    primary_score >= primary_cutoff
  )
  primary_impaired_awareness <- ifelse(
    is.na(primary_normal_awareness),
    NA,
    !primary_normal_awareness
  )
  primary_status <- primary_cutoff_result(primary_score, primary_cutoff)

  overall_group <- ifelse(
    is.na(primary_impaired_awareness),
    "Unable to calculate; missing required values",
    ifelse(primary_impaired_awareness, "IAH", "NAH")
  )

  data.frame(
    participant_id = participant_ids_for(df, participant_ids),
    unadjusted_45_sum = unadjusted_45_sum,
    adjusted_45_vs_90_sum = adjusted_45_vs_90_sum,
    unadjusted_cutoff = unadjusted_cutoff,
    adjusted_cutoff = adjusted_cutoff,
    unadjusted_distance = unadjusted_45_sum - unadjusted_cutoff,
    adjusted_distance = adjusted_45_vs_90_sum - adjusted_cutoff,
    unadjusted_normal_awareness = unadjusted_normal_awareness,
    adjusted_normal_awareness = adjusted_normal_awareness,
    unadjusted_impaired_awareness = unadjusted_impaired_awareness,
    adjusted_impaired_awareness = adjusted_impaired_awareness,
    unadjusted_at_risk = unadjusted_impaired_awareness,
    adjusted_at_risk = adjusted_impaired_awareness,
    discordant_flag = discordant_flag,
    missing_value_count = missing_value_count,
    missing_45_count = missing_45_count,
    missing_90_count = missing_90_count,
    score_method = score_method,
    score_method_label = score_method_label(score_method),
    score_method_detail = score_method_detail(score_method),
    primary_score = primary_score,
    primary_cutoff = primary_cutoff,
    primary_distance = primary_score - primary_cutoff,
    primary_normal_awareness = primary_normal_awareness,
    primary_impaired_awareness = primary_impaired_awareness,
    primary_status = primary_status,
    primary_cutoff_result = primary_status,
    imputation_used = FALSE,
    imputed_variables = "",
    overall_group = overall_group,
    check.names = FALSE
  )
}

score_summary_counts <- function(scores) {
  if ("primary_impaired_awareness" %in% names(scores)) {
    unable <- is.na(scores$primary_impaired_awareness)
    impaired <- scores$primary_impaired_awareness %in% TRUE
  } else {
    unable <- is.na(scores$unadjusted_at_risk) | is.na(scores$adjusted_at_risk)
    unadjusted_impaired <- scores$unadjusted_at_risk %in% TRUE
    adjusted_impaired <- scores$adjusted_at_risk %in% TRUE
    impaired <- (unadjusted_impaired | adjusted_impaired) & !unable
  }

  methods <- if ("score_method" %in% names(scores)) {
    scores$score_method
  } else {
    rep(NA_character_, nrow(scores))
  }

  data.frame(
    subjects_scored = nrow(scores),
    impaired_awareness = sum(impaired & !unable),
    normal_awareness = sum(!impaired & !unable),
    adjusted_method = sum(methods == "adjusted_45_vs_90", na.rm = TRUE),
    unadjusted_method = sum(methods == "unadjusted_45", na.rm = TRUE),
    unable_to_calculate = sum(unable),
    check.names = FALSE
  )
}

format_score_results_for_display <- function(scores) {
  expected_cols <- c(
    "participant_id",
    "unadjusted_45_sum",
    "adjusted_45_vs_90_sum",
    "unadjusted_at_risk",
    "adjusted_at_risk",
    "discordant_flag",
    "missing_value_count",
    "score_method_label",
    "primary_score",
    "primary_cutoff",
    "primary_cutoff_result",
    "imputation_used",
    "overall_group"
  )
  missing_cols <- setdiff(expected_cols, names(scores))

  if (length(missing_cols) > 0) {
    stop("Missing score result columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  data.frame(
    "Subject ID" = scores$participant_id,
    "Score method" = scores$score_method_label,
    "Score" = round(scores$primary_score, 2),
    "Cutoff" = round(scores$primary_cutoff, 2),
    "Cutoff result" = scores$primary_cutoff_result,
    "Awareness status" = scores$overall_group,
    check.names = FALSE
  )
}

score_contributions <- function(row, vars = CLAMP_VARIABLES) {
  cols_45 <- paste0(vars, "_45")
  cols_90 <- paste0(vars, "_90")
  missing_cols <- setdiff(c(cols_45, cols_90), names(row))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  row <- coerce_clamp_numeric(row, c(cols_45, cols_90))
  data.frame(
    variable = vars,
    value_90 = as.numeric(row[1, cols_90, drop = TRUE]),
    value_45 = as.numeric(row[1, cols_45, drop = TRUE]),
    adjusted_contribution = as.numeric(row[1, cols_45, drop = TRUE]) -
      as.numeric(row[1, cols_90, drop = TRUE]),
    check.names = FALSE
  )
}

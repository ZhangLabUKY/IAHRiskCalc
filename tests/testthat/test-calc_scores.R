test_that("calc_clamp_scores uses all 45 and paired 90 columns", {
  df <- as.data.frame(as.list(stats::setNames(rep(0, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  for (var in CLAMP_VARIABLES) {
    df[[paste0(var, "_45")]] <- 4
    df[[paste0(var, "_90")]] <- 1
  }
  rownames(df) <- "Example"

  scores <- calc_clamp_scores(df)

  expect_equal(scores$participant_id, "Example")
  expect_equal(scores$unadjusted_45_sum, 80)
  expect_equal(scores$adjusted_45_vs_90_sum, 60)
  expect_true(scores$unadjusted_at_risk)
  expect_true(scores$adjusted_at_risk)
  expect_equal(scores$overall_group, "IAH")
})

test_that("single-cutoff positive cases are flagged as likely IAH", {
  df <- as.data.frame(as.list(stats::setNames(rep(0, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  for (var in CLAMP_VARIABLES) {
    df[[paste0(var, "_45")]] <- 4
    df[[paste0(var, "_90")]] <- 4
  }

  scores <- calc_clamp_scores(df)

  expect_true(scores$unadjusted_at_risk)
  expect_false(scores$adjusted_at_risk)
  expect_true(scores$discordant_flag)
  expect_equal(scores$overall_group, "Likely IAH")
})

test_that("below-cutoff cases are labelled NAH", {
  df <- as.data.frame(as.list(stats::setNames(rep(0, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  for (var in CLAMP_VARIABLES) {
    df[[paste0(var, "_45")]] <- 1
    df[[paste0(var, "_90")]] <- 1
  }

  scores <- calc_clamp_scores(df)

  expect_false(scores$unadjusted_at_risk)
  expect_false(scores$adjusted_at_risk)
  expect_false(scores$discordant_flag)
  expect_equal(scores$overall_group, "NAH")
})

test_that("missing required columns are reported", {
  df <- data.frame(Heart_45 = 1, check.names = FALSE)
  check <- validate_required_columns(df)

  expect_false(check$ok)
  expect_true("Heart_90" %in% check$missing_cols)
})

test_that("mean imputation records imputed variables", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  reference <- df
  reference$Heart_45 <- c(3)
  df$Heart_45 <- NA

  imputed <- impute_missing_with_means(df, reference)
  scores <- apply_imputation_metadata(calc_clamp_scores(imputed), imputed)

  expect_equal(imputed$Heart_45, 3)
  expect_true(scores$imputation_used)
  expect_equal(scores$imputed_variables, "Heart_45")
})

test_that("display score results keep compact risk columns only", {
  df <- as.data.frame(as.list(stats::setNames(rep(0, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  for (var in CLAMP_VARIABLES) {
    df[[paste0(var, "_45")]] <- 4
    df[[paste0(var, "_90")]] <- 1
  }
  rownames(df) <- "Example"

  scores <- calc_clamp_scores(df)
  display_scores <- format_score_results_for_display(scores)

  expect_true("unadjusted_distance" %in% names(scores))
  expect_true("adjusted_distance" %in% names(scores))
  expect_equal(nrow(display_scores), nrow(scores))
  expect_equal(
    names(display_scores),
    c(
      "Subject ID",
      "Unadjusted 45 mg/dL score (cutoff >= 66.5)",
      "Adjusted 45-vs-90 score (cutoff >= 25)",
      "Risk status"
    )
  )
  expect_equal(display_scores[["Subject ID"]], "Example")
  expect_equal(display_scores[["Unadjusted 45 mg/dL score (cutoff >= 66.5)"]], 80)
  expect_equal(display_scores[["Adjusted 45-vs-90 score (cutoff >= 25)"]], 60)
  expect_equal(display_scores[["Risk status"]], "IAH")
  expect_false("unadjusted_distance" %in% names(display_scores))
  expect_false("adjusted_distance" %in% names(display_scores))
  expect_true("unadjusted_45_sum" %in% names(scores))
  expect_true("adjusted_45_vs_90_sum" %in% names(scores))
  expect_true("overall_group" %in% names(scores))
})

test_that("score summary counts separate any positive, both positive, and discordant rows", {
  scores <- data.frame(
    unadjusted_at_risk = c(TRUE, TRUE, FALSE, FALSE, NA),
    adjusted_at_risk = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    check.names = FALSE
  )

  counts <- score_summary_counts(scores)

  expect_equal(counts$subjects_scored, 5)
  expect_equal(counts$any_cutoff_positive, 3)
  expect_equal(counts$both_cutoffs_positive, 1)
  expect_equal(counts$discordant, 2)
  expect_equal(counts$unable_to_calculate, 1)
})

test_that("score contributions expose unadjusted and adjusted component values", {
  df <- as.data.frame(as.list(stats::setNames(rep(0, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  df$Heart_45 <- 6
  df$Heart_90 <- 2

  contributions <- score_contributions(df, vars = "Heart")

  expect_equal(contributions$variable, "Heart")
  expect_equal(contributions$value_45, 6)
  expect_equal(contributions$value_90, 2)
  expect_equal(contributions$adjusted_contribution, 4)
})

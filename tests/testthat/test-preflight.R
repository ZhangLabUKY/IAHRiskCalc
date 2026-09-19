test_that("synthetic clean wide upload preflights offset warnings before scoring", {
  df <- score_fixture()
  df <- data.frame("Subject ID" = c("S001", "S002"), df[rep(1, 2), ], check.names = FALSE)
  df$Epinephrine_90[[1]] <- 0
  df$Norepinephrine_90[[1]] <- 0
  path <- write_wide_csv_fixture(df)

  preflight <- preflight_upload(path, "synthetic_wide.csv")

  expect_true(preflight$ok)
  expect_equal(preflight$audit$detected_format, "pattern_wide")
  expect_equal(nrow(preflight$data), 2)
  expect_equal(nrow(preflight$offset_fields), 2)
  expect_true(all(preflight$offset_fields$participant_id == "S001"))
  expect_true(all(preflight$offset_fields$variable %in% c("Epinephrine_90", "Norepinephrine_90")))
})

test_that("synthetic workbook upload preflights audit without scoring", {
  path <- write_raw_grouped_xlsx_fixture()

  preflight <- preflight_upload(path, "synthetic_raw_grouped.xlsx")

  expect_true(preflight$ok)
  expect_equal(preflight$audit$detected_format, "raw_grouped_workbook")
  expect_equal(length(preflight$audit$parsed_columns), length(required_score_cols()))
  expect_equal(nrow(preflight$data), 2)
})

test_that("preflight detects missing required values before scoring", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df$Heart_45 <- NA

  preflight <- preflight_manual(df)

  expect_true(preflight$has_missing_required)
  expect_equal(preflight$missing_values$missing_value_count[[1]], 1)
  expect_match(preflight$missing_values$missing_variables[[1]], "Heart_45")
})

test_that("preflight reapplies selected subject IDs before warnings", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df <- data.frame(Notes = "Selected subject", df, check.names = FALSE)
  normalized <- normalize_wide_upload(df)
  normalized$data$Heart_45 <- NA

  preflight <- preflight_normalized(normalized)
  selected <- apply_subject_id_selection_to_preflight(preflight, "column_1")

  expect_equal(rownames(selected$data), "Selected subject")
  expect_equal(selected$missing_values$participant_id, "Selected subject")
})

test_that("missing-value display uses friendly labels and whole-number counts", {
  missing_values <- data.frame(
    participant_id = "S001",
    missing_value_count = 2L,
    missing_variables = "Heart_45, Shaky_90",
    check.names = FALSE
  )

  display <- format_missing_values_for_display(missing_values)

  expect_equal(names(display), c("Subject ID", "Missing value count", "Missing variables"))
  expect_equal(display[["Subject ID"]], "S001")
  expect_equal(display[["Missing value count"]], "2")
  expect_equal(display[["Missing variables"]], "Heart_45, Shaky_90")
})

test_that("symptom ratings require whole numbers from zero to six", {
  valid <- score_fixture(value_45 = 4, value_90 = 1)
  valid$Heart_90 <- 0
  valid$Dizzy_45 <- 6

  valid_check <- validate_scoring_input(valid)
  expect_true(valid_check$ok)
  expect_true(valid_check$symptom_check$ok)

  physiological_decimal <- valid
  physiological_decimal$Cortisol_45 <- 1.25
  expect_true(validate_scoring_input(physiological_decimal)$ok)

  invalid_cases <- list(
    `Heart_90` = 0.2,
    `Shaky_45` = 2.5,
    `Sweaty_90` = -1,
    `Hungry_45` = 7
  )

  for (field in names(invalid_cases)) {
    invalid <- valid
    invalid[[field]] <- invalid_cases[[field]]
    invalid_check <- validate_scoring_input(invalid)

    expect_false(invalid_check$ok)
    expect_false(invalid_check$symptom_check$ok)
    expect_equal(invalid_check$symptom_check$invalid_values$symptom_field, field)
    expect_match(invalid_check$message, "whole numbers from 0 to 6", fixed = TRUE)
    expect_match(invalid_check$message, field, fixed = TRUE)
  }
})

test_that("upload preflight reports invalid symptom ratings", {
  df <- score_fixture(value_45 = 4, value_90 = 1)
  df <- data.frame("Subject ID" = "S001", df, check.names = FALSE)
  df$Heart_45 <- 0.2
  path <- write_wide_csv_fixture(df)

  preflight <- preflight_upload(path, "invalid_symptom.csv")
  display <- format_invalid_symptom_ratings_for_display(
    preflight$symptom_check$invalid_values
  )

  expect_false(preflight$ok)
  expect_true(preflight$has_invalid_symptom_ratings)
  expect_equal(names(display), c("Subject ID", "Symptom field", "Entered value", "Issue"))
  expect_equal(display[["Subject ID"]], "S001")
  expect_equal(display[["Symptom field"]], "Heart_45")
  expect_equal(display[["Entered value"]], "0.2")
  expect_equal(display[["Issue"]], "Not a whole number")
})

test_that("real clean wide upload preflights offset warnings before scoring", {
  skip_if_no_source_checkout()
  path <- reference_data_file("ClampData_clean_wide.csv")

  preflight <- preflight_upload(path, "ClampData_clean_wide.csv")

  expect_true(preflight$ok)
  expect_equal(preflight$audit$detected_format, "pattern_wide")
  expect_equal(nrow(preflight$data), 19)
  expect_equal(nrow(preflight$offset_fields), 2)
  expect_true(all(preflight$offset_fields$participant_id == "03JC"))
  expect_true(all(preflight$offset_fields$variable %in% c("Epinephrine_90", "Norepinephrine_90")))
})

test_that("real workbook upload preflights audit without scoring", {
  skip_if_no_source_checkout()
  path <- reference_data_file("ClampData.xlsx")

  preflight <- preflight_upload(path, "ClampData.xlsx")

  expect_true(preflight$ok)
  expect_equal(preflight$audit$detected_format, "raw_grouped_workbook")
  expect_equal(length(preflight$audit$parsed_columns), length(required_score_cols()))
  expect_equal(nrow(preflight$data), 19)
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

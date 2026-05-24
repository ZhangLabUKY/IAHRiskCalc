test_that("canonical CSV uploads are normalized and unrelated columns are ignored", {
  df <- score_fixture()
  df <- data.frame("Participant ID" = "P001", Notes = "ignore me", df, check.names = FALSE)
  path <- tempfile(fileext = ".csv")
  write.csv(df, path, row.names = FALSE, na = "")

  normalized <- normalize_uploaded_clamp(path, original_name = "canonical.csv")

  expect_equal(normalized$audit$detected_format, "pattern_wide")
  expect_equal(rownames(normalized$data), "P001")
  expect_true(all(required_score_cols() %in% names(normalized$data)))
  expect_false("Notes" %in% names(normalized$data))
  expect_equal(normalized$data$Heart_45, 4)
})

test_that("wide uploads prefer ID-like subject columns", {
  id_names <- c("Subject ID", "subject_id", "participant id", "patient_id", "record_id")

  for (id_name in id_names) {
    df <- score_fixture()
    df <- data.frame(Notes = "not preferred", df, check.names = FALSE)
    df <- cbind(stats::setNames(data.frame("S001", check.names = FALSE), id_name), df)

    normalized <- normalize_wide_upload(df)

    expect_equal(rownames(normalized$data), "S001")
    expect_equal(normalized$audit$subject_id$default_key, "column_1")
    expect_true(id_name %in% normalized$audit$subject_id$candidates$label)
  }
})

test_that("wide uploads expose unrelated ID candidates but default to generated subject IDs", {
  df <- score_fixture()
  df <- data.frame(Notes = "Free text", df, check.names = FALSE)

  normalized <- normalize_wide_upload(df)

  expect_equal(rownames(normalized$data), "Subject_1")
  expect_equal(normalized$audit$subject_id$default_key, "__generated_subject_ids__")
  expect_true("Notes" %in% normalized$audit$subject_id$candidates$label)

  selected <- apply_subject_ids_to_normalized(normalized, "column_1")
  expect_equal(rownames(selected$data), "Free text")
})

test_that("CSV uploads parse when the file path contains spaces", {
  df <- score_fixture()
  df <- data.frame("Subject ID" = "S001", df, check.names = FALSE)
  spaced_dir <- file.path(tempdir(), "csv upload path with spaces")
  dir.create(spaced_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(spaced_dir, "sample upload.csv")
  write.csv(df, path, row.names = FALSE, na = "")

  normalized <- normalize_uploaded_clamp(path, original_name = "sample upload.csv")

  expect_equal(normalized$audit$detected_format, "pattern_wide")
  expect_equal(rownames(normalized$data), "S001")
  expect_equal(normalized$data$Heart_45, 4)
})

test_that("pattern-wide uploads normalize observed clean wide names", {
  df <- data.frame(
    patient_id = "P001",
    gold_score = 1,
    heart_pounding_baseline = 999,
    heart_pounding_90 = 2,
    heart_pounding_45 = 4,
    shaky_tremulous_90 = 1,
    shaky_tremulous_45 = 3,
    tired_drowsy_90 = 5,
    tired_drowsy_45 = 6,
    free_fatty_acids_90 = 8,
    free_fatty_acids_45 = 16,
    notes = "ignore",
    check.names = FALSE
  )

  normalized <- normalize_wide_upload(df)

  expect_equal(normalized$data$Heart_90, 2)
  expect_equal(normalized$data$Heart_45, 4)
  expect_equal(normalized$data$Shaky_90, 1)
  expect_equal(normalized$data$Tired_45, 6)
  expect_equal(normalized$data$FreeFattyAcids_90, 8)
  expect_true("gold_score" %in% normalized$audit$ignored_columns)
  expect_true("heart_pounding_baseline" %in% normalized$audit$ignored_columns)
})

test_that("duplicate wide mappings stop with a clear error", {
  df <- data.frame(
    patient_id = "P001",
    heart_45 = 1,
    heart_pounding_45 = 2,
    check.names = FALSE
  )

  expect_error(
    normalize_wide_upload(df),
    "Multiple columns.*Heart_45"
  )
})

test_that("partial wide parses proceed with missing columns as NA and audit warnings", {
  df <- data.frame(
    patient_id = "P001",
    heart_pounding_45 = 4,
    heart_pounding_90 = 2,
    check.names = FALSE
  )

  normalized <- normalize_wide_upload(df)

  expect_equal(normalized$data$Heart_45, 4)
  expect_true(is.na(normalized$data$Shaky_45))
  expect_match(normalized$audit$parser_warnings[[1]], "Shaky_45")
})

test_that("raw grouped workbook headers are mapped to canonical scoring columns", {
  normalized <- normalize_raw_grouped_upload(raw_grouped_fixture())

  expect_equal(normalized$audit$detected_format, "raw_grouped_workbook")
  expect_equal(rownames(normalized$data), c("P001", "P002"))
  expect_equal(normalized$audit$subject_id$default_key, "column_1")
  expect_equal(normalized$data$Heart_90[[1]], "1")
  expect_equal(normalized$data$Heart_45[[1]], "4")
  expect_equal(normalized$data$Shaky_45[[1]], "4")
})

test_that("raw grouped Tired/Drowsy headers map to Tired", {
  raw <- raw_grouped_fixture()
  raw[1, raw[1, ] == "Tired"] <- "Tired/Drowsy"

  normalized <- normalize_raw_grouped_upload(raw)

  expect_equal(normalized$data$Tired_90[[1]], "1")
  expect_equal(normalized$data$Tired_45[[1]], "4")
})

test_that("header variants are recognized and baseline is ignored", {
  normalized <- normalize_raw_grouped_upload(raw_grouped_fixture())

  expect_true(all(required_score_cols() %in% names(normalized$data)))
  expect_false(any(grepl("Baseline", names(normalized$data), ignore.case = TRUE)))
  expect_equal(normalized$data$Heart_90[[1]], "1")
  expect_equal(normalized$data$Heart_45[[1]], "4")
})

test_that("physiological values are not log transformed during import", {
  normalized <- normalize_raw_grouped_upload(raw_grouped_fixture())

  expect_equal(suppressWarnings(as.numeric(normalized$data$Cortisol_90[[1]])), 8)
  expect_equal(suppressWarnings(as.numeric(normalized$data$Cortisol_45[[1]])), 32)
})

test_that("strict and imputation behavior work after raw upload parsing", {
  raw <- raw_grouped_fixture()
  normalized <- normalize_raw_grouped_upload(raw)
  normalized$data$Dopamine_45[[1]] <- NA
  missing <- missing_required_values(normalized$data)

  expect_equal(missing$missing_value_count[[1]], 1)
  expect_match(missing$missing_variables[[1]], "Dopamine_45")

  reference <- score_fixture()
  reference$Dopamine_45 <- 9
  imputed <- impute_missing_with_means(normalized$data, reference)
  scores <- apply_imputation_metadata(calc_clamp_scores(imputed), imputed)

  expect_equal(imputed$Dopamine_45[[1]], 9)
  expect_true(scores$imputation_used[[1]])
  expect_match(scores$imputed_variables[[1]], "Dopamine_45")
})

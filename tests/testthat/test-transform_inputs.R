test_that("symptom values are unchanged and physiological values are log2 transformed", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(NA_real_, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  for (var in SYMPTOM_VARIABLES) {
    df[[paste0(var, "_45")]] <- 4
    df[[paste0(var, "_90")]] <- 2
  }
  for (var in COUNTERREGULATORY_VARIABLES) {
    df[[paste0(var, "_45")]] <- 8
    df[[paste0(var, "_90")]] <- 4
  }

  transformed <- transform_physiological_responses(df)

  expect_true(transformed$ok)
  expect_equal(transformed$data$Heart_45, 4)
  expect_equal(transformed$data$Cortisol_45, 3)
  expect_equal(transformed$data$Cortisol_90, 2)
})

test_that("non-positive physiological values require confirmation", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df <- df[rep(1, 2), ]
  df$Cortisol_45 <- c(0, 5)
  rownames(df) <- c("P001", "P002")

  transformed <- transform_physiological_responses(df, allow_offset = FALSE)

  expect_false(transformed$ok)
  expect_true(transformed$needs_offset_confirmation)
  expect_equal(transformed$offset_fields$participant_id[[1]], "P001")
  expect_equal(transformed$offset_fields$variable[[1]], "Cortisol_45")
  expect_equal(transformed$offset_fields$offset_value[[1]], 4)
})

test_that("confirmed non-positive physiological values use per-variable minimum-positive offset before log2", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df <- df[rep(1, 2), ]
  df$Cortisol_45 <- c(0, 5)

  transformed <- transform_physiological_responses(df, allow_offset = TRUE)

  expect_true(transformed$ok)
  expect_true(transformed$offset_applied)
  expect_equal(transformed$data$Cortisol_45[[1]], log2(4))
  expect_equal(transformed$data$Cortisol_45[[2]], log2(5))
})

test_that("non-positive physiological values fail clearly when their variable has no positive anchor", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df$Cortisol_45 <- 0

  transformed <- transform_physiological_responses(df, allow_offset = TRUE)

  expect_false(transformed$ok)
  expect_false(transformed$needs_offset_confirmation)
  expect_match(transformed$message, "no positive raw values")
  expect_match(transformed$message, "Cortisol_45")
})

test_that("offset field display uses friendly labels", {
  offset_fields <- data.frame(
    participant_id = "P001",
    variable = "Cortisol_45",
    raw_value = 0,
    offset_value = 4,
    check.names = FALSE
  )

  display <- format_offset_fields_for_display(offset_fields)

  expect_equal(names(display), c("Subject ID", "Variable", "Raw Value", "Offset Value"))
  expect_equal(display[["Subject ID"]], "P001")
  expect_equal(display[["Variable"]], "Cortisol_45")
  expect_equal(display[["Raw Value"]], "0.00")
  expect_equal(display[["Offset Value"]], "4.00")
})

test_that("transformation metadata records affected fields", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df <- df[rep(1, 2), ]
  df$Cortisol_45 <- c(0, 5)
  rownames(df) <- c("P001", "P002")

  transformed <- transform_physiological_responses(df, allow_offset = TRUE)
  scores <- calc_clamp_scores(transformed$data)
  scores <- apply_transform_metadata(scores, transformed)

  expect_true(scores$phys_log2_transformed[[1]])
  expect_true(scores$phys_offset_applied[[1]])
  expect_equal(scores$phys_offset_variables[[1]], "Cortisol_45")
  expect_equal(scores$phys_offset_variables[[2]], "")
})

test_that("uploaded-data imputation means are calculated after transformation", {
  df <- as.data.frame(
    as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
    check.names = FALSE
  )
  df <- df[rep(1, 2), ]
  rownames(df) <- c("P001", "P002")
  df$Cortisol_45 <- c(4, NA)

  transformed <- transform_physiological_responses(df, allow_offset = TRUE)
  imputed <- impute_missing_with_means(transformed$data, transformed$data)

  expect_equal(imputed$Cortisol_45[[2]], 2)
})

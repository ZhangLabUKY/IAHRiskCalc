manual_server_inputs <- function(value_45 = 8, value_90 = 1, subject_id = "Manual subject") {
  values <- list(
    input_mode = "manual",
    manual_participant_id = subject_id
  )
  for (var in CLAMP_VARIABLES) {
    values[[manual_input_id(var, 90)]] <- value_90
    values[[manual_input_id(var, 45)]] <- value_45
  }
  values
}

file_input_value <- function(path, name = basename(path), type = "text/csv") {
  data.frame(
    name = name,
    size = file.info(path)$size,
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

set_many_inputs <- function(session, values) {
  do.call(session$setInputs, values)
}

test_that("app UI exposes the calculator, plot, and methods workflows", {
  html <- paste(as.character(iah_app_ui()), collapse = "\n")

  expect_match(html, "IAH Clamp-Based Risk Calculator", fixed = TRUE)
  expect_match(html, "Calculator", fixed = TRUE)
  expect_match(html, "Plots", fixed = TRUE)
  expect_match(html, "Methods", fixed = TRUE)
  expect_match(html, "Upload file", fixed = TRUE)
  expect_match(html, "Manual entry", fixed = TRUE)
  expect_match(html, "No Imputation", fixed = TRUE)
  expect_match(html, "Mean imputation", fixed = TRUE)
  expect_match(html, "Both scores below threshold: IAH.", fixed = TRUE)
  expect_match(html, "Exactly one score below threshold: Likely IAH.", fixed = TRUE)
  expect_match(html, "Both scores greater than or equal to threshold: NAH.", fixed = TRUE)
})

test_that("manual entry UI uses readable clamp labels", {
  html <- paste(as.character(manual_entry_ui()), collapse = "\n")

  expect_match(html, "Heart Pounding", fixed = TRUE)
  expect_match(html, "Shaky/Tremulous", fixed = TRUE)
  expect_match(html, "Tired/Drowsy", fixed = TRUE)
  expect_match(html, "Free fatty acids", fixed = TRUE)
  expect_match(html, "Pancreatic Polypeptide", fixed = TRUE)
})

test_that("manual entry initializes before manual inputs are populated", {
  shiny::testServer(iah_app_server, {
    session$setInputs(input_mode = "manual")

    preflight <- current_preflight()
    expect_true(preflight$ok)
    expect_true(preflight$has_missing_required)
    expect_equal(rownames(preflight$data), "New subject")
    expect_equal(preflight$missing_values$participant_id, "New subject")
    expect_equal(preflight$missing_values$missing_value_count, length(required_score_cols()))
  })
})

test_that("manual entry scoring succeeds with complete values", {
  shiny::testServer(iah_app_server, {
    set_many_inputs(session, manual_server_inputs())
    session$setInputs(calculate = 1)

    result <- current_result()
    expect_true(result$ok)
    expect_equal(result$source, "manual")
    expect_equal(result$scores$participant_id, "Manual subject")
    expect_equal(result$scores$overall_group, "NAH")
    expect_true(has_successful_result())
  })
})

test_that("upload scoring stores uploaded data for plot workflows", {
  path <- write_wide_csv_fixture()

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "synthetic_wide.csv")
    )
    session$setInputs(calculate = 1)

    result <- current_result()
    state <- uploaded_state()
    expect_true(result$ok)
    expect_equal(result$source, "upload")
    expect_false(is.null(state))
    expect_equal(rownames(state$df), "S001")
    expect_equal(state$scores$participant_id, "S001")
  })
})

test_that("upload offset confirmation blocks then scores with a positive anchor", {
  df <- score_fixture(value_45 = 8, value_90 = 1)
  df <- data.frame("Subject ID" = c("S001", "S002"), df[rep(1, 2), ], check.names = FALSE)
  df$Cortisol_45[[1]] <- 0
  df$Cortisol_45[[2]] <- 10
  path <- write_wide_csv_fixture(df)

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "offset_upload.csv")
    )
    session$setInputs(calculate = 1)

    pending <- current_result()
    expect_false(pending$ok)
    expect_true(pending$needs_offset)
    expect_true(isTRUE(pending$transform$needs_offset_confirmation))

    session$setInputs(confirm_offset = 1)
    result <- current_result()
    expect_true(result$ok)
    expect_true(any(result$scores$phys_offset_applied))
    expect_true(has_successful_result())
  })
})

test_that("missing values use no imputation by default and mean imputation when selected", {
  df <- score_fixture(value_45 = 8, value_90 = 1)
  df <- data.frame("Subject ID" = c("S001", "S002"), df[rep(1, 2), ], check.names = FALSE)
  df$Heart_45[[1]] <- NA_real_
  df$Heart_45[[2]] <- 8
  path <- write_wide_csv_fixture(df)

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "missing_upload.csv")
    )

    session$setInputs(calculate = 1)
    strict_result <- current_result()
    expect_true(strict_result$ok)
    expect_equal(
      strict_result$scores$overall_group[[1]],
      "Unable to calculate; missing required values"
    )
    expect_false(strict_result$scores$imputation_used[[1]])

    session$setInputs(missing_mode = "impute")
    session$setInputs(calculate = 2)
    imputed_result <- current_result()
    expect_true(imputed_result$ok)
    expect_true(imputed_result$scores$imputation_used[[1]])
    expect_equal(imputed_result$scores$overall_group[[1]], "NAH")
  })
})

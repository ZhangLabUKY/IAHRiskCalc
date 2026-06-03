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
})

test_that("manual entry UI uses readable clamp labels", {
  html <- paste(as.character(manual_entry_ui()), collapse = "\n")

  expect_match(html, "Heart Pounding", fixed = TRUE)
  expect_match(html, "Shaky/Tremulous", fixed = TRUE)
  expect_match(html, "Tired/Drowsy", fixed = TRUE)
  expect_match(html, "Free fatty acids", fixed = TRUE)
  expect_match(html, "Pancreatic Polypeptide", fixed = TRUE)
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

test_that("manual mode initializes safely before fields are populated", {
  shiny::testServer(iah_app_server, {
    session$setInputs(input_mode = "manual")

    preflight <- current_preflight()

    expect_equal(nrow(preflight$data), 1)
    expect_equal(rownames(preflight$data), "New subject")
    expect_true(all(is.na(preflight$data[1, required_score_cols()])))
    expect_true(show_manual_entry())
  })
})

test_that("manual edit restores session values and recalculates from edits", {
  shiny::testServer(iah_app_server, {
    values <- manual_server_inputs(subject_id = "Manual cached subject")
    values[[manual_input_id("Heart", 45)]] <- 2
    set_many_inputs(session, values)
    session$setInputs(calculate = 1)

    first_result <- current_result()
    expect_true(first_result$ok)
    expect_false(show_manual_entry())
    expect_equal(manual_entry_cache()$values$Heart_45, 2)

    session$setInputs(edit_manual_entry = 1)
    expect_true(show_manual_entry())
    restored <- paste(
      as.character(manual_entry_ui(manual_entry_cache()$values)),
      collapse = "\n"
    )
    expect_match(restored, 'value="2"', fixed = TRUE)

    edit_values <- list()
    edit_values[[manual_input_id("Heart", 45)]] <- 12
    set_many_inputs(session, edit_values)
    session$setInputs(calculate = 2)

    second_result <- current_result()
    expect_true(second_result$ok)
    expect_equal(manual_entry_cache()$values$Heart_45, 12)
    expect_gt(
      second_result$scores$primary_score[[1]],
      first_result$scores$primary_score[[1]]
    )
  })
})

test_that("manual scoring stores profile state for plot workflows", {
  shiny::testServer(iah_app_server, {
    set_many_inputs(session, manual_server_inputs(subject_id = "Plot subject"))
    session$setInputs(calculate = 1)

    state <- profile_state()

    expect_false(is.null(state))
    expect_equal(state$source, "manual")
    expect_equal(rownames(state$df), "Plot subject")
    expect_equal(state$scores$participant_id, "Plot subject")
  })
})

test_that("single-subject cards use full awareness wording and risk gauge", {
  iah_score <- data.frame(
    primary_score = 16,
    primary_cutoff = 25,
    primary_impaired_awareness = TRUE,
    primary_cutoff_result = "Below cutoff: IAH",
    overall_group = "IAH",
    check.names = FALSE
  )
  nah_score <- iah_score
  nah_score$primary_score <- 30
  nah_score$primary_impaired_awareness <- FALSE
  nah_score$primary_cutoff_result <- "Meets cutoff: NAH"
  nah_score$overall_group <- "NAH"
  unable_score <- iah_score
  unable_score$primary_score <- NA_real_
  unable_score$primary_cutoff <- NA_real_
  unable_score$primary_impaired_awareness <- NA
  unable_score$primary_cutoff_result <- "Unable to calculate"
  unable_score$overall_group <- "Unable to calculate; missing required values"

  iah_html <- paste(as.character(single_score_cards(iah_score)), collapse = "\n")
  nah_html <- paste(as.character(single_score_cards(nah_score)), collapse = "\n")
  unable_html <- paste(
    as.character(single_score_cards(unable_score)),
    collapse = "\n"
  )

  expect_match(iah_html, "Impaired awareness of hypoglycemia", fixed = TRUE)
  expect_match(nah_html, "Normal awareness of hypoglycemia", fixed = TRUE)
  expect_match(iah_html, "IAH Risk Prediction", fixed = TRUE)
  expect_match(iah_html, "risk-gauge iah", fixed = TRUE)
  expect_match(nah_html, "risk-gauge nah", fixed = TRUE)
  expect_match(unable_html, "risk-gauge unknown", fixed = TRUE)
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

test_that("multi-subject summary cards show impaired-range counts", {
  scores <- data.frame(
    primary_impaired_awareness = c(TRUE, TRUE, FALSE, FALSE, NA),
    score_method = c(
      "adjusted_45_vs_90",
      "unadjusted_45",
      "adjusted_45_vs_90",
      "unadjusted_45",
      "unable_to_calculate"
    ),
    check.names = FALSE
  )

  html <- paste(as.character(score_summary_cards(scores)), collapse = "\n")

  expect_match(html, "IAH", fixed = TRUE)
  expect_match(html, "NAH", fixed = TRUE)
  expect_match(html, "Primary score below cutoff", fixed = TRUE)
  expect_match(html, "Primary score meets or exceeds cutoff", fixed = TRUE)
  expect_match(html, "Adjusted method", fixed = TRUE)
  expect_match(html, "Unadjusted method", fixed = TRUE)
  expect_match(html, "score-value\">2<", fixed = TRUE)
  expect_match(html, "score-value\">1<", fixed = TRUE)
})

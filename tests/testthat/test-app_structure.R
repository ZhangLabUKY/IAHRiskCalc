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
  expected_version <- app_version_label()

  expect_match(html, "navbar-title-text", fixed = TRUE)
  expect_match(html, "navbar-brand", fixed = TRUE)
  expect_match(html, "IAH Clamp-Based Risk Calculator", fixed = TRUE)
  expect_match(html, "Calculator", fixed = TRUE)
  expect_match(html, "Plots", fixed = TRUE)
  expect_match(html, "Methods", fixed = TRUE)
  expect_match(html, "GitHub Repository", fixed = TRUE)
  expect_match(html, "https://github.com/ZhangLabUKY/IAHRiskCalc", fixed = TRUE)
  expect_match(html, "App Website", fixed = TRUE)
  expect_match(html, "https://zhanglabuky.github.io/IAHRiskCalc/", fixed = TRUE)
  expect_match(html, expected_version, fixed = TRUE)
  expect_false(grepl(
    paste0(
      '<li class="bslib-nav-item nav-item form-inline">\\s*',
      '<span class="navbar-version">',
      expected_version
    ),
    html
  ))
  expect_match(html, "Upload file", fixed = TRUE)
  expect_match(html, "Manual entry", fixed = TRUE)
  expect_match(html, "Example data", fixed = TRUE)
  expect_match(html, "Example subject 1", fixed = TRUE)
  expect_match(html, "Example subject 2", fixed = TRUE)
  expect_match(html, "Example subject 3", fixed = TRUE)
  expect_match(html, "width: 200px !important", fixed = TRUE)
  expect_match(html, "gap: 14px", fixed = TRUE)
  expect_match(html, "margin: 22px 0 32px", fixed = TRUE)
  expect_match(html, "manual-example-controls", fixed = TRUE)
  expect_match(html, "manual-example-buttons", fixed = TRUE)
  expect_match(html, "No Imputation", fixed = TRUE)
  expect_match(html, "Mean imputation", fixed = TRUE)
  expect_match(html, "Manual entry is the default workflow", fixed = TRUE)
  expect_match(html, "up to four subjects", fixed = TRUE)
  expect_match(html, "Patient Value, Overall Classification, and IAH Risk Prediction", fixed = TRUE)
  expect_match(html, "uploaded data use the existing per-column offset rule", fixed = TRUE)
  expect_match(html, "Manual entry uses a paired same-analyte rule", fixed = TRUE)
  expect_match(html, "Adjusted scoring shows the response profile plot", fixed = TRUE)
  expect_match(html, "Unadjusted scoring shows only the clamp response contribution plot", fixed = TRUE)
})

test_that("navbar and uploaded result CSS preserve contrast and horizontal cards", {
  skip_if_no_source_checkout()
  css <- paste(readLines(project_file("www", "styles.css")), collapse = "\n")

  expect_match(css, ".navbar-default .navbar-brand", fixed = TRUE)
  expect_match(css, "font-size: 21px", fixed = TRUE)
  expect_match(css, ".navbar-default .navbar-nav > .active > a", fixed = TRUE)
  expect_match(css, "background: rgba(255, 255, 255, 0.24)", fixed = TRUE)
  expect_match(css, "box-shadow: 0 -3px 0 #ffffff inset", fixed = TRUE)
  expect_match(css, "color: #ffffff !important", fixed = TRUE)
  expect_match(css, ".uploaded-results-page", fixed = TRUE)
  expect_match(css, "flex-direction: column", fixed = TRUE)
  expect_match(css, ".uploaded-subject-result .score-grid", fixed = TRUE)
  expect_match(css, "grid-template-columns: repeat(3, minmax(0, 1fr))", fixed = TRUE)
  expect_match(css, ".manual-example-controls", fixed = TRUE)
  expect_match(css, ".manual-example-buttons", fixed = TRUE)
  expect_match(css, ".manual-example-button", fixed = TRUE)
  expect_match(css, ".workflow-panel .manual-example-title", fixed = TRUE)
  expect_match(css, "margin: 22px 0 32px", fixed = TRUE)
  expect_match(css, "gap: 14px", fixed = TRUE)
  expect_match(css, "min-width: 200px", fixed = TRUE)
  expect_match(css, "width: 200px !important", fixed = TRUE)
  expect_match(css, "height: 44px", fixed = TRUE)
  expect_match(css, "white-space: normal", fixed = TRUE)
})

test_that("manual entry UI uses readable clamp labels", {
  html <- paste(as.character(manual_entry_ui()), collapse = "\n")

  expect_match(html, "Heart Pounding", fixed = TRUE)
  expect_match(html, "Shaky/Tremulous", fixed = TRUE)
  expect_match(html, "Tired/Drowsy", fixed = TRUE)
  expect_match(html, "Free fatty acids", fixed = TRUE)
  expect_match(html, "Pancreatic Polypeptide", fixed = TRUE)
})

test_that("manual examples are complete and classify as intended", {
  expected_groups <- list(
    example_subject_1 = NULL,
    example_subject_2 = "NAH",
    example_subject_3 = "IAH"
  )

  for (case in names(expected_groups)) {
    example <- manual_example_values(case)

    expect_equal(names(example$values), required_score_cols())
    expect_false(any(is.na(unlist(example$values, use.names = FALSE))))
    expect_match(example$participant_id, "^Example subject [1-3]$")

    df <- as.data.frame(example$values, check.names = FALSE)
    rownames(df) <- example$participant_id
    transformed <- transform_physiological_responses(
      df,
      allow_offset = FALSE,
      offset_method = "paired"
    )
    scores <- calc_clamp_scores(transformed$data)

    expect_true(transformed$ok)
    if (!is.null(expected_groups[[case]])) {
      expect_equal(scores$overall_group, expected_groups[[case]])
    } else {
      expect_false(is.na(scores$primary_score))
      expect_true(scores$overall_group %in% c("IAH", "NAH"))
    }
    expect_equal(scores$missing_value_count, 0)
  }
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

test_that("manual example buttons populate cache and clear stale results", {
  shiny::testServer(iah_app_server, {
    set_many_inputs(session, manual_server_inputs())
    session$setInputs(calculate = 1)

    expect_true(current_result()$ok)
    expect_false(is.null(profile_state()))

    cached_values <- list()
    for (index in 1:3) {
      inputs <- list(index)
      names(inputs) <- paste0("load_example_subject_", index)
      do.call(session$setInputs, inputs)
      example <- manual_entry_cache()

      expect_equal(example$participant_id, paste("Example subject", index))
      expect_equal(names(example$values), required_score_cols())
      expect_true(show_manual_entry())
      expect_true(is.null(current_result()))
      expect_true(is.null(profile_state()))

      cached_values[[index]] <- example$values
    }
    expect_false(identical(cached_values[[1]], cached_values[[2]]))
    expect_false(identical(cached_values[[2]], cached_values[[3]]))
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

test_that("uploaded score cards render four subjects per page", {
  df <- score_fixture(value_45 = 8, value_90 = 1)
  df <- df[rep(1, 5), , drop = FALSE]
  df[1, required_45_cols()] <- 1
  df[1, required_90_cols()] <- 1
  scores <- calc_clamp_scores(df, participant_ids = paste0("S00", seq_len(5)))

  html <- paste(as.character(uploaded_score_cards(scores)), collapse = "\n")

  expect_match(html, "Subjects 1-4", fixed = TRUE)
  expect_match(html, "Subjects 5-5", fixed = TRUE)
  expect_match(html, "Subject ID: S001", fixed = TRUE)
  expect_match(html, "Subject ID: S005", fixed = TRUE)
  expect_match(html, "Patient Value", fixed = TRUE)
  expect_match(html, "Overall Classification", fixed = TRUE)
  expect_match(html, "Impaired awareness of hypoglycemia", fixed = TRUE)
  expect_match(html, "Normal awareness of hypoglycemia", fixed = TRUE)
  expect_match(html, "IAH Risk Prediction", fixed = TRUE)
  expect_match(html, "risk-gauge iah", fixed = TRUE)
  expect_match(html, "risk-gauge nah", fixed = TRUE)
})

test_that("upload results show card tabs and header download without rendered table", {
  df <- score_fixture(value_45 = 8, value_90 = 1)
  df <- df[rep(1, 5), , drop = FALSE]
  path <- write_wide_csv_fixture(data.frame(
    "Subject ID" = paste0("S00", seq_len(5)),
    df,
    check.names = FALSE
  ))

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "five_subjects.csv")
    )
    session$setInputs(calculate = 1)

    section_html <- paste(as.character(output$results_section), collapse = "\n")
    cards_html <- paste(as.character(output$result_cards), collapse = "\n")

    expect_match(section_html, "Download CSV", fixed = TRUE)
    expect_no_match(section_html, "results_table", fixed = TRUE)
    expect_no_match(section_html, "results_table_base", fixed = TRUE)
    expect_match(cards_html, "Subjects 1-4", fixed = TRUE)
    expect_match(cards_html, "Subject ID: S001", fixed = TRUE)
    expect_match(cards_html, "Subject ID: S005", fixed = TRUE)
  })
})

test_that("manual single-subject results keep CSV hidden", {
  shiny::testServer(iah_app_server, {
    set_many_inputs(session, manual_server_inputs())
    session$setInputs(calculate = 1)

    section_html <- paste(as.character(output$results_section), collapse = "\n")

    expect_match(section_html, "Edit entered data", fixed = TRUE)
    expect_no_match(section_html, "Download CSV", fixed = TRUE)
  })
})

test_that("one-subject uploads still show upload cards and CSV download", {
  path <- write_wide_csv_fixture()

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "one_subject.csv")
    )
    session$setInputs(calculate = 1)

    section_html <- paste(as.character(output$results_section), collapse = "\n")
    cards_html <- paste(as.character(output$result_cards), collapse = "\n")

    expect_match(section_html, "Download CSV", fixed = TRUE)
    expect_no_match(section_html, "Edit entered data", fixed = TRUE)
    expect_match(cards_html, "Subjects 1-1", fixed = TRUE)
    expect_match(cards_html, "Subject ID: S001", fixed = TRUE)
  })
})

test_that("uploaded scoring still selects adjusted or unadjusted per subject", {
  df <- score_fixture(value_45 = 8, value_90 = 1)
  df <- df[rep(1, 2), , drop = FALSE]
  df[2, required_90_cols()] <- NA_real_
  path <- write_wide_csv_fixture(data.frame(
    "Subject ID" = c("Adjusted subject", "Unadjusted subject"),
    df,
    check.names = FALSE
  ))

  shiny::testServer(iah_app_server, {
    session$setInputs(
      input_mode = "upload",
      calculator_file = file_input_value(path, "mixed_methods.csv")
    )
    session$setInputs(calculate = 1)

    result <- current_result()

    expect_true(result$ok)
    expect_equal(
      result$scores$score_method,
      c("adjusted_45_vs_90", "unadjusted_45")
    )
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

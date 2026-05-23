skip_if_no_source_checkout()

test_that("app no longer depends on bundled clamp files at startup", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("clampImputed.csv", app_text, fixed = TRUE))
  expect_false(grepl("clampPreprocess.csv", app_text, fixed = TRUE))
  expect_false(grepl("read_clamp_csv", app_text, fixed = TRUE))
})

test_that("app keeps the requested top-level tabs", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl('tabPanel\\(\\s*"Calculator"', app_text))
  expect_true(grepl('tabPanel\\(\\s*"Plots"', app_text))
  expect_true(grepl('tabPanel\\(\\s*"Methods"', app_text))
  expect_false(grepl('tabPanel\\(\\s*"Response Profile"', app_text))
  expect_false(grepl('tabPanel\\(\\s*"Batch Upload"', app_text))
})

test_that("calculator scoring uses explicit button-triggered events", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("observeEvent\\(\\s*input\\$calculate", app_text))
  expect_true(grepl("observeEvent\\(\\s*input\\$confirm_offset", app_text))
  expect_false(grepl("score_requested\\(\\) > 0", app_text, fixed = TRUE))
})

test_that("missing value controls are conditionally rendered after preflight", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl('uiOutput\\("missing_mode_ui"\\)', app_text))
  expect_true(grepl("has_missing_required", app_text, fixed = TRUE))
  expect_true(grepl('radioButtons\\(\\s*"missing_mode"', app_text))
  expect_true(grepl('"No Imputation" = "strict"', app_text, fixed = TRUE))
  expect_false(grepl('"Strict mode" = "strict"', app_text, fixed = TRUE))
  expect_true(grepl("No missing required 45/90 values detected", app_text, fixed = TRUE))
})

test_that("methods tab documents current calculation workflow", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("Purpose and Inputs", app_text, fixed = TRUE))
  expect_true(grepl("Automated Preprocessing", app_text, fixed = TRUE))
  expect_true(grepl("80% of the minimum positive raw value", app_text, fixed = TRUE))
  expect_true(grepl("No Imputation", app_text, fixed = TRUE))
  expect_true(grepl("Mean imputation", app_text, fixed = TRUE))
  expect_true(grepl("Both scores at or above cutoff: IAH", app_text, fixed = TRUE))
  expect_true(grepl("Exactly one score at or above cutoff: Likely IAH", app_text, fixed = TRUE))
  expect_true(grepl("Both scores below cutoff: NAH", app_text, fixed = TRUE))
  expect_true(grepl("The Plots tab shows interactive Plotly figures", app_text, fixed = TRUE))
  expect_true(grepl("Calculator results can be downloaded as CSV", app_text, fixed = TRUE))
  expect_true(grepl("PDF downloads as one combined multi-page file", app_text, fixed = TRUE))
  expect_true(grepl("not a standalone clinical diagnostic tool", app_text, fixed = TRUE))
  expect_false(grepl("Strict mode leaves scores uncalculated", app_text, fixed = TRUE))
})

test_that("stylesheet defines clinical research theme basics", {
  css_path <- file.path(PROJECT_ROOT, "www", "styles.css")
  css_text <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("--iah-blue: #0033a0", css_text, fixed = TRUE))
  expect_true(grepl("linear-gradient(180deg, #edf4fb", css_text, fixed = TRUE))
  expect_true(grepl(".container-fluid > h2:first-child", css_text, fixed = TRUE))
  expect_true(grepl(".nav-tabs > li.active > a", css_text, fixed = TRUE))
  expect_true(grepl("box-shadow: var(--iah-shadow)", css_text, fixed = TRUE))
  expect_true(grepl(".methods h3", css_text, fixed = TRUE))
})

test_that("upload change clears previous scored state and creates preflight state", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("upload_preflight_state\\(preflight_upload", app_text))
  expect_true(grepl("last_result\\(NULL\\)", app_text))
  expect_true(grepl("uploaded_state\\(NULL\\)", app_text))
})

test_that("initial calculator warning area stays quiet before upload", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("Upload a file or choose manual entry to begin", app_text, fixed = TRUE))
  expect_true(grepl("Upload a clamp file to review data quality before scoring", app_text, fixed = TRUE))
  expect_true(grepl('uiOutput\\("preflight_panel"\\)', app_text))
  expect_true(grepl("review-panel", app_text, fixed = TRUE))
})

test_that("calculator headings are not numbered", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("1. Choose Input", app_text, fixed = TRUE))
  expect_false(grepl("2. Missing Values", app_text, fixed = TRUE))
  expect_false(grepl("3. Results", app_text, fixed = TRUE))
  expect_true(grepl('h3\\("Input"\\)', app_text))
  expect_true(grepl('h3\\("Missing Values"\\)', app_text))
  expect_true(grepl('h3\\("Results"\\)', app_text))
})

test_that("manual entry fields are only shown in manual mode", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  score_text <- paste(readLines(file.path(PROJECT_ROOT, "R", "calc_scores.R"), warn = FALSE), collapse = "\n")
  ui_text <- strsplit(app_text, "server <-", fixed = TRUE)[[1]][[1]]

  expect_false(grepl("Manual entry fields", app_text, fixed = TRUE))
  expect_false(grepl("Switch to Manual entry to enter values directly", app_text, fixed = TRUE))
  expect_true(grepl("input.input_mode == 'manual'", app_text, fixed = TRUE))
  expect_true(grepl("manual_entry_ui\\(\\)", app_text))
  expect_true(grepl('uiOutput\\("manual_entry_section"\\)', ui_text))
  expect_false(grepl("manual_entry_ui\\(\\)", ui_text))
  expect_true(grepl("output\\$manual_entry_section <- renderUI", app_text))
  expect_true(grepl('h3\\("Manual Entry"\\)', app_text))
  expect_true(grepl("Enter raw physiological values below", app_text, fixed = TRUE))
  expect_true(grepl("CLAMP_VARIABLE_LABELS <- c", score_text, fixed = TRUE))
  expect_true(grepl("Heart Pounding", score_text, fixed = TRUE))
  expect_true(grepl("Shaky/Tremulous", score_text, fixed = TRUE))
  expect_true(grepl("Tired/Drowsy", score_text, fixed = TRUE))
  expect_true(grepl("Free fatty acids", score_text, fixed = TRUE))
  expect_true(grepl("Pancreatic Polypeptide", score_text, fixed = TRUE))
  expect_true(grepl('class = "entry-name"', app_text, fixed = TRUE))
  expect_true(grepl('class = "entry-inputs"', app_text, fixed = TRUE))
})

test_that("manual mode suppresses upload preflight warnings in the main workspace", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("output\\$preflight_messages <- renderUI\\(\\{\\s*if \\(isTRUE\\(has_successful_result\\(\\)\\)\\).*?if \\(identical\\(input\\$input_mode, \"manual\"\\)\\)", app_text))
  expect_true(grepl("output\\$preflight_panel <- renderUI\\(\\{\\s*if \\(isTRUE\\(has_successful_result\\(\\)\\)\\).*?if \\(identical\\(input\\$input_mode, \"manual\"\\)\\)", app_text))
  expect_true(grepl("offset_warning_ui\\(result\\$transform\\)", app_text))
})

test_that("redundant result-level audit panel is removed", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("Upload and missing-value audit", app_text, fixed = TRUE))
  expect_false(grepl('tableOutput\\("upload_audit_table"\\)', app_text))
  expect_false(grepl("output\\$upload_audit_table <-", app_text))
  expect_false(grepl('tableOutput\\("missing_table"\\)', app_text))
  expect_false(grepl("output\\$missing_table <-", app_text))
  expect_false(grepl('tableOutput\\("offset_audit_table"\\)', app_text))
  expect_false(grepl("output\\$offset_audit_table <-", app_text))

  expect_true(grepl('uiOutput\\("preflight_panel"\\)', app_text))
  expect_true(grepl('tableOutput\\("preflight_upload_audit_table"\\)', app_text))
  expect_true(grepl('tableOutput\\("preflight_missing_table"\\)', app_text))
  expect_true(grepl('tableOutput\\("preflight_offset_table"\\)', app_text))
})

test_that("offset warning tables use friendly display labels", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("format_offset_fields_for_display\\(preflight\\$offset_fields\\)", app_text))
  expect_true(grepl("format_offset_fields_for_display\\(result\\$transform\\$offset_fields\\)", app_text))
})

test_that("calculator plots are consolidated into response profile tab", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl('tags\\$summary\\("Plots"\\)', app_text))
  expect_false(grepl("calculator_profile_plot", app_text, fixed = TRUE))
  expect_false(grepl("calculator_contribution_plot", app_text, fixed = TRUE))
  expect_false(grepl("first_plot_row <-", app_text, fixed = TRUE))
  expect_false(grepl('plotOutput\\("reference_profile_plot"', app_text))
  expect_false(grepl("renderPlot\\(\\{", app_text))

  expect_true(grepl('source\\("R/plots.R"\\)', app_text))
  expect_true(grepl("reference_profile_plot", app_text, fixed = TRUE))
  expect_true(grepl("reference_contribution_plot", app_text, fixed = TRUE))
  expect_true(grepl("reference_unadjusted_contribution_plot", app_text, fixed = TRUE))
  expect_true(grepl("plot_response_profile", app_text, fixed = TRUE))
  expect_true(grepl("plot_adjusted_contributions", app_text, fixed = TRUE))
  expect_true(grepl("plot_unadjusted_contributions", app_text, fixed = TRUE))
  expect_true(grepl('plotly::plotlyOutput\\(\\s*"reference_profile_plot",\\s*height = 520', app_text))
  expect_true(grepl('plotly::plotlyOutput\\(\\s*"reference_contribution_plot",\\s*height = 500', app_text))
  expect_true(grepl('plotly::plotlyOutput\\(\\s*"reference_unadjusted_contribution_plot",\\s*height = 500', app_text))
  expect_true(grepl("output\\$reference_profile_plot <- plotly::renderPlotly", app_text))
  expect_true(grepl("output\\$reference_contribution_plot <- plotly::renderPlotly", app_text))
  expect_true(grepl("output\\$reference_unadjusted_contribution_plot <- plotly::renderPlotly", app_text))
  expect_true(grepl('selectInput\\(\\s*"figure_export_format"', app_text))
  expect_true(grepl('downloadButton\\("download_profile_figures", "Download figures"\\)', app_text))
  expect_true(grepl("output\\$download_profile_figures <- downloadHandler", app_text))
  expect_true(grepl("export_profile_figures_pdf\\(row, file\\)", app_text))
  expect_true(grepl("export_profile_figure_files\\(row, export_dir, format\\)", app_text))
})

test_that("calculator results detail uses compact display table", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("Results table", app_text, fixed = TRUE))
  expect_true(grepl('tags\\$summary\\("Results"\\)', app_text))
  expect_true(grepl('downloadButton\\("download_results_csv", "Download CSV"\\)', app_text))
  expect_true(grepl("output\\$download_results_csv <- downloadHandler", app_text))
  expect_true(grepl("tags\\$details\\(\\s*class = \"detail-panel\",\\s*open = TRUE", app_text))
  expect_true(grepl("format_score_results_for_display\\(result\\$scores\\)", app_text))
  expect_true(grepl("DT::datatable\\(\\s*format_score_results_for_display", app_text))
})

test_that("preflight warnings hide after successful calculator scoring", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("has_successful_result <- reactive", app_text, fixed = TRUE))
  expect_true(grepl("!is\\.null\\(result\\) && isTRUE\\(result\\$ok\\) && !isTRUE\\(result\\$needs_offset\\)", app_text))
  expect_true(grepl("output\\$preflight_messages <- renderUI\\(\\{\\s*if \\(isTRUE\\(has_successful_result\\(\\)\\)\\)", app_text))
  expect_true(grepl("output\\$preflight_panel <- renderUI\\(\\{\\s*if \\(isTRUE\\(has_successful_result\\(\\)\\)\\)", app_text))
})

test_that("multi-subject summary cards distinguish positive cutoff categories", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("score_summary_counts\\(scores\\)", app_text))
  expect_true(grepl("Any cutoff positive", app_text, fixed = TRUE))
  expect_true(grepl("Both cutoffs positive", app_text, fixed = TRUE))
  expect_true(grepl("IAH classification", app_text, fixed = TRUE))
  expect_true(grepl("Likely IAH", app_text, fixed = TRUE))
  expect_true(grepl("Exactly one score crosses cutoff", app_text, fixed = TRUE))
  expect_false(grepl("Discordant", app_text, fixed = TRUE))
  expect_false(grepl("0.001", app_text, fixed = TRUE))
})

test_that("calculator results section is hidden until successful scoring", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")
  ui_text <- strsplit(app_text, "server <-", fixed = TRUE)[[1]][[1]]

  expect_true(grepl('uiOutput\\("results_section"\\)', app_text))
  expect_true(grepl("output\\$results_section <- renderUI", app_text))
  expect_true(grepl("req\\(result, result\\$ok\\)", app_text))
  expect_false(grepl('uiOutput\\("result_cards"\\)', ui_text))
})

test_that("upload workflow exposes subject ID selector and wording", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl('uiOutput\\("subject_id_selector"\\)', app_text))
  expect_true(grepl('selectInput\\(\\s*"subject_id_column"', app_text))
  expect_true(grepl('"Subject ID"', app_text, fixed = TRUE))
  expect_true(grepl("Subjects scored", app_text, fixed = TRUE))
  expect_true(grepl("Uploaded subject", app_text, fixed = TRUE))
  expect_true(grepl("subject response profiles", app_text, fixed = TRUE))

  expect_false(grepl("Participants scored", app_text, fixed = TRUE))
  expect_false(grepl("Participant ID", app_text, fixed = TRUE))
  expect_false(grepl("Uploaded participant", app_text, fixed = TRUE))
  expect_false(grepl("participant response profiles", app_text, fixed = TRUE))
})

test_that("changing subject ID selector clears stale upload results", {
  app_path <- file.path(PROJECT_ROOT, "app.R")
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("observeEvent\\(\\s*input\\$subject_id_column", app_text))
  expect_true(grepl("last_result\\(NULL\\)", app_text))
  expect_true(grepl("uploaded_state\\(NULL\\)", app_text))
  expect_true(grepl("apply_subject_id_selection_to_preflight", app_text, fixed = TRUE))
})

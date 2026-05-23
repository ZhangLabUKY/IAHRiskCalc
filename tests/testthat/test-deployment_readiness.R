skip_if_no_source_checkout()

required_deployment_packages <- c(
  "shiny",
  "plotly",
  "ggplot2",
  "htmlwidgets",
  "DT",
  "data.table",
  "readxl",
  "zip",
  "svglite",
  "ragg",
  "testthat"
)

test_that("project has package-like deployment metadata", {
  expect_true(file.exists(file.path(PROJECT_ROOT, "app.R")))
  expect_true(dir.exists(file.path(PROJECT_ROOT, "R")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "www", "styles.css")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "DESCRIPTION")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "README.md")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "NEWS.md")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "manifest.json")))
  expect_true(dir.exists(reference_data_file()))
  expect_true(file.exists(reference_data_file("ClampData_clean_wide.csv")))
  expect_true(file.exists(reference_data_file("ClampData.xlsx")))

  description <- read.dcf(file.path(PROJECT_ROOT, "DESCRIPTION"))[1, ]
  expect_equal(description[["Package"]], "IAHRiskCalc")
  expect_equal(description[["Version"]], "0.0.1")
  expect_match(description[["Title"]], "IAH Clamp-Based Risk Calculator", fixed = TRUE)
  expect_match(description[["Description"]], "Shiny application", fixed = TRUE)
  expect_match(description[["URL"]], "https://zhanglabuky.github.io/IAHRiskCalc/", fixed = TRUE)
  expect_match(description[["URL"]], "https://github.com/ZhangLabUKY/IAHRiskCalc", fixed = TRUE)
  expect_equal(description[["BugReports"]], "https://github.com/ZhangLabUKY/IAHRiskCalc/issues")

  news_text <- paste(readLines(project_file("NEWS.md"), warn = FALSE), collapse = "\n")
  expect_match(news_text, "# IAHRiskCalc 0.0.1", fixed = TRUE)

  pkgdown_text <- paste(readLines(project_file("_pkgdown.yml"), warn = FALSE), collapse = "\n")
  expect_match(pkgdown_text, "url: https://zhanglabuky.github.io/IAHRiskCalc/", fixed = TRUE)
})

test_that("declared deployment packages are available", {
  description <- read.dcf(file.path(PROJECT_ROOT, "DESCRIPTION"))[1, ]
  declared_packages <- paste(
    description[["Imports"]],
    description[["Suggests"]],
    collapse = "\n"
  )

  for (pkg in required_deployment_packages) {
    expect_match(declared_packages, paste0("\\b", pkg, "\\b"))
    expect_true(requireNamespace(pkg, quietly = TRUE), info = pkg)
  }
})

test_that("manifest exists and records core runtime dependencies", {
  manifest_path <- file.path(PROJECT_ROOT, "manifest.json")
  manifest_text <- paste(readLines(manifest_path, warn = FALSE), collapse = "\n")

  expect_match(manifest_text, '"appmode"\\s*:\\s*"shiny"')
  for (pkg in setdiff(required_deployment_packages, "testthat")) {
    expect_match(manifest_text, paste0('"Package"\\s*:\\s*"', pkg, '"'))
  }
})

test_that("representative sample uploads parse, transform, and score", {
  samples <- list(
    csv = reference_data_file("ClampData_clean_wide.csv"),
    xlsx = reference_data_file("ClampData.xlsx")
  )

  for (sample_path in samples) {
    normalized <- normalize_uploaded_clamp(sample_path, original_name = basename(sample_path))
    expect_true(all(required_score_cols() %in% names(normalized$data)))
    expect_gt(nrow(normalized$data), 0)

    transformed <- transform_physiological_responses(
      normalized$data,
      allow_offset = TRUE
    )
    expect_true(transformed$ok)

    scores <- apply_transform_metadata(
      calc_clamp_scores(transformed$data),
      transformed
    )
    expect_equal(nrow(scores), nrow(normalized$data))
    expect_true(all(scores$overall_group %in% c(
      "IAH",
      "Likely IAH",
      "NAH",
      "Unable to calculate; missing required values"
    )))
  }
})

test_that("static figure exports work for deployment devices", {
  normalized <- normalize_uploaded_clamp(
    reference_data_file("ClampData_clean_wide.csv"),
    original_name = "ClampData_clean_wide.csv"
  )
  transformed <- transform_physiological_responses(
    normalized$data,
    allow_offset = TRUE
  )
  row <- transformed$data[1, , drop = FALSE]
  out_dir <- file.path(tempdir(), paste0("deployment_export_", as.integer(stats::runif(1, 1, 1e6))))
  dir.create(out_dir)

  paths <- export_profile_figure_files(row, out_dir, "png")
  expect_equal(length(paths), 3)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size > 0))

  pdf_path <- file.path(out_dir, "profile_figures.pdf")
  export_profile_figures_pdf(row, pdf_path)
  expect_true(file.exists(pdf_path))
  expect_gt(file.info(pdf_path)$size, 0)
})

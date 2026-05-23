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
  "testthat",
  "writexl"
)

read_package_description <- function() {
  if (has_source_checkout()) {
    return(read.dcf(file.path(PROJECT_ROOT, "DESCRIPTION"))[1, ])
  }
  as.list(utils::packageDescription("IAHRiskCalc"))
}

test_that("source checkout has package-like deployment metadata", {
  skip_if_no_source_checkout()
  expect_true(file.exists(file.path(PROJECT_ROOT, "app.R")))
  expect_true(dir.exists(file.path(PROJECT_ROOT, "R")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "www", "styles.css")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "DESCRIPTION")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "README.md")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "NEWS.md")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "_pkgdown.yml")))

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

test_that("app and test sources avoid machine-specific paths", {
  skip_if_no_source_checkout()
  candidate_files <- c(
    project_file("app.R"),
    project_file("DESCRIPTION"),
    project_file("NEWS.md"),
    project_file("README.md"),
    project_file("README.Rmd"),
    project_file("_pkgdown.yml"),
    list.files(project_file("R"), pattern = "\\.[Rr]$", full.names = TRUE),
    list.files(project_file("tests"), pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    list.files(project_file(".github"), pattern = "\\.ya?ml$", recursive = TRUE, full.names = TRUE)
  )
  candidate_files <- candidate_files[file.exists(candidate_files)]
  text <- paste(vapply(candidate_files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  forbidden <- c(
    paste0("C", ":/"),
    paste0("C", ":", "\\"),
    paste0("D", ":/a"),
    paste0("Users", "/"),
    paste0("Users", "\\"),
    paste0("One", "Drive"),
    paste0("App", "Data"),
    paste0("ssa", "390"),
    paste0("R-", "4.5.3"),
    paste0("Rscript", ".exe")
  )

  for (fragment in forbidden) {
    expect_false(grepl(fragment, text, fixed = TRUE), info = fragment)
  }
})

test_that("declared deployment packages are available", {
  description <- read_package_description()
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

test_that("synthetic sample uploads parse, transform, and score", {
  samples <- list(
    csv = write_wide_csv_fixture(),
    xlsx = write_raw_grouped_xlsx_fixture()
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
    write_wide_csv_fixture(),
    original_name = "synthetic_wide.csv"
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

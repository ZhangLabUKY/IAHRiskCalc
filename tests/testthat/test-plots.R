test_that("plot helpers expose readable response profile outputs", {
  expect_true(is.function(plot_response_profile))
  expect_true(is.function(plot_adjusted_contributions))
  expect_true(is.function(plot_unadjusted_contributions))
  expect_equal(clamp_variable_label("FreeFattyAcids"), "Free fatty acids")
  expect_equal(clamp_variable_label("PancreaticP"), "Pancreatic Polypeptide")
})

test_that("plot helpers return Plotly widgets", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  df$Heart_45 <- 6
  df$Heart_90 <- 2

  expect_s3_class(plot_response_profile(df, vars = "Heart"), "plotly")
  expect_s3_class(plot_adjusted_contributions(df, vars = "Heart"), "plotly")
  expect_s3_class(plot_unadjusted_contributions(df, vars = "Heart"), "plotly")
})

test_that("static plot export helpers create requested files", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  df$Heart_45 <- 6
  df$Heart_90 <- 2
  out_dir <- file.path(tempdir(), paste0("plot_export_test_", as.integer(stats::runif(1, 1, 1e6))))
  dir.create(out_dir)

  paths <- export_profile_figure_files(df, out_dir, "png", vars = "Heart")
  expect_equal(length(paths), 3)
  expect_true(all(file.exists(paths)))
  expect_true(all(tools::file_ext(paths) == "png"))

  pdf_path <- file.path(out_dir, "figures.pdf")
  export_profile_figures_pdf(df, pdf_path, vars = "Heart")
  expect_true(file.exists(pdf_path))
  expect_gt(file.info(pdf_path)$size, 0)
})

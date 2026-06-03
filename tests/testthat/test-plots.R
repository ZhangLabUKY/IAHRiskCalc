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

test_that("interactive plot axes use dark bold titles and readable ticks", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  df$Heart_45 <- 6
  df$Heart_90 <- 2

  profile <- plot_response_profile(df, vars = "Heart")
  adjusted <- plot_adjusted_contributions(df, vars = "Heart")
  unadjusted <- plot_unadjusted_contributions(df, vars = "Heart")
  profile_layout <- profile$x$layoutAttrs[[1]]
  adjusted_layout <- adjusted$x$layoutAttrs[[1]]
  unadjusted_layout <- unadjusted$x$layoutAttrs[[1]]

  expect_match(profile_layout$xaxis$title$text, "<b>", fixed = TRUE)
  expect_equal(profile_layout$xaxis$title$font$color, "#111827")
  expect_equal(profile_layout$yaxis$title$font$color, "#111827")
  expect_equal(adjusted_layout$xaxis$title$font$color, "#111827")
  expect_equal(adjusted_layout$yaxis$tickfont$color, "#111827")
  expect_equal(unadjusted_layout$xaxis$title$font$color, "#111827")
  expect_equal(unadjusted_layout$yaxis$tickfont$color, "#111827")
})

test_that("static plot theme uses dark bold axis styling", {
  theme <- static_plot_text_theme()

  expect_equal(theme$axis.title.x$face, "bold")
  expect_equal(theme$axis.title.y$face, "bold")
  expect_equal(theme$axis.title.x$colour %||% theme$axis.title.x$color, "#111827")
  expect_equal(theme$axis.title.y$colour %||% theme$axis.title.y$color, "#111827")
  expect_equal(theme$axis.text$colour %||% theme$axis.text$color, "#111827")
})

test_that("static plot export helpers create requested files", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  df$Heart_45 <- 6
  df$Heart_90 <- 2
  out_dir <- file.path(tempdir(), paste0("plot_export_test_", as.integer(stats::runif(1, 1, 1e6))))
  dir.create(out_dir)

  paths <- export_profile_figure_files(df, out_dir, "png", vars = "Heart")
  expect_equal(length(paths), 2)
  expect_true(all(file.exists(paths)))
  expect_true(all(tools::file_ext(paths) == "png"))

  pdf_path <- file.path(out_dir, "figures.pdf")
  export_profile_figures_pdf(df, pdf_path, vars = "Heart")
  expect_true(file.exists(pdf_path))
  expect_gt(file.info(pdf_path)$size, 0)
})

test_that("static exports include response profile only for adjusted scoring", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  out_dir <- file.path(
    tempdir(),
    paste0("plot_method_export_test_", as.integer(stats::runif(1, 1, 1e6)))
  )
  dir.create(out_dir)

  adjusted <- export_profile_figure_files(
    df,
    out_dir,
    "png",
    vars = "Heart",
    method = "adjusted_45_vs_90"
  )
  unadjusted <- export_profile_figure_files(
    df,
    out_dir,
    "png",
    vars = "Heart",
    method = "unadjusted_45"
  )

  expect_equal(length(adjusted), 2)
  expect_true(any(grepl("clamp_response_profile", basename(adjusted), fixed = TRUE)))
  expect_true(any(grepl("clamp_response_contributions", basename(adjusted), fixed = TRUE)))
  expect_equal(length(unadjusted), 1)
  expect_false(any(grepl("clamp_response_profile", basename(unadjusted), fixed = TRUE)))
  expect_true(any(grepl("clamp_response_contributions", basename(unadjusted), fixed = TRUE)))
})

test_that("static contribution captions are wrapped and explain negative bars", {
  df <- as.data.frame(as.list(stats::setNames(rep(1, length(required_score_cols())), required_score_cols())),
                      check.names = FALSE)
  adjusted <- static_adjusted_contributions_plot(df, vars = "Heart")
  unadjusted <- static_unadjusted_contributions_plot(df, vars = "Heart")

  expect_match(adjusted$labels$caption, "\n", fixed = TRUE)
  expect_match(adjusted$labels$caption, "transformed 45 mg/dL response", fixed = TRUE)
  expect_match(unadjusted$labels$caption, "\n", fixed = TRUE)
  expect_match(unadjusted$labels$caption, "log2 transformation", fixed = TRUE)
})

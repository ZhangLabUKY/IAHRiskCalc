profile_long <- function(
  row,
  vars = CLAMP_VARIABLES,
  levels = CLAMP_GLUCOSE_LEVELS
) {
  expected_cols <- clamp_cols(vars, levels)
  available_cols <- intersect(expected_cols, names(row))

  if (length(available_cols) == 0) {
    return(data.frame(
      variable = character(0),
      glucose = numeric(0),
      value = numeric(0)
    ))
  }

  row <- coerce_clamp_numeric(row, available_cols)

  pieces <- strsplit(available_cols, "_")
  data.frame(
    variable = vapply(pieces, `[`, character(1), 1),
    glucose = as.numeric(vapply(pieces, `[`, character(1), 2)),
    value = as.numeric(row[1, available_cols, drop = TRUE]),
    check.names = FALSE
  )
}

empty_plotly_message <- function(message) {
  plotly::plot_ly() |>
    plotly::layout(
      font = list(size = 14),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = message,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 16)
      ))
    )
}

plot_response_profile <- function(row, vars = CLAMP_VARIABLES) {
  data <- profile_long(row, vars)

  if (nrow(data) == 0 || all(is.na(data$value))) {
    return(empty_plotly_message("No profile data available."))
  }

  data$label <- vapply(data$variable, clamp_variable_label, character(1))
  colors <- grDevices::hcl.colors(length(vars), palette = "Dark 3")
  names(colors) <- vars

  plot <- plotly::plot_ly()
  for (var in vars) {
    var_data <- data[data$variable == var & !is.na(data$value), ]
    if (nrow(var_data) == 0) {
      next
    }
    var_data <- var_data[order(var_data$glucose, decreasing = TRUE), ]
    label <- clamp_variable_label(var)

    plot <- plotly::add_trace(
      plot,
      data = var_data,
      x = ~glucose,
      y = ~value,
      type = "scatter",
      mode = "lines+markers",
      name = label,
      line = list(color = colors[[var]], width = 2),
      marker = list(color = colors[[var]], size = 7),
      hovertemplate = paste0(
        "<b>",
        label,
        "</b><br>",
        "Glucose level: %{x} mg/dL<br>",
        "Response value: %{y:.2f}",
        "<extra></extra>"
      )
    )
  }

  plotly::layout(
    plot,
    font = list(size = 14),
    title = list(
      text = "Clamp Response Profile by Glucose Level",
      x = 0.02,
      font = list(size = 18)
    ),
    xaxis = list(
      title = list(text = "Glucose level (mg/dL)", font = list(size = 15)),
      autorange = "reversed",
      tickmode = "array",
      tickvals = rev(CLAMP_GLUCOSE_LEVELS),
      zeroline = FALSE,
      tickfont = list(size = 13)
    ),
    yaxis = list(
      title = list(text = "Response value used for scoring", font = list(size = 15)),
      zeroline = TRUE,
      tickfont = list(size = 13)
    ),
    legend = list(
      orientation = "h",
      x = 0,
      y = -0.22,
      xanchor = "left",
      font = list(size = 12)
    ),
    margin = list(l = 70, r = 25, t = 55, b = 130),
    hovermode = "closest"
  ) |>
    plotly::config(displaylogo = FALSE)
}

plot_adjusted_contributions <- function(row, vars = CLAMP_VARIABLES) {
  contributions <- score_contributions(row, vars)

  if (
    nrow(contributions) == 0 || all(is.na(contributions$adjusted_contribution))
  ) {
    return(empty_plotly_message("No contribution data available."))
  }

  contributions$label <- vapply(
    contributions$variable,
    clamp_variable_label,
    character(1)
  )
  contributions <- contributions[order(contributions$adjusted_contribution), ]
  colors <- ifelse(
    contributions$adjusted_contribution >= 0,
    "#2b6cb0",
    "#b83232"
  )

  plotly::plot_ly(
    contributions,
    x = ~adjusted_contribution,
    y = ~label,
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "45 minus 90 response: %{x:.2f}<br>",
      "Red bars indicate variables where the transformed 45 mg/dL response is lower than the transformed 90 mg/dL response.",
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      font = list(size = 14),
      title = list(
        text = "Clamp Response Contributions",
        x = 0.02,
        font = list(size = 18)
      ),
      xaxis = list(
        title = list(text = "Response contribution", font = list(size = 15)),
        zeroline = TRUE,
        zerolinecolor = "#555555",
        tickfont = list(size = 13)
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = contributions$label,
        tickfont = list(size = 13)
      ),
      margin = list(l = 155, r = 25, t = 55, b = 70),
      showlegend = FALSE
    ) |>
    plotly::config(displaylogo = FALSE)
}

plot_unadjusted_contributions <- function(row, vars = CLAMP_VARIABLES) {
  contributions <- score_contributions(row, vars)

  if (nrow(contributions) == 0 || all(is.na(contributions$value_45))) {
    return(empty_plotly_message("No unadjusted contribution data available."))
  }

  contributions$label <- vapply(
    contributions$variable,
    clamp_variable_label,
    character(1)
  )
  contributions <- contributions[order(contributions$value_45), ]
  colors <- ifelse(contributions$value_45 >= 0, "#2b6cb0", "#b83232")

  plotly::plot_ly(
    contributions,
    x = ~value_45,
    y = ~label,
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "45 mg/dL response value: %{x:.2f}<br>",
      "Red bars indicate transformed values below zero; physiological raw values between 0 and 1 become negative after log2 transformation.",
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      font = list(size = 14),
      title = list(
        text = "Clamp Response Contributions",
        x = 0.02,
        font = list(size = 18)
      ),
      xaxis = list(
        title = list(text = "Response contribution", font = list(size = 15)),
        zeroline = TRUE,
        zerolinecolor = "#555555",
        tickfont = list(size = 13)
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = contributions$label,
        tickfont = list(size = 13)
      ),
      margin = list(l = 155, r = 25, t = 55, b = 70),
      showlegend = FALSE
    ) |>
    plotly::config(displaylogo = FALSE)
}

safe_filename_part <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (is.na(x) || !nzchar(x)) "subject" else x
}

STATIC_PLOT_BASE_SIZE <- 17

static_plot_text_theme <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = ggplot2::rel(1.2)
    ),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.05)),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.95)),
    legend.title = ggplot2::element_text(
      face = "bold",
      size = ggplot2::rel(1.0)
    ),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.9))
  )
}

static_contribution_text_theme <- function() {
  static_plot_text_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.92)),
      plot.margin = ggplot2::margin(12, 18, 24, 16)
    )
}

static_plot_caption <- function(text, width = 82) {
  paste(strwrap(text, width = width), collapse = "\n")
}

static_response_profile_plot <- function(row, vars = CLAMP_VARIABLES) {
  data <- profile_long(row, vars)
  data <- data[!is.na(data$value), , drop = FALSE]

  if (nrow(data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = "No profile data available."
        ) +
        ggplot2::theme_void()
    )
  }

  data$label <- vapply(data$variable, clamp_variable_label, character(1))
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = glucose, y = value, color = label, group = label)
  ) +
    ggplot2::geom_line(linewidth = 0.55, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.7, na.rm = TRUE) +
    ggplot2::scale_x_reverse(breaks = rev(CLAMP_GLUCOSE_LEVELS)) +
    ggplot2::labs(
      title = "Clamp Response Profile by Glucose Level",
      x = "Glucose level (mg/dL)",
      y = "Response value used for scoring",
      color = "Variable"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 4, byrow = TRUE)) +
    ggplot2::theme_minimal(base_size = STATIC_PLOT_BASE_SIZE) +
    static_plot_text_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

static_adjusted_contributions_plot <- function(row, vars = CLAMP_VARIABLES) {
  contributions <- score_contributions(row, vars)
  contributions <- contributions[
    !is.na(contributions$adjusted_contribution),
    ,
    drop = FALSE
  ]

  if (nrow(contributions) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = "No contribution data available."
        ) +
        ggplot2::theme_void()
    )
  }

  contributions$label <- vapply(
    contributions$variable,
    clamp_variable_label,
    character(1)
  )
  contributions$direction <- contributions$adjusted_contribution >= 0
  contributions$label <- stats::reorder(
    contributions$label,
    contributions$adjusted_contribution
  )

  ggplot2::ggplot(
    contributions,
    ggplot2::aes(x = adjusted_contribution, y = label, fill = direction)
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, color = "#555555", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#2b6cb0", "FALSE" = "#b83232")
    ) +
    ggplot2::labs(
      title = "Clamp Response Contributions",
      x = "Response contribution",
      y = NULL,
      caption = static_plot_caption(
        "Red bars indicate variables where the transformed 45 mg/dL response is lower than the transformed 90 mg/dL response."
      )
    ) +
    ggplot2::theme_minimal(base_size = STATIC_PLOT_BASE_SIZE) +
    static_contribution_text_theme() +
    ggplot2::theme(
      legend.position = "none",
      plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.78)),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

static_unadjusted_contributions_plot <- function(row, vars = CLAMP_VARIABLES) {
  contributions <- score_contributions(row, vars)
  contributions <- contributions[!is.na(contributions$value_45), , drop = FALSE]

  if (nrow(contributions) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = "No unadjusted contribution data available."
        ) +
        ggplot2::theme_void()
    )
  }

  contributions$label <- vapply(
    contributions$variable,
    clamp_variable_label,
    character(1)
  )
  contributions$direction <- contributions$value_45 >= 0
  contributions$label <- stats::reorder(
    contributions$label,
    contributions$value_45
  )

  ggplot2::ggplot(
    contributions,
    ggplot2::aes(x = value_45, y = label, fill = direction)
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = 0, color = "#555555", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#2b6cb0", "FALSE" = "#b83232")
    ) +
    ggplot2::labs(
      title = "Clamp Response Contributions",
      x = "Response contribution",
      y = NULL,
      caption = static_plot_caption(
        "Red bars indicate transformed values below zero; physiological raw values between 0 and 1 become negative after log2 transformation."
      )
    ) +
    ggplot2::theme_minimal(base_size = STATIC_PLOT_BASE_SIZE) +
    static_contribution_text_theme() +
    ggplot2::theme(
      legend.position = "none",
      plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.78)),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

static_empty_contribution_plot <- function(message = "No contribution data available.") {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 5
    ) +
    ggplot2::theme_void(base_size = STATIC_PLOT_BASE_SIZE)
}

static_score_contributions_plot <- function(
  row,
  method = "adjusted_45_vs_90",
  vars = CLAMP_VARIABLES
) {
  if (identical(method, "unadjusted_45")) {
    return(static_unadjusted_contributions_plot(row, vars))
  }
  if (identical(method, "adjusted_45_vs_90")) {
    return(static_adjusted_contributions_plot(row, vars))
  }
  static_empty_contribution_plot()
}

profile_static_plots <- function(
  row,
  vars = CLAMP_VARIABLES,
  method = "adjusted_45_vs_90"
) {
  plots <- list(
    clamp_response_contributions = static_score_contributions_plot(
      row,
      method,
      vars
    )
  )
  if (identical(method, "adjusted_45_vs_90")) {
    plots <- c(
      list(clamp_response_profile = static_response_profile_plot(row, vars)),
      plots
    )
  }
  plots
}

save_static_plot <- function(plot, path, format, width = 10, height = 7) {
  format <- tolower(format)
  if (format == "svg") {
    ggplot2::ggsave(
      path,
      plot = plot,
      device = svglite::svglite,
      width = width,
      height = height,
      units = "in"
    )
  } else if (format == "png") {
    ggplot2::ggsave(
      path,
      plot = plot,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "in",
      dpi = 300
    )
  } else if (format %in% c("jpg", "jpeg")) {
    ggplot2::ggsave(
      path,
      plot = plot,
      device = ragg::agg_jpeg,
      width = width,
      height = height,
      units = "in",
      dpi = 300
    )
  } else if (format %in% c("tif", "tiff")) {
    ggplot2::ggsave(
      path,
      plot = plot,
      device = ragg::agg_tiff,
      width = width,
      height = height,
      units = "in",
      dpi = 300
    )
  } else {
    stop("Unsupported figure format: ", format, call. = FALSE)
  }
}

export_profile_figure_files <- function(
  row,
  output_dir,
  format,
  vars = CLAMP_VARIABLES,
  method = "adjusted_45_vs_90"
) {
  format <- tolower(format)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  plots <- profile_static_plots(row, vars, method)
  extension <- if (format == "jpg") "jpeg" else format

  vapply(
    names(plots),
    function(name) {
      path <- file.path(output_dir, paste0(name, ".", extension))
      save_static_plot(plots[[name]], path, extension)
      path
    },
    character(1)
  )
}

export_profile_figures_pdf <- function(
  row,
  path,
  vars = CLAMP_VARIABLES,
  method = "adjusted_45_vs_90"
) {
  plots <- profile_static_plots(row, vars, method)
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)

  on.exit(grDevices::dev.off(), add = TRUE)
  for (plot in plots) {
    print(plot)
  }
  path
}

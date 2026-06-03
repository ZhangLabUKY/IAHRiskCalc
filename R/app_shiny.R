status_label <- function(value) {
  if (is.na(value)) {
    return("Unable to calculate")
  }
  if (isTRUE(value)) "Impaired awareness range" else "Normal awareness range"
}

manual_input_id <- function(var, level) {
  paste0("manual_", var, "_", level)
}

manual_entry_value <- function(values, var, level) {
  field <- paste0(var, "_", level)
  if (!is.null(values) && field %in% names(values)) {
    value <- values[[field]]
    if (!is.null(value) && length(value) > 0 && !is.na(value[[1]])) {
      return(value[[1]])
    }
  }
  NA_real_
}

manual_entry_group_ui <- function(title, vars, values = NULL) {
  tagList(
    h4(title),
    div(
      class = "entry-grid compact",
      lapply(vars, function(var) {
        div(
          class = "entry-row",
          tags$strong(class = "entry-name", clamp_variable_label(var)),
          div(
            class = "entry-inputs",
            numericInput(
              manual_input_id(var, 90),
              "90 mg/dL",
              value = manual_entry_value(values, var, 90),
              step = 0.1
            ),
            numericInput(
              manual_input_id(var, 45),
              "45 mg/dL",
              value = manual_entry_value(values, var, 45),
              step = 0.1
            )
          )
        )
      })
    )
  )
}

manual_entry_ui <- function(values = NULL) {
  tagList(
    div(
      class = "app-message warn",
      "Enter raw physiological values below. Physiological fields are log2-transformed before scoring."
    ),
    manual_entry_group_ui("Symptoms", SYMPTOM_VARIABLES, values),
    manual_entry_group_ui(
      "Physiological responses",
      COUNTERREGULATORY_VARIABLES,
      values
    )
  )
}

summary_card <- function(title, value, meta = NULL, class = "") {
  div(
    class = paste("score-card", class),
    h4(title),
    div(class = "score-value", value),
    if (!is.null(meta)) div(class = "score-meta", meta)
  )
}

score_summary_cards <- function(scores) {
  counts <- score_summary_counts(scores)

  div(
    class = "score-grid",
    summary_card("Subjects scored", counts$subjects_scored),
    summary_card(
      "IAH",
      counts$impaired_awareness,
      "Primary score below cutoff",
      "risk-summary"
    ),
    summary_card(
      "NAH",
      counts$normal_awareness,
      "Primary score meets or exceeds cutoff"
    ),
    summary_card(
      "Adjusted method",
      counts$adjusted_method,
      "Complete 45 and 90 mg/dL data"
    ),
    summary_card(
      "Unadjusted method",
      counts$unadjusted_method,
      "Complete 45 mg/dL data only"
    ),
    summary_card(
      "Unable to calculate",
      counts$unable_to_calculate,
      "Missing required values"
    )
  )
}

single_score_cards <- function(score) {
  score_value <- ifelse(
    is.na(score$primary_score),
    "Missing",
    round(score$primary_score, 2)
  )
  cutoff_value <- ifelse(
    is.na(score$primary_cutoff),
    "Unavailable",
    round(score$primary_cutoff, 2)
  )
  result_class <- ifelse(
    isTRUE(score$primary_impaired_awareness),
    "score-flag risk",
    "score-flag"
  )

  div(
    class = "score-grid",
    div(
      class = "score-card",
      h4("Patient Value"),
      div(class = "score-value", score_value),
      div(class = "score-meta", paste("Cutoff:", cutoff_value)),
      div(class = result_class, score$primary_cutoff_result)
    ),
    div(
      class = "score-card wide",
      h4("Overall Classification"),
      div(class = "classification", score$overall_group),
      div(
        class = "score-meta",
        ifelse(
          is.na(score$primary_score),
          "Complete 45 mg/dL data are required.",
          "Scores at or above cutoff classify as NAH."
        )
      )
    )
  )
}

offset_warning_ui <- function(transform_result) {
  fields <- transform_result$offset_fields
  div(
    class = "app-message warn",
    h4("Offset confirmation needed"),
    p(paste(
      nrow(fields),
      "physiological value(s) are zero or negative. Press Continue to set those values to 80% of the minimum positive raw value for their variable",
      "before log2 transformation."
    )),
    tableOutput("offset_fields_table"),
    actionButton(
      "confirm_offset",
      "Continue with offset",
      class = "btn-primary"
    )
  )
}


iah_app_ui <- function() {
  fluidPage(
  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "styles.css"
  )),
  titlePanel("IAH Clamp-Based Risk Calculator"),
  tabsetPanel(
    tabPanel(
      "Calculator",
      div(
        class = "workflow-grid",
        div(
          class = "workflow-panel",
          h3("Input"),
          radioButtons(
            "input_mode",
            NULL,
            choices = c("Manual entry" = "manual", "Upload file" = "upload"),
            selected = "manual",
            inline = TRUE
          ),
          conditionalPanel(
            "input.input_mode == 'upload'",
            fileInput(
              "calculator_file",
              "Upload clamp file",
              accept = c(".csv", ".xls", ".xlsx")
            ),
            uiOutput("subject_id_selector"),
            p(
              class = "small-note",
              "Upload a clamp file to review data quality before scoring."
            )
          ),
          conditionalPanel(
            "input.input_mode == 'manual'",
            textInput(
              "manual_participant_id",
              "Subject ID",
              value = "New subject"
            ),
            p(
              class = "small-note",
              "Manual entry scores one subject at a time."
            )
          ),
          uiOutput("missing_mode_ui"),
          actionButton("calculate", "Calculate risk", class = "btn-primary")
        ),
        div(
          class = "workflow-main",
          uiOutput("preflight_messages"),
          uiOutput("preflight_panel"),
          uiOutput("manual_entry_section"),
          uiOutput("calculator_messages"),
          uiOutput("offset_warning"),
          uiOutput("results_section")
        )
      )
    ),
    tabPanel(
      "Plots",
      div(
        class = "profile-shell",
        uiOutput("profile_empty_state"),
        conditionalPanel(
          "output.hasProfileData",
          selectInput(
            "profile_participant",
            "Subject",
            choices = character(0)
          ),
          div(
            class = "download-controls",
            selectInput(
              "figure_export_format",
              "Figure format",
              choices = c(
                "PDF" = "pdf",
                "TIFF" = "tiff",
                "SVG" = "svg",
                "PNG" = "png",
                "JPEG" = "jpeg"
              ),
              selected = "pdf"
            ),
            downloadButton("download_profile_figures", "Download figures")
          ),
          fluidRow(
            column(
              12,
              uiOutput("profile_plot_section")
            )
          ),
          fluidRow(
            column(
              12,
              uiOutput("contribution_plot_note"),
              plotly::plotlyOutput("reference_contribution_plot", height = 500)
            )
          )
        )
      )
    ),
    tabPanel(
      "Methods",
      div(
        class = "methods",
        h3("Purpose and Inputs"),
        p(
          "This app supports research workflows for calculating impaired awareness of hypoglycemia (IAH) risk from hyperinsulinemic clamp response data. Subjects can be scored from an uploaded clamp file or from manual entry."
        ),
        p(
          "The risk calculation uses required response values measured at 45 and 90 mg/dL. Uploaded data become the working dataset for the current app session and are also used for plots, downloads, and mean imputation when selected."
        ),
        h3("Automated Preprocessing"),
        tags$ul(
          tags$li(
            "Uploads can be parsed from canonical wide files or raw grouped workbook layouts."
          ),
          tags$li(
            "The Subject ID selector controls which uploaded column is used for subject labels in warnings, results, and plots."
          ),
          tags$li(
            "Symptom values are used as entered. Physiological variables are treated as raw values and log2-transformed before scoring."
          )
        ),
        h3("Log2 Offset Handling"),
        p(
          "Physiological raw values that are zero or negative cannot be log2-transformed directly. When these values are detected, the app asks for confirmation before scoring."
        ),
        p(
          "After confirmation, each affected uploaded value is set to 80% of the minimum positive raw value for that same physiological field in the current scoring dataset. For manual entry, the app uses 80% of the minimum positive value from the same physiological response's 45/90 pair. If no positive value is available from the applicable anchor, scoring stops with a clear transform error."
        ),
        h3("Missing Data"),
        p(
          "Complete 45 mg/dL values are required for scoring. Complete paired 90 mg/dL values allow the preferred adjusted score; if 90 mg/dL values are unavailable, the app uses the unadjusted 45 mg/dL score."
        ),
        tags$ul(
          tags$li(
            tags$strong("No Imputation: "),
            "missing required 45 mg/dL values remain missing and affected subject scores are reported as unable to calculate."
          ),
          tags$li(
            tags$strong("Mean imputation: "),
            "missing required 45 mg/dL values are filled with column means from the current uploaded dataset after physiological preprocessing."
          )
        ),
        h3("Risk Scores"),
        tags$ul(
          tags$li(
            tags$strong("Unadjusted 45 mg/dL response score: "),
            "the sum of all 20 response values measured at glucose 45 mg/dL."
          ),
          tags$li(
            tags$strong("Adjusted 45-vs-90 response score: "),
            "the sum of each variable's 45 mg/dL response minus its paired 90 mg/dL response."
          )
        ),
        h3("Cutoffs and Labels"),
        tags$ul(
          tags$li("Adjusted scoring is used when complete 45 and 90 mg/dL data are available; its cutoff is 25."),
          tags$li("Unadjusted scoring is used when complete 45 mg/dL data are available but 90 mg/dL data are unavailable; its cutoff is 66.5."),
          tags$li("A primary score greater than or equal to its cutoff is classified as NAH."),
          tags$li("A primary score below its cutoff is classified as IAH.")
        ),
        h3("Plots and Exports"),
        p(
          "The Plots tab shows interactive Plotly figures for the selected uploaded subject: the clamp response profile and the contribution plot matching the score method used for classification."
        ),
        p(
          "Calculator results can be downloaded as CSV. Figure downloads use static ggplot2 exports: PDF downloads as one combined multi-page file, while TIFF, SVG, PNG, and JPEG downloads are packaged as a zip containing the response profile and primary contribution panels."
        ),
        h3("Disclaimer"),
        p(
          "This tool supports research workflows using clamp-derived response data and study-specific cutoffs. It is not a standalone clinical diagnostic tool."
        )
      )
    )
  )
)

}

iah_app_server <- function(input, output, session) {
  uploaded_state <- reactiveVal(NULL)
  profile_state <- reactiveVal(NULL)
  upload_preflight_state <- reactiveVal(NULL)
  last_result <- reactiveVal(NULL)
  offset_confirmed <- reactiveVal(FALSE)
  pending_offset_payload <- reactiveVal(NULL)
  show_manual_entry <- reactiveVal(TRUE)
  manual_entry_cache <- reactiveVal(NULL)

  observeEvent(input$calculator_file, {
    offset_confirmed(FALSE)
    last_result(NULL)
    pending_offset_payload(NULL)
    uploaded_state(NULL)
    profile_state(NULL)
    if (!is.null(input$calculator_file)) {
      upload_preflight_state(preflight_upload(
        input$calculator_file$datapath,
        original_name = input$calculator_file$name
      ))
    } else {
      upload_preflight_state(NULL)
    }
  })

  observeEvent(input$input_mode, {
    offset_confirmed(FALSE)
    last_result(NULL)
    pending_offset_payload(NULL)
    profile_state(NULL)
    show_manual_entry(TRUE)
  })

  observeEvent(
    input$subject_id_column,
    {
      offset_confirmed(FALSE)
      last_result(NULL)
      pending_offset_payload(NULL)
      uploaded_state(NULL)
      profile_state(NULL)
    },
    ignoreInit = TRUE
  )

  manual_scalar_input <- function(id) {
    value <- input[[id]]
    if (is.null(value) || length(value) == 0) {
      return(NA_real_)
    }

    value <- suppressWarnings(as.numeric(value[[1]]))
    if (length(value) == 0 || is.na(value)) {
      NA_real_
    } else {
      value
    }
  }

  manual_subject_id <- function() {
    id <- input$manual_participant_id
    if (is.null(id) || length(id) == 0) {
      return("New subject")
    }

    id <- as.character(id[[1]])
    if (is.na(id) || !nzchar(trimws(id))) {
      "New subject"
    } else {
      id
    }
  }

  manual_df <- reactive({
    values <- list()
    for (var in CLAMP_VARIABLES) {
      for (level in c(90, 45)) {
        values[[paste0(var, "_", level)]] <- manual_scalar_input(
          manual_input_id(var, level)
        )
      }
    }
    df <- as.data.frame(values, check.names = FALSE)
    rownames(df) <- manual_subject_id()
    df
  })

  cache_manual_entry <- function() {
    values <- as.list(manual_df()[1, , drop = TRUE])
    manual_entry_cache(list(
      participant_id = manual_subject_id(),
      values = values
    ))
  }

  current_preflight <- reactive({
    if (identical(input$input_mode, "upload")) {
      return(apply_subject_id_selection_to_preflight(
        upload_preflight_state(),
        input$subject_id_column
      ))
    }
    preflight_manual(manual_df(), offset_method = "paired")
  })

  output$subject_id_selector <- renderUI({
    preflight <- upload_preflight_state()
    req(preflight, preflight$audit, preflight$audit$subject_id)
    selectInput(
      "subject_id_column",
      "Subject ID",
      choices = subject_id_choices(preflight$audit$subject_id),
      selected = preflight$audit$subject_id$default_key
    )
  })

  has_missing_for_mode <- reactive({
    preflight <- current_preflight()
    !is.null(preflight) && isTRUE(preflight$has_missing_required)
  })

  output$missing_mode_ui <- renderUI({
    preflight <- current_preflight()
    if (is.null(preflight) || !isTRUE(preflight$has_missing_required)) {
      return(tagList(
        h3("Missing Values"),
        p(class = "small-note", "No missing required 45 mg/dL values detected.")
      ))
    }

    tagList(
      h3("Missing Values"),
      radioButtons(
        "missing_mode",
        NULL,
        choices = c("No Imputation" = "strict", "Mean imputation" = "impute"),
        selected = "strict"
      )
    )
  })

  selected_missing_mode <- reactive({
    if (isTRUE(has_missing_for_mode())) {
      input$missing_mode %||% "strict"
    } else {
      "strict"
    }
  })

  score_dataset <- function(df, reference_df, audit = NULL, source = "upload") {
    offset_method <- if (identical(source, "manual")) "paired" else "column"
    transform_result <- transform_physiological_responses(
      df,
      allow_offset = offset_confirmed(),
      offset_method = offset_method
    )

    if (!transform_result$ok) {
      return(list(
        ok = FALSE,
        needs_offset = isTRUE(transform_result$needs_offset_confirmation),
        transform = transform_result,
        df = df,
        reference_df = reference_df,
        audit = audit,
        source = source,
        message = transform_result$message %||%
          transform_result$transform_warnings
      ))
    }

    validation <- validate_scoring_input(transform_result$data)
    if (!validation$column_check$ok || !validation$numeric_check$ok) {
      return(list(
        ok = FALSE,
        needs_offset = FALSE,
        validation = validation,
        transform = transform_result,
        audit = audit,
        source = source,
        message = paste(
          validation$column_check$message,
          validation$numeric_check$message,
          sep = " "
        )
      ))
    }

    scored_df <- transform_result$data
    if (selected_missing_mode() == "impute") {
      if (is.null(reference_df) && identical(source, "upload")) {
        reference_df <- transform_result$data
      }
      if (is.null(reference_df)) {
        return(list(
          ok = FALSE,
          needs_offset = FALSE,
          validation = validation,
          transform = transform_result,
          audit = audit,
          source = source,
          message = "Mean imputation for manual entry requires an uploaded dataset in the current session."
        ))
      }
      scored_df <- impute_missing_with_means(scored_df, reference_df)
    }

    scores <- calc_clamp_scores(scored_df)
    if (selected_missing_mode() == "impute") {
      scores <- apply_imputation_metadata(scores, scored_df)
    }
    scores <- apply_transform_metadata(scores, transform_result)

    if (!is.null(audit)) {
      scores$detected_format <- audit$detected_format
      scores$detected_sheet <- ifelse(
        is.na(audit$detected_sheet),
        "",
        audit$detected_sheet
      )
      scores$parser_warnings <- paste(audit$parser_warnings, collapse = " | ")
    }

    list(
      ok = TRUE,
      needs_offset = FALSE,
      scores = scores,
      df = scored_df,
      validation = validation,
      transform = transform_result,
      audit = audit,
      source = source
    )
  }

  build_calculator_payload <- function() {
    if (input$input_mode == "upload") {
      normalized <- current_preflight()
      req(normalized)
      return(list(
        df = normalized$data,
        reference_df = NULL,
        audit = normalized$audit,
        source = "upload"
      ))
    }

    reference_df <- NULL
    if (!is.null(uploaded_state())) {
      reference_df <- uploaded_state()$df
    }
    list(
      df = manual_df(),
      reference_df = reference_df,
      audit = NULL,
      source = "manual"
    )
  }

  run_payload <- function(payload) {
    result <- score_dataset(
      payload$df,
      reference_df = payload$reference_df,
      audit = payload$audit,
      source = payload$source
    )

    if (isTRUE(result$needs_offset)) {
      pending_offset_payload(payload)
    } else {
      pending_offset_payload(NULL)
    }

    if (isTRUE(result$ok) && identical(result$source, "upload")) {
      uploaded_state(list(
        df = result$df,
        scores = result$scores,
        audit = result$audit
      ))
    }
    if (isTRUE(result$ok)) {
      profile_state(list(
        df = result$df,
        scores = result$scores,
        source = result$source
      ))
      updateSelectInput(
        session,
        "profile_participant",
        choices = rownames(result$df),
        selected = rownames(result$df)[[1]]
      )
    }

    last_result(result)
    if (isTRUE(result$ok) && identical(result$source, "manual")) {
      cache_manual_entry()
      show_manual_entry(FALSE)
    }
    result
  }

  observeEvent(
    input$calculate,
    {
      offset_confirmed(FALSE)
      pending_offset_payload(NULL)
      run_payload(build_calculator_payload())
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$confirm_offset,
    {
      payload <- pending_offset_payload()
      req(payload)
      offset_confirmed(TRUE)
      run_payload(payload)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$edit_manual_entry,
    {
      show_manual_entry(TRUE)
    },
    ignoreInit = TRUE
  )

  current_result <- reactive({
    last_result()
  })

  has_successful_result <- reactive({
    result <- current_result()
    !is.null(result) && isTRUE(result$ok) && !isTRUE(result$needs_offset)
  })

  output$preflight_messages <- renderUI({
    if (isTRUE(has_successful_result())) {
      return(NULL)
    }
    if (identical(input$input_mode, "manual")) {
      return(NULL)
    }

    preflight <- current_preflight()
    if (is.null(preflight)) {
      return(NULL)
    }
    if (isTRUE(preflight$has_offset_warnings)) {
      return(div(
        class = "app-message warn slim",
        "File parsed successfully. Review log2 transformation warnings before calculating risk."
      ))
    }
    if (isTRUE(preflight$has_missing_required)) {
      return(div(
        class = "app-message warn slim",
        "File parsed successfully. Missing required values were detected."
      ))
    }
    div(
      class = "app-message ok slim",
      "File parsed successfully. No data warnings detected."
    )
  })

  output$preflight_panel <- renderUI({
    if (isTRUE(has_successful_result())) {
      return(NULL)
    }
    if (identical(input$input_mode, "manual")) {
      return(NULL)
    }

    preflight <- current_preflight()
    req(preflight)

    has_parser_warnings <- !is.null(preflight$audit) &&
      length(preflight$audit$parser_warnings) > 0 &&
      any(nzchar(preflight$audit$parser_warnings))
    has_missing <- isTRUE(preflight$has_missing_required)
    has_offsets <- isTRUE(preflight$has_offset_warnings)

    if (!has_parser_warnings && !has_missing && !has_offsets) {
      return(NULL)
    }

    tags$details(
      class = "detail-panel review-panel",
      open = TRUE,
      tags$summary("Upload and data warnings"),
      if (has_parser_warnings) {
        tagList(
          h4("Upload Audit"),
          tableOutput("preflight_upload_audit_table")
        )
      },
      if (has_missing) {
        tagList(
          h4("Missing Required Values"),
          tableOutput("preflight_missing_table")
        )
      },
      if (has_offsets) {
        tagList(
          h4("Log2 Transformation Warnings"),
          tableOutput("preflight_offset_table")
        )
      }
    )
  })

  output$preflight_status_table <- renderTable(
    {
      preflight_status_frame(current_preflight())
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$preflight_upload_audit_table <- renderTable(
    {
      preflight <- current_preflight()
      req(preflight)
      if (is.null(preflight$audit)) {
        return(data.frame(
          message = "Manual entry does not have upload audit metadata."
        ))
      }
      upload_audit_frame(preflight$audit)
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$preflight_missing_table <- renderTable(
    {
      preflight <- current_preflight()
      req(preflight)
      missing_rows <- preflight$missing_values
      missing_rows <- missing_rows[
        missing_rows$missing_value_count > 0,
        ,
        drop = FALSE
      ]
      format_missing_values_for_display(missing_rows)
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$preflight_offset_table <- renderTable(
    {
      preflight <- current_preflight()
      req(preflight)
      format_offset_fields_for_display(preflight$offset_fields)
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$manual_entry_section <- renderUI({
    if (
      !identical(input$input_mode, "manual") || !isTRUE(show_manual_entry())
    ) {
      return(NULL)
    }

    tagList(
      h3("Manual Entry"),
      manual_entry_ui(manual_entry_cache()$values)
    )
  })

  output$calculator_messages <- renderUI({
    result <- current_result()
    req(result)

    if (isTRUE(result$needs_offset)) {
      return(NULL)
    }
    if (!isTRUE(result$ok)) {
      return(div(
        class = "app-message error",
        result$message %||% "Unable to calculate scores."
      ))
    }
    if (any(result$scores$imputation_used)) {
      return(div(
        class = "app-message warn slim",
        "Scores calculated with mean imputation."
      ))
    }
    if (any(result$scores$phys_offset_applied)) {
      return(div(
        class = "app-message warn slim",
        "Scores calculated. One or more physiological values used the 80% minimum-positive-value offset before log2 transformation."
      ))
    }
    div(class = "app-message ok slim", "Scores calculated successfully.")
  })

  output$results_section <- renderUI({
    result <- current_result()
    req(result, result$ok)
    tagList(
      div(
        class = "results-header",
        h3("Results"),
        if (identical(result$source, "manual") && nrow(result$scores) == 1) {
          actionButton(
            "edit_manual_entry",
            "Edit entered data",
            class = "btn-default"
          )
        }
      ),
      uiOutput("result_cards"),
      if (nrow(result$scores) > 1) {
        tags$details(
          class = "detail-panel",
          open = TRUE,
          tags$summary("Results"),
          div(
            class = "download-row",
            downloadButton("download_results_csv", "Download CSV")
          ),
          uiOutput("results_table_ui")
        )
      }
    )
  })

  output$offset_warning <- renderUI({
    result <- current_result()
    req(result)
    if (isTRUE(result$needs_offset)) {
      offset_warning_ui(result$transform)
    }
  })

  output$offset_fields_table <- renderTable(
    {
      result <- current_result()
      req(result, result$needs_offset)
      format_offset_fields_for_display(result$transform$offset_fields)
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$result_cards <- renderUI({
    result <- current_result()
    req(result, result$ok)
    if (nrow(result$scores) == 1) {
      single_score_cards(result$scores[1, ])
    } else {
      score_summary_cards(result$scores)
    }
  })

  output$results_table_ui <- renderUI({
    result <- current_result()
    req(result, result$ok, nrow(result$scores) > 1)
    if (requireNamespace("DT", quietly = TRUE)) {
      DT::DTOutput("results_table")
    } else {
      tableOutput("results_table_base")
    }
  })

  if (requireNamespace("DT", quietly = TRUE)) {
    output$results_table <- DT::renderDT({
      result <- current_result()
      req(result, result$ok, nrow(result$scores) > 1)
      DT::datatable(
        format_score_results_for_display(result$scores),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  } else {
    output$results_table_base <- renderTable(
      {
        result <- current_result()
        req(result, result$ok, nrow(result$scores) > 1)
        format_score_results_for_display(result$scores)
      },
      striped = TRUE,
      bordered = TRUE
    )
  }

  output$download_results_csv <- downloadHandler(
    filename = function() {
      result <- current_result()
      req(result, result$ok, nrow(result$scores) > 1)
      ids <- safe_filename_part(paste(
        result$scores$participant_id,
        collapse = "_"
      ))
      paste0("iah_risk_results_", ids, ".csv")
    },
    content = function(file) {
      result <- current_result()
      req(result, result$ok, nrow(result$scores) > 1)
      write.csv(
        format_score_results_for_display(result$scores),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )

  output$hasProfileData <- reactive({
    !is.null(profile_state())
  })
  outputOptions(output, "hasProfileData", suspendWhenHidden = FALSE)

  output$profile_empty_state <- renderUI({
    if (is.null(profile_state())) {
      div(
        class = "app-message warn",
        "Upload or manually enter data, then calculate risk in the Calculator tab to view plots. Response profiles appear when adjusted 45/90 data are available."
      )
    }
  })

  selected_profile_score <- reactive({
    state <- profile_state()
    req(state, input$profile_participant)
    score <- state$scores[
      state$scores$participant_id == input$profile_participant,
      ,
      drop = FALSE
    ]
    if (nrow(score) == 0) {
      return(NULL)
    }
    score[1, , drop = FALSE]
  })

  output$profile_plot_section <- renderUI({
    score <- selected_profile_score()
    req(score)
    if (!identical(score$score_method[[1]], "adjusted_45_vs_90")) {
      return(div(
        class = "app-message warn slim",
        "Response profile is shown only when adjusted 45/90 data are available. This subject was scored with unadjusted 45 mg/dL data, so only the contribution plot is shown."
      ))
    }
    plotly::plotlyOutput("reference_profile_plot", height = 520)
  })

  output$contribution_plot_note <- renderUI({
    score <- selected_profile_score()
    req(score)
    if (identical(score$score_method[[1]], "adjusted_45_vs_90")) {
      div(
        class = "small-note plot-note",
        "Red bars show variables where the transformed 45 mg/dL response is lower than the transformed 90 mg/dL response, reducing the adjusted total."
      )
    } else {
      div(
        class = "small-note plot-note",
        "Red bars show transformed contributions below zero. For physiological values, this can occur after log2 transformation when raw values are between 0 and 1; it does not mean the raw input was invalid."
      )
    }
  })

  output$reference_profile_plot <- plotly::renderPlotly({
    state <- profile_state()
    req(state, input$profile_participant)
    score <- selected_profile_score()
    req(score, identical(score$score_method[[1]], "adjusted_45_vs_90"))
    row <- state$df[input$profile_participant, , drop = FALSE]
    plot_response_profile(row)
  })

  output$reference_contribution_plot <- plotly::renderPlotly({
    state <- profile_state()
    req(state, input$profile_participant)
    row <- state$df[input$profile_participant, , drop = FALSE]
    score <- selected_profile_score()
    method <- if (!is.null(score)) score$score_method[[1]] else "adjusted_45_vs_90"
    if (identical(method, "unadjusted_45")) {
      plot_unadjusted_contributions(row)
    } else {
      plot_adjusted_contributions(row)
    }
  })

  output$download_profile_figures <- downloadHandler(
    filename = function() {
      format <- input$figure_export_format %||% "pdf"
      subject <- safe_filename_part(input$profile_participant %||% "subject")
      if (identical(format, "pdf")) {
        paste0("response_profile_", subject, ".pdf")
      } else {
        paste0("response_profile_", subject, "_", format, ".zip")
      }
    },
    content = function(file) {
      state <- profile_state()
      req(state, input$profile_participant)
      format <- input$figure_export_format %||% "pdf"
      row <- state$df[input$profile_participant, , drop = FALSE]
      score <- selected_profile_score()
      method <- if (!is.null(score)) score$score_method[[1]] else "adjusted_45_vs_90"

      if (identical(format, "pdf")) {
        export_profile_figures_pdf(row, file, method = method)
        return(invisible(NULL))
      }

      export_dir <- file.path(
        tempdir(),
        paste0(
          "profile_figures_",
          safe_filename_part(input$profile_participant)
        )
      )
      if (dir.exists(export_dir)) {
        unlink(export_dir, recursive = TRUE)
      }
      dir.create(export_dir, recursive = TRUE)
      paths <- export_profile_figure_files(row, export_dir, format, method = method)
      zip::zipr(zipfile = file, files = basename(paths), root = export_dir)
    }
  )
}


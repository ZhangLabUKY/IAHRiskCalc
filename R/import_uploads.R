normalize_upload_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

RESPONSE_LABEL_ALIASES <- list(
  Heart = c("heart", "heart pounding", "heartpounding", "heart pound", "heart pounds"),
  Shaky = c("shaky", "shaky tremulous", "shaky/tremulous", "tremulous"),
  Sweaty = c("sweaty", "sweating", "sweat"),
  Hungry = c("hungry", "hunger"),
  Tingling = c("tingling", "tingle"),
  Confused = c("confused", "confusion"),
  Tired = c("tired", "tired drowsy", "tired/drowsy", "drowsy", "fatigue", "fatigued"),
  Weak = c("weak", "weakness"),
  Warm = c("warm", "warmth"),
  Faint = c("faint", "faintness"),
  Dizzy = c("dizzy", "dizziness"),
  Cortisol = c("cortisol"),
  Glucagon = c("glucagon"),
  Dopamine = c("dopamine"),
  Epinephrine = c("epinephrine", "epi"),
  Norepinephrine = c("norepinephrine", "norepi", "nor epinephrine"),
  FreeFattyAcids = c("free fatty acids", "freefattyacids", "ffa", "free fatty acid"),
  HGH = c("hgh", "growth hormone", "human growth hormone"),
  PancreaticP = c("pancreaticp", "pancreatic p", "pancreatic polypeptide", "pp"),
  Insulin = c("insulin")
)

response_alias_lookup <- local({
  aliases <- character(0)
  for (var in names(RESPONSE_LABEL_ALIASES)) {
    keys <- vapply(RESPONSE_LABEL_ALIASES[[var]], normalize_upload_text, character(1))
    aliases[keys] <- var
  }
  aliases
})

response_alias_patterns <- local({
  lapply(names(RESPONSE_LABEL_ALIASES), function(var) {
    aliases <- vapply(RESPONSE_LABEL_ALIASES[[var]], normalize_upload_text, character(1))
    alias_compact <- gsub(" ", "", aliases)
    list(
      variable = var,
      pattern = paste0("(^| )(", paste(aliases, collapse = "|"), ")( |$)"),
      compact_pattern = paste0("(^|_)(", paste(alias_compact, collapse = "|"), ")(_|$)")
    )
  })
})

canonical_response_name <- function(x) {
  key <- normalize_upload_text(x)
  if (is.na(key) || key == "") {
    return(NA_character_)
  }
  if (key %in% names(response_alias_lookup)) {
    return(unname(response_alias_lookup[[key]]))
  }
  compact_key <- gsub(" ", "", key)
  if (compact_key %in% names(response_alias_lookup)) {
    return(unname(response_alias_lookup[[compact_key]]))
  }
  NA_character_
}

canonical_response_from_key <- function(key, compact_key = gsub(" ", "", key)) {
  if (is.na(key) || key == "") {
    return(NA_character_)
  }

  matches <- character(0)

  for (pattern in response_alias_patterns) {
    if (grepl(pattern$pattern, key) || grepl(pattern$compact_pattern, compact_key)) {
      matches <- c(matches, pattern$variable)
    }
  }

  matches <- unique(matches)
  if (length(matches) == 1) {
    return(matches)
  }
  NA_character_
}

canonical_response_from_text <- function(x) {
  key <- normalize_upload_text(x)
  canonical_response_from_key(key, gsub(" ", "", key))
}

canonical_glucose_level <- function(x) {
  key <- normalize_upload_text(x)
  if (is.na(key) || key == "") {
    return(NA_character_)
  }
  if (grepl("baseline", key)) {
    return("baseline")
  }
  match <- regmatches(key, regexpr("\\d+", key))
  if (length(match) == 0 || match == "") {
    return(NA_character_)
  }
  if (match %in% as.character(CLAMP_GLUCOSE_LEVELS)) {
    return(match)
  }
  NA_character_
}

canonical_glucose_from_key <- function(key) {
  if (is.na(key) || key == "" || grepl("baseline", key)) {
    return(NA_character_)
  }

  numbers <- regmatches(key, gregexpr("\\d+", key))[[1]]
  numbers <- intersect(numbers, as.character(CLAMP_GLUCOSE_LEVELS))
  numbers <- setdiff(numbers, c("111", "65", "55"))

  if (length(numbers) == 1) {
    return(numbers)
  }
  NA_character_
}

canonical_glucose_from_text <- function(x) {
  canonical_glucose_from_key(normalize_upload_text(x))
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[trimws(x) == ""] <- NA_character_
  x
}

generated_subject_ids <- function(n) {
  paste0("Subject_", seq_len(n))
}

is_subject_id_like_name <- function(x) {
  key <- normalize_upload_text(x)
  compact_key <- gsub(" ", "", key)
  key %in% c(
    "subject", "subject id", "subj",
    "participant", "participant id",
    "patient", "patient id",
    "id", "record id"
  ) || compact_key %in% c(
    "subject", "subjectid", "subj",
    "participant", "participantid",
    "patient", "patientid",
    "id", "recordid"
  )
}

subject_id_values <- function(values) {
  values <- blank_to_na(values)
  fallback <- generated_subject_ids(length(values))
  make.unique(ifelse(is.na(values), fallback, as.character(values)))
}

subject_id_metadata <- function(
  row_count,
  labels = character(0),
  values = list(),
  prefer_first_candidate = FALSE
) {
  generated_key <- "__generated_subject_ids__"
  candidates <- data.frame(
    key = generated_key,
    label = "Generated subject IDs",
    source_column = NA_character_,
    id_like = FALSE,
    is_generated = TRUE,
    stringsAsFactors = FALSE
  )
  candidate_values <- list()
  candidate_values[[generated_key]] <- generated_subject_ids(row_count)

  if (length(labels) > 0) {
    for (index in seq_along(labels)) {
      label <- labels[[index]]
      candidate_values_raw <- values[[index]]
      if (is.null(candidate_values_raw)) {
        next
      }
      candidate_values_clean <- subject_id_values(candidate_values_raw)
      if (!any(!is.na(blank_to_na(candidate_values_raw)))) {
        next
      }
      candidates <- rbind(
        candidates,
        data.frame(
          key = paste0("column_", index),
          label = label,
          source_column = label,
          id_like = is_subject_id_like_name(label),
          is_generated = FALSE,
          stringsAsFactors = FALSE
        )
      )
      candidate_values[[paste0("column_", index)]] <- candidate_values_clean
    }
  }

  non_generated <- candidates[!candidates$is_generated, , drop = FALSE]
  preferred <- non_generated$key[non_generated$id_like]
  default_key <- if (length(preferred) > 0) {
    preferred[[1]]
  } else if (prefer_first_candidate && nrow(non_generated) > 0) {
    non_generated$key[[1]]
  } else {
    generated_key
  }

  list(
    candidates = candidates,
    values = candidate_values,
    default_key = default_key,
    selected_key = default_key
  )
}

subject_id_choices <- function(metadata) {
  if (is.null(metadata) || is.null(metadata$candidates)) {
    return(stats::setNames("__generated_subject_ids__", "Generated subject IDs"))
  }
  stats::setNames(metadata$candidates$key, metadata$candidates$label)
}

selected_subject_ids <- function(metadata, selected_key = NULL) {
  if (is.null(metadata)) {
    return(NULL)
  }
  key <- selected_key %||% metadata$selected_key %||% metadata$default_key
  values <- metadata$values[[key]]
  if (is.null(values)) {
    values <- metadata$values[[metadata$default_key]]
    key <- metadata$default_key
  }
  make.unique(as.character(values))
}

apply_subject_ids_to_normalized <- function(normalized, selected_key = NULL) {
  metadata <- normalized$audit$subject_id
  ids <- selected_subject_ids(metadata, selected_key)
  if (is.null(ids)) {
    return(normalized)
  }
  rownames(normalized$data) <- ids
  normalized$audit$subject_id$selected_key <- selected_key %||% metadata$default_key
  normalized
}

read_upload_csv <- function(path) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The data.table package is required for .csv uploads.", call. = FALSE)
  }

  as.data.frame(
    data.table::fread(
      file = path,
      check.names = FALSE,
      data.table = FALSE,
      na.strings = c("", "NA")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

read_upload_csv_raw <- function(path) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The data.table package is required for .csv uploads.", call. = FALSE)
  }

  as.data.frame(
    data.table::fread(
      file = path,
      header = FALSE,
      check.names = FALSE,
      data.table = FALSE,
      na.strings = c("", "NA")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

read_upload_excel_sheet <- function(path, sheet, col_names = TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The readxl package is required for .xls and .xlsx uploads.", call. = FALSE)
  }

  as.data.frame(
    readxl::read_excel(
      path,
      sheet = sheet,
      col_names = col_names,
      .name_repair = "minimal"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

read_upload_tables <- function(path, original_name = basename(path)) {
  ext <- tolower(tools::file_ext(original_name))

  if (ext == "csv") {
    return(list(list(
      sheet = NA_character_,
      canonical = read_upload_csv(path),
      raw = read_upload_csv_raw(path)
    )))
  }

  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("The readxl package is required for .xls and .xlsx uploads.", call. = FALSE)
    }
    sheets <- readxl::excel_sheets(path)
    return(lapply(sheets, function(sheet) {
      list(
        sheet = sheet,
        canonical = read_upload_excel_sheet(path, sheet, col_names = TRUE),
        raw = read_upload_excel_sheet(path, sheet, col_names = FALSE)
      )
    }))
  }

  stop("Unsupported upload type: .", ext, ". Please upload .csv, .xls, or .xlsx.", call. = FALSE)
}

wide_column_mapping <- function(column_names) {
  keys <- vapply(column_names, normalize_upload_text, character(1))
  compact_keys <- gsub(" ", "", keys)
  variables <- mapply(canonical_response_from_key, keys, compact_keys, USE.NAMES = FALSE)
  glucose <- vapply(keys, canonical_glucose_from_key, character(1))
  keep <- !is.na(variables) & glucose %in% c("45", "90")

  if (!any(keep)) {
    return(data.frame(
      column_name = character(0),
      variable = character(0),
      glucose = character(0),
      canonical_column = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    column_name = column_names[keep],
    variable = variables[keep],
    glucose = glucose[keep],
    canonical_column = paste0(variables[keep], "_", glucose[keep]),
    stringsAsFactors = FALSE
  )
}

duplicate_mapping_error <- function(mappings, source_label = "upload") {
  duplicates <- mappings$canonical_column[duplicated(mappings$canonical_column)]
  duplicates <- unique(duplicates)
  if (length(duplicates) == 0) {
    return(NULL)
  }

  details <- vapply(duplicates, function(field) {
    candidates <- mappings$column_name[mappings$canonical_column == field]
    paste0(field, " <- ", paste(candidates, collapse = ", "))
  }, character(1))

  paste(
    "Multiple columns in",
    source_label,
    "map to the same scoring field:",
    paste(details, collapse = " | ")
  )
}

is_wide_pattern_upload <- function(df) {
  nrow(wide_column_mapping(names(df))) > 0
}

normalize_wide_upload <- function(df, sheet = NA_character_, mappings = NULL) {
  raw_names <- names(df)
  if (is.null(mappings)) {
    mappings <- wide_column_mapping(raw_names)
  }
  duplicate_error <- duplicate_mapping_error(mappings, "wide upload")
  if (!is.null(duplicate_error)) {
    stop(duplicate_error, call. = FALSE)
  }

  id_column_names <- setdiff(raw_names, mappings$column_name)
  id_metadata <- subject_id_metadata(
    row_count = nrow(df),
    labels = id_column_names,
    values = lapply(id_column_names, function(column_name) df[[column_name]]),
    prefer_first_candidate = FALSE
  )

  normalized <- as.data.frame(
    matrix(NA_real_, nrow = nrow(df), ncol = length(required_score_cols())),
    check.names = FALSE
  )
  names(normalized) <- required_score_cols()

  for (row_index in seq_len(nrow(mappings))) {
    mapping <- mappings[row_index, ]
    normalized[[mapping$canonical_column]] <- df[[mapping$column_name]]
  }

  rownames(normalized) <- selected_subject_ids(id_metadata)

  ignored_cols <- setdiff(raw_names, mappings$column_name)
  missing_after_parse <- setdiff(required_score_cols(), mappings$canonical_column)
  parser_warnings <- character(0)
  if (length(missing_after_parse) > 0) {
    parser_warnings <- c(
      parser_warnings,
      paste("Required scoring columns not found in wide upload:", paste(missing_after_parse, collapse = ", "))
    )
  }

  list(
    data = normalized,
    audit = list(
      detected_format = "pattern_wide",
      detected_sheet = sheet,
      parsed_columns = mappings$canonical_column,
      ignored_columns = ignored_cols,
      subject_id = id_metadata,
      parser_warnings = parser_warnings
    )
  )
}

score_column_map_for_header_pair <- function(raw_df, group_row, glucose_row) {
  current_response <- NA_character_
  group_keys <- vapply(raw_df[group_row, ], normalize_upload_text, character(1))
  group_compact_keys <- gsub(" ", "", group_keys)
  group_responses <- mapply(canonical_response_from_key, group_keys, group_compact_keys, USE.NAMES = FALSE)
  glucose_keys <- vapply(raw_df[glucose_row, ], normalize_upload_text, character(1))
  glucose <- vapply(glucose_keys, canonical_glucose_level, character(1))
  response_by_col <- rep(NA_character_, ncol(raw_df))

  for (col_index in seq_len(ncol(raw_df))) {
    response <- group_responses[[col_index]]
    if (!is.na(response)) {
      current_response <- response
    }
    response_by_col[[col_index]] <- current_response
  }

  keep <- !is.na(response_by_col) & glucose %in% c("45", "90")
  if (!any(keep)) {
    return(data.frame(
      column_index = integer(0),
      variable = character(0),
      glucose = character(0),
      canonical_column = character(0),
      stringsAsFactors = FALSE
    ))
  }

  mappings <- data.frame(
    column_index = which(keep),
    variable = response_by_col[keep],
    glucose = glucose[keep],
    canonical_column = paste0(response_by_col[keep], "_", glucose[keep]),
    stringsAsFactors = FALSE
  )
  mappings <- mappings[!duplicated(mappings$canonical_column), , drop = FALSE]
  mappings
}

best_raw_grouped_candidate <- function(raw_df) {
  max_header_row <- min(10, nrow(raw_df) - 1)
  best <- NULL

  for (group_row in seq_len(max_header_row)) {
    glucose_row <- group_row + 1
    mappings <- score_column_map_for_header_pair(raw_df, group_row, glucose_row)
    score <- nrow(mappings)

    if (is.null(best) || score > best$score) {
      best <- list(
        score = score,
        group_row = group_row,
        glucose_row = glucose_row,
        data_start_row = glucose_row + 1,
        mappings = mappings
      )
    }
  }

  best
}

raw_subject_id_metadata <- function(raw_df, mappings, candidate, data_rows) {
  first_score_col <- min(mappings$column_index)
  if (first_score_col <= 1) {
    return(subject_id_metadata(length(data_rows)))
  }

  candidate_cols <- seq_len(first_score_col - 1)
  labels <- character(0)
  values <- list()
  header_rows <- seq_len(candidate$data_start_row - 1)

  for (col_index in candidate_cols) {
    column_values <- raw_df[data_rows, col_index]
    if (!any(!is.na(blank_to_na(column_values)))) {
      next
    }
    header_values <- blank_to_na(raw_df[header_rows, col_index])
    header_values <- header_values[!is.na(header_values)]
    label <- if (length(header_values) > 0) {
      header_values[[1]]
    } else {
      paste("Raw column", col_index)
    }
    labels <- c(labels, label)
    values[[length(values) + 1]] <- column_values
  }

  subject_id_metadata(
    row_count = length(data_rows),
    labels = labels,
    values = values,
    prefer_first_candidate = TRUE
  )
}

normalize_raw_grouped_upload <- function(raw_df, sheet = NA_character_, candidate = NULL) {
  names(raw_df) <- paste0("V", seq_len(ncol(raw_df)))
  raw_df[] <- lapply(raw_df, as.character)
  if (is.null(candidate)) {
    candidate <- best_raw_grouped_candidate(raw_df)
  }

  if (is.null(candidate) || candidate$score == 0) {
    stop("No recognizable clamp response blocks were found in the upload.", call. = FALSE)
  }

  data_rows <- seq(candidate$data_start_row, nrow(raw_df))
  mappings <- candidate$mappings
  mappings$column_name <- paste0("Raw column ", mappings$column_index)
  duplicate_error <- duplicate_mapping_error(mappings, "raw grouped upload")
  if (!is.null(duplicate_error)) {
    stop(duplicate_error, call. = FALSE)
  }

  has_any_score_value <- apply(raw_df[data_rows, mappings$column_index, drop = FALSE], 1, function(x) {
    any(!is.na(blank_to_na(x)))
  })
  data_rows <- data_rows[has_any_score_value]

  normalized <- as.data.frame(
    matrix(NA_real_, nrow = length(data_rows), ncol = length(required_score_cols())),
    check.names = FALSE
  )
  names(normalized) <- required_score_cols()

  for (row_index in seq_len(nrow(mappings))) {
    mapping <- mappings[row_index, ]
    normalized[[mapping$canonical_column]] <- raw_df[data_rows, mapping$column_index]
  }

  id_metadata <- raw_subject_id_metadata(raw_df, mappings, candidate, data_rows)
  rownames(normalized) <- selected_subject_ids(id_metadata)
  parsed_columns <- mappings$canonical_column
  missing_after_parse <- setdiff(required_score_cols(), parsed_columns)
  parser_warnings <- character(0)
  if (length(missing_after_parse) > 0) {
    parser_warnings <- c(
      parser_warnings,
      paste("Required scoring columns not found in raw layout:", paste(missing_after_parse, collapse = ", "))
    )
  }

  list(
    data = normalized,
    audit = list(
      detected_format = "raw_grouped_workbook",
      detected_sheet = sheet,
      parsed_columns = parsed_columns,
      ignored_columns = paste0("Raw column ", setdiff(seq_len(ncol(raw_df)), mappings$column_index)),
      subject_id = id_metadata,
      parser_warnings = parser_warnings
    )
  )
}

normalize_uploaded_clamp <- function(path, original_name = basename(path)) {
  tables <- read_upload_tables(path, original_name)
  best_raw <- NULL

  for (table in tables) {
    wide_mappings <- wide_column_mapping(names(table$canonical))
    if (nrow(wide_mappings) > 0) {
      return(normalize_wide_upload(table$canonical, sheet = table$sheet, mappings = wide_mappings))
    }

    candidate <- best_raw_grouped_candidate(table$raw)
    if (!is.null(candidate) && (is.null(best_raw) || candidate$score > best_raw$candidate$score)) {
      best_raw <- list(table = table, candidate = candidate)
    }
  }

  if (!is.null(best_raw) && best_raw$candidate$score > 0) {
    return(normalize_raw_grouped_upload(best_raw$table$raw, sheet = best_raw$table$sheet, candidate = best_raw$candidate))
  }

  stop("Upload did not match a canonical wide file or a raw grouped clamp workbook.", call. = FALSE)
}

upload_audit_frame <- function(audit) {
  subject_label <- ""
  if (!is.null(audit$subject_id)) {
    selected_key <- audit$subject_id$selected_key %||% audit$subject_id$default_key
    matched <- audit$subject_id$candidates[audit$subject_id$candidates$key == selected_key, , drop = FALSE]
    if (nrow(matched) > 0) {
      subject_label <- matched$label[[1]]
    }
  }

  data.frame(
    detected_format = audit$detected_format,
    detected_sheet = ifelse(is.na(audit$detected_sheet), "", audit$detected_sheet),
    subject_id_source = subject_label,
    parsed_column_count = length(audit$parsed_columns),
    ignored_column_count = length(audit$ignored_columns),
    parser_warnings = paste(audit$parser_warnings, collapse = " | "),
    check.names = FALSE
  )
}

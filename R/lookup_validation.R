lookup_annotation_column <- function(lookup_index) {
  paste0("dd", as.integer(lookup_index))
}


lookup_label_value <- function(config, lookup_index) {
  label <- config[[paste0("lookup", as.integer(lookup_index), "Label")]]
  if (is.null(label) || !nzchar(paste0(label))) {
    return(paste0("Lookup ", as.integer(lookup_index)))
  }

  paste0(label)
}


lookup_enabled_value <- function(config, lookup_index) {
  lookup_index <- as.integer(lookup_index)
  if (identical(lookup_index, 1L)) {
    return(TRUE)
  }

  isTRUE(config[[paste0("lookup", lookup_index, "Enabled")]])
}


lookup_annotation_values <- function(annotation_data, lookup_index) {
  annotation_data <- normalize_annotation_dataframe(annotation_data)
  column_name <- lookup_annotation_column(lookup_index)

  if (!column_name %in% names(annotation_data)) {
    return(character(0))
  }

  values <- trimws(as.character(annotation_data[[column_name]]))
  values <- values[!is.na(values) & nzchar(values) & values != "NA"]
  sort(unique(values))
}


lookup_table_values_from_data <- function(table_data) {
  if (is.null(table_data)) {
    return(character(0))
  }

  table_data <- as.data.frame(table_data, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"value" %in% names(table_data)) {
    return(character(0))
  }

  values <- trimws(as.character(table_data[["value"]]))
  values <- values[!is.na(values) & nzchar(values) & values != "NA"]
  sort(unique(values))
}


lookup_table_values_from_config <- function(config, lookup_index, data_dir = myEnv$data_dir) {
  csv_file <- config[[paste0("lookup", as.integer(lookup_index), "CsvFile")]]
  if (is.null(csv_file) || !nzchar(paste0(csv_file))) {
    return(character(0))
  }

  lookup_values <- tryCatch(
    load_lookup(
      fileToLoad = csv_file,
      display_column = "display",
      value_column = "value",
      data_dir = data_dir
    ),
    error = function(e) list()
  )

  values <- trimws(as.character(unlist(lookup_values, use.names = FALSE)))
  values <- values[!is.na(values) & nzchar(values) & values != "NA"]
  sort(unique(values))
}


format_lookup_validation_values <- function(values, max_values = 8L) {
  values <- sort(unique(values))
  if (length(values) <= max_values) {
    return(paste(values, collapse = ", "))
  }

  paste0(
    paste(values[seq_len(max_values)], collapse = ", "),
    ", and ",
    length(values) - max_values,
    " more"
  )
}


validate_lookup_values_for_annotations <- function(annotation_data,
                                                   lookup_index,
                                                   lookup_values,
                                                   lookup_enabled = TRUE,
                                                   lookup_label = NULL) {
  lookup_index <- as.integer(lookup_index)
  annotation_values <- lookup_annotation_values(annotation_data, lookup_index)

  if (length(annotation_values) == 0) {
    return(list(valid = TRUE, messages = character(0), missing_values = character(0)))
  }

  if (is.null(lookup_label) || !nzchar(paste0(lookup_label))) {
    lookup_label <- paste0("Lookup ", lookup_index)
  }

  if (!isTRUE(lookup_enabled)) {
    return(list(
      valid = FALSE,
      messages = paste0(
        "Lookup ",
        lookup_index,
        " (",
        lookup_label,
        ") is disabled, but existing annotations use: ",
        format_lookup_validation_values(annotation_values),
        "."
      ),
      missing_values = annotation_values
    ))
  }

  lookup_values <- sort(unique(trimws(as.character(lookup_values))))
  lookup_values <- lookup_values[!is.na(lookup_values) & nzchar(lookup_values) & lookup_values != "NA"]
  missing_values <- setdiff(annotation_values, lookup_values)

  if (length(missing_values) == 0) {
    return(list(valid = TRUE, messages = character(0), missing_values = character(0)))
  }

  list(
    valid = FALSE,
    messages = paste0(
      "Lookup ",
      lookup_index,
      " (",
      lookup_label,
      ") is missing values already used by annotations: ",
      format_lookup_validation_values(missing_values),
      "."
    ),
    missing_values = missing_values
  )
}


validate_lookup_settings_against_annotations <- function(annotation_data,
                                                         config,
                                                         data_dir = myEnv$data_dir) {
  validation_results <- lapply(seq_len(4), function(lookup_index) {
    validate_lookup_values_for_annotations(
      annotation_data = annotation_data,
      lookup_index = lookup_index,
      lookup_values = lookup_table_values_from_config(
        config = config,
        lookup_index = lookup_index,
        data_dir = data_dir
      ),
      lookup_enabled = lookup_enabled_value(config, lookup_index),
      lookup_label = lookup_label_value(config, lookup_index)
    )
  })

  messages <- unlist(lapply(validation_results, `[[`, "messages"), use.names = FALSE)
  list(
    valid = length(messages) == 0,
    messages = messages,
    results = validation_results
  )
}


validate_settings_lookup_csv_change <- function(annotation_data,
                                                spec,
                                                table_data,
                                                config = myEnv$config) {
  lookup_index <- suppressWarnings(as.integer(sub("^lookup", "", spec$key)))
  if (is.na(lookup_index) || lookup_index < 1L || lookup_index > 4L) {
    return(list(valid = TRUE, messages = character(0), results = list()))
  }

  result <- validate_lookup_values_for_annotations(
    annotation_data = annotation_data,
    lookup_index = lookup_index,
    lookup_values = lookup_table_values_from_data(table_data),
    lookup_enabled = TRUE,
    lookup_label = lookup_label_value(config, lookup_index)
  )

  list(
    valid = isTRUE(result$valid),
    messages = result$messages,
    results = list(result)
  )
}

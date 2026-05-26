#' Project settings export helpers
#'
#' @noRd

safe_shiny_roots <- function(volume_getter = shinyFiles::getVolumes()) {
  roots <- tryCatch(
    volume_getter(),
    warning = function(w) NULL,
    error = function(e) NULL
  )

  if (!is.null(roots) && length(roots) > 0 && !is.null(names(roots)) && all(nzchar(names(roots)))) {
    return(roots)
  }

  home_path <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  if (!dir.exists(home_path)) {
    home_path <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  }

  c(Home = home_path)
}


default_project_settings_export_name <- function(config = myEnv$config) {
  project_folder <- config$projectFolder
  if (is.null(project_folder)) {
    project_folder <- ""
  }
  folder_name <- basename(
    normalizePath(project_folder, winslash = "/", mustWork = FALSE)
  )

  if (is.null(folder_name) || !nzchar(folder_name) || identical(folder_name, ".")) {
    folder_name <- "pannotator-project"
  }

  paste0(folder_name, ".yml")
}


normalize_project_settings_export_path <- function(save_selection, default_extension = "yml") {
  if (is.null(save_selection) || nrow(save_selection) == 0) {
    return(NULL)
  }

  export_path <- as.character(save_selection$datapath[[1]])
  export_type <- as.character(save_selection$type[[1]])

  if (!nzchar(tools::file_ext(export_path))) {
    extension <- default_extension
    if (!is.na(export_type) && nzchar(export_type)) {
      extension <- export_type
    }
    export_path <- paste0(export_path, ".", extension)
  }

  normalizePath(export_path, winslash = "/", mustWork = FALSE)
}

normalize_export_only_google_maps_key <- function(google_maps_api_key = NULL) {
  if (is.null(google_maps_api_key)) {
    return(NULL)
  }

  google_maps_api_key <- trimws(paste0(google_maps_api_key))
  if (!nzchar(google_maps_api_key)) {
    return(NULL)
  }

  google_maps_api_key
}


build_project_settings_export_config <- function(
    config = myEnv$config,
    data_path = NULL,
    google_maps_api_key = NULL
) {
  if (is.null(data_path) || !nzchar(data_path)) {
    data_path <- config$projectFolder
  }

  if (is.null(data_path) || !nzchar(data_path)) {
    data_path <- myEnv$data_dir
  }

  export_config <- merge_panel_config(config = config, data_path = data_path)
  export_config$projectFolder <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
  google_maps_api_key <- normalize_export_only_google_maps_key(google_maps_api_key)

  if (!is.null(google_maps_api_key)) {
    export_config$mapPanelSource <- "Google.Maps"
    export_config$mapAPIKey <- google_maps_api_key
  }

  export_config
}


project_settings_bundle_file_fields <- function() {
  c(
    "usernameLookupFile",
    paste0("lookup", seq_len(4), "CsvFile"),
    paste0("lookup", seq_len(4), "HelpFile"),
    "annotationsFile"
  )
}


project_settings_bundle_file_name <- function(file_name, fallback_file_name) {
  file_name <- trimws(paste0(file_name))
  if (!nzchar(file_name)) {
    file_name <- fallback_file_name
  }

  file_name <- basename(gsub("\\\\", "/", file_name))
  if (!nzchar(file_name) || identical(file_name, ".") || identical(file_name, "/")) {
    file_name <- fallback_file_name
  }

  file_name
}


is_absolute_file_path <- function(file_path) {
  grepl("^([A-Za-z]:)?[/\\\\]", file_path)
}


project_settings_bundle_source_path <- function(file_name, data_dir) {
  file_name <- trimws(paste0(file_name))
  if (!nzchar(file_name)) {
    return("")
  }

  if (is_absolute_file_path(file_name)) {
    return(normalizePath(file_name, winslash = "/", mustWork = FALSE))
  }

  normalizePath(file.path(data_dir, file_name), winslash = "/", mustWork = FALSE)
}


build_project_settings_bundle_config <- function(
    config = myEnv$config,
    export_dir,
    google_maps_api_key = NULL
) {
  export_dir <- normalizePath(export_dir, winslash = "/", mustWork = FALSE)
  export_config <- build_project_settings_export_config(
    config = config,
    data_path = export_dir,
    google_maps_api_key = google_maps_api_key
  )
  defaults <- default_app_config(export_dir)

  for (field in project_settings_bundle_file_fields()) {
    export_config[[field]] <- project_settings_bundle_file_name(
      file_name = config[[field]],
      fallback_file_name = defaults[[field]]
    )
  }

  export_config
}


copy_project_settings_bundle_files <- function(
    config = myEnv$config,
    export_config,
    export_dir,
    source_data_dir = NULL,
    annotations_data = NULL,
    annotations_file_path = NULL
) {
  if (is.null(source_data_dir) || !nzchar(source_data_dir)) {
    source_data_dir <- config$projectFolder
  }
  if (is.null(source_data_dir) || !nzchar(source_data_dir)) {
    source_data_dir <- myEnv$data_dir
  }

  source_data_dir <- normalizePath(source_data_dir, winslash = "/", mustWork = FALSE)
  export_dir <- normalizePath(export_dir, winslash = "/", mustWork = FALSE)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  copied_files <- list()
  for (field in project_settings_bundle_file_fields()) {
    destination <- normalizePath(
      file.path(export_dir, export_config[[field]]),
      winslash = "/",
      mustWork = FALSE
    )
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

    if (identical(field, "annotationsFile")) {
      source <- annotations_file_path
      if (is.null(source) || !nzchar(source)) {
        source <- project_settings_bundle_source_path(config[[field]], source_data_dir)
      }

      if (!is.null(annotations_data)) {
        saveRDS(normalize_annotation_dataframe(annotations_data), destination)
        copied <- TRUE
        note <- "saved current annotations"
      } else if (nzchar(source) && file.exists(source)) {
        source <- normalizePath(source, winslash = "/", mustWork = FALSE)
        copied <- identical(source, destination) || file.copy(source, destination, overwrite = TRUE)
        note <- if (isTRUE(copied)) "copied" else "copy failed"
      } else {
        saveRDS(create_user_dataframe(), destination)
        copied <- TRUE
        note <- "created empty annotations file"
      }
    } else {
      source <- project_settings_bundle_source_path(config[[field]], source_data_dir)
      if (nzchar(source) && file.exists(source)) {
        source <- normalizePath(source, winslash = "/", mustWork = FALSE)
        copied <- identical(source, destination) || file.copy(source, destination, overwrite = TRUE)
        note <- if (isTRUE(copied)) "copied" else "copy failed"
      } else {
        copied <- FALSE
        note <- "source file not found"
      }
    }

    copied_files[[length(copied_files) + 1L]] <- data.frame(
      field = field,
      file = export_config[[field]],
      source = source,
      destination = destination,
      copied = isTRUE(copied),
      note = note,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, copied_files)
}


write_project_settings_yaml <- function(
    config = myEnv$config,
    file_path,
    data_path = NULL,
    google_maps_api_key = NULL
) {
  export_config <- build_project_settings_export_config(
    config = config,
    data_path = data_path,
    google_maps_api_key = google_maps_api_key
  )
  export_path <- normalizePath(file_path, winslash = "/", mustWork = FALSE)

  dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
  configr::write.config(
    config.dat = export_config,
    file.path = export_path,
    write.type = "yaml",
    indent = 4
  )

  normalizePath(export_path, winslash = "/", mustWork = FALSE)
}


export_project_settings_bundle <- function(
    config = myEnv$config,
    export_dir,
    yaml_file_name = default_project_settings_export_name(config),
    source_data_dir = NULL,
    annotations_data = NULL,
    annotations_file_path = NULL,
    google_maps_api_key = NULL
) {
  export_dir <- normalizePath(export_dir, winslash = "/", mustWork = FALSE)
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  yaml_file_name <- basename(yaml_file_name)
  if (!nzchar(tools::file_ext(yaml_file_name))) {
    yaml_file_name <- paste0(yaml_file_name, ".yml")
  }

  export_config <- build_project_settings_bundle_config(
    config = config,
    export_dir = export_dir,
    google_maps_api_key = google_maps_api_key
  )
  copied_files <- copy_project_settings_bundle_files(
    config = config,
    export_config = export_config,
    export_dir = export_dir,
    source_data_dir = source_data_dir,
    annotations_data = annotations_data,
    annotations_file_path = annotations_file_path
  )
  yaml_path <- write_project_settings_yaml(
    config = export_config,
    file_path = file.path(export_dir, yaml_file_name),
    data_path = export_dir
  )

  list(
    yaml_path = yaml_path,
    export_dir = export_dir,
    files = copied_files
  )
}


project_settings_run_app_example <- function(file_path) {
  paste0(
    "run_app(projectSettingsFile = \"",
    normalizePath(file_path, winslash = "/", mustWork = FALSE),
    "\")"
  )
}

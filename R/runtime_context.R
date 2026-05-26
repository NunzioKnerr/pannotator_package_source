#' @noRd
default_user_config_dir <- function() {
  normalizePath(
    tools::R_user_dir("pannotator", which = "config"),
    mustWork = FALSE
  )
}


#' @noRd
default_user_data_dir <- function() {
  normalizePath(
    tools::R_user_dir("pannotator", which = "data"),
    mustWork = FALSE
  )
}


#' @noRd
default_user_config_path <- function(config_dir = default_user_config_dir()) {
  normalizePath(
    file.path(config_dir, "default-project-config.yml"),
    mustWork = FALSE
  )
}


#' @noRd
create_runtime_temp_dir <- function(temp_root = tempdir()) {
  runtime_dir <- tempfile("pannotator-runtime-", tmpdir = temp_root)
  dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(runtime_dir, mustWork = FALSE)
}


#' @noRd
resolve_project_settings_file <- function(
    projectSettingsFile = NULL,
    golem_options = golem::get_golem_options()
) {
  if (is.null(projectSettingsFile) || !nzchar(projectSettingsFile)) {
    projectSettingsFile <- golem_options$projectSettingsFile
  }

  if (is.null(projectSettingsFile) || !nzchar(projectSettingsFile)) {
    return(NULL)
  }

  normalizePath(projectSettingsFile, mustWork = FALSE)
}


#' @noRd
read_runtime_config <- function(config_path, data_dir = NULL) {
  if (file.exists(config_path)) {
    config <- configr::read.config(config_path)
  } else {
    config <- list()
  }

  if (is.null(data_dir) || !nzchar(data_dir)) {
    data_dir <- config$projectFolder
  }

  if (is.null(data_dir) || !nzchar(data_dir)) {
    data_dir <- default_user_data_dir()
  }

  data_dir <- normalizePath(data_dir, mustWork = FALSE)
  merge_panel_config(config, data_path = data_dir)
}


#' @noRd
build_runtime_context <- function(
    projectSettingsFile = NULL,
    initialize = TRUE,
    golem_options = golem::get_golem_options()
) {
  if (isTRUE(initialize)) {
    initialize_config()
  }

  resolved_project_file <- resolve_project_settings_file(
    projectSettingsFile = projectSettingsFile,
    golem_options = golem_options
  )

  source <- "default"
  config_dir <- default_user_config_dir()
  config_path <- default_user_config_path(config_dir)

  if (!is.null(resolved_project_file)) {
    if (!file.exists(resolved_project_file)) {
      stop(
        "Project settings file not found: ",
        resolved_project_file,
        call. = FALSE
      )
    }

    source <- "project"
    config_path <- resolved_project_file
    config_dir <- normalizePath(dirname(config_path), mustWork = FALSE)
  }

  config <- read_runtime_config(config_path)
  data_dir <- normalizePath(config$projectFolder, mustWork = FALSE)

  list(
    config = config,
    config_path = normalizePath(config_path, mustWork = FALSE),
    config_dir = config_dir,
    data_dir = data_dir,
    projectSettingsFile = resolved_project_file,
    source = source,
    runtime_temp_dir = create_runtime_temp_dir()
  )
}


#' @noRd
create_app_state <- function(context = NULL) {
  runtime <- shiny::reactiveValues()
  reset_runtime_state(runtime)

  if (!is.null(context)) {
    runtime$config <- context$config
    runtime$runtime_context <- context
  }

  runtime
}


#' @noRd
runtime_context_value <- function(runtime = r) {
  context <- tryCatch(
    shiny::isolate(runtime$runtime_context),
    error = function(e) NULL
  )

  if (is.null(context)) {
    context <- myEnv$runtime_context
  }

  if (is.null(context)) {
    context <- build_runtime_context(initialize = TRUE)
  }

  context
}


#' @noRd
runtime_data_dir <- function(runtime = r) {
  context <- runtime_context_value(runtime)

  if (!is.null(context$data_dir) && nzchar(context$data_dir)) {
    return(context$data_dir)
  }

  normalizePath(myEnv$data_dir, mustWork = FALSE)
}


#' @noRd
runtime_temp_dir <- function(runtime = r) {
  context <- runtime_context_value(runtime)

  if (!is.null(context$runtime_temp_dir) && nzchar(context$runtime_temp_dir)) {
    return(normalizePath(context$runtime_temp_dir, mustWork = FALSE))
  }

  normalizePath(tempdir(), mustWork = FALSE)
}


#' @noRd
runtime_kmz_dir <- function(runtime = r) {
  kmz_dir <- tryCatch(
    shiny::isolate(runtime$current_kmz_dir),
    error = function(e) NULL
  )

  if (!is.null(kmz_dir) && nzchar(kmz_dir)) {
    return(normalizePath(kmz_dir, mustWork = FALSE))
  }

  normalizePath(file.path(runtime_temp_dir(runtime), "kmz"), mustWork = FALSE)
}


#' @noRd
new_runtime_kmz_dir <- function(runtime = r) {
  kmz_dir <- tempfile("kmz-", tmpdir = runtime_temp_dir(runtime))
  dir.create(kmz_dir, recursive = TRUE, showWarnings = FALSE)
  runtime$current_kmz_dir <- normalizePath(kmz_dir, mustWork = FALSE)
  runtime_kmz_dir(runtime)
}


#' @noRd
runtime_kml_path <- function(runtime = r) {
  file.path(runtime_kmz_dir(runtime), "doc.kml")
}


#' @noRd
runtime_kmz_files_dir <- function(runtime = r) {
  file.path(runtime_kmz_dir(runtime), "files")
}


#' @noRd
encode_resource_path <- function(path) {
  path_segments <- strsplit(gsub("\\\\", "/", path), "/", fixed = FALSE)[[1]]
  path_segments <- path_segments[nzchar(path_segments)]
  paste(vapply(path_segments, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
}


#' @noRd
runtime_resource_relative_path <- function(path, root = normalizePath(tempdir(), mustWork = FALSE)) {
  normalized_path <- gsub("\\\\", "/", normalizePath(path, mustWork = FALSE))
  normalized_root <- gsub("\\\\", "/", normalizePath(root, mustWork = FALSE))
  escaped_root <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", normalized_root)
  sub(paste0("^", escaped_root, "/?"), "", normalized_path)
}


#' @noRd
runtime_resource_url <- function(path, runtime = r) {
  relative_path <- runtime_resource_relative_path(path, root = normalizePath(tempdir(), mustWork = FALSE))
  paste0("/temp_dir/", encode_resource_path(relative_path))
}


#' @noRd
runtime_image_url <- function(image_name, runtime = r) {
  runtime_resource_url(file.path(runtime_kmz_files_dir(runtime), image_name), runtime = runtime)
}


#' @noRd
runtime_config_value <- function(runtime = r) {
  config <- tryCatch(
    shiny::isolate(runtime$config),
    error = function(e) NULL
  )

  if (!is.null(config)) {
    return(config)
  }

  myEnv$config
}


#' @noRd
runtime_config_path <- function(runtime = r) {
  context <- runtime_context_value(runtime)

  if (!is.null(context$config_path) && nzchar(context$config_path)) {
    return(context$config_path)
  }

  normalizePath(myEnv$project_config_file, mustWork = FALSE)
}


#' @noRd
update_runtime_context_config <- function(runtime = r) {
  context <- runtime_context_value(runtime)
  context$config <- shiny::isolate(runtime$config)
  runtime$runtime_context <- context
  myEnv$runtime_context <- context
  myEnv$config <- shiny::isolate(runtime$config)
  invisible(context)
}


#' @noRd
prepare_runtime_help_files <- function(context) {
  for (i in 1:4) {
    lookup_file <- paste0("lookup", i, "HelpFile")
    from_path <- normalizePath(
      file.path(context$data_dir, context$config[[lookup_file]]),
      mustWork = FALSE
    )
    to_path <- normalizePath(
      file.path(context$runtime_temp_dir, paste0("help", i, ".pdf")),
      mustWork = FALSE
    )

    if (file.exists(from_path)) {
      file.copy(from_path, to_path, overwrite = TRUE)
    }
  }

  invisible(context)
}


#' @noRd
reset_runtime_state <- function(runtime = r) {
  reset_names <- c(
    "active_annotations",
    "annotation_card_observers",
    "annotation_cards",
    "annotation_panel_notice",
    "annotation_polygons",
    "annotation_table_notice",
    "config",
    "current_annotation_360markers",
    "current_annotation_360polygons",
    "current_annotation_markers",
    "current_annotation_polygons",
    "current_annotation_whole_images",
    "current_image",
    "current_image_metadata",
    "current_kmz_name",
    "current_map_zoom",
    "destroy",
    "exiftool_status",
    "geometry",
    "image_panel_notice",
    "imgs_lst",
    "imgs_metadata",
    "map_panel_notice",
    "new_leaflet360_item",
    "new_leafletMap_item",
    "refresh_user_config",
    "remove_leaflet360_item",
    "remove_leafletMap_item",
    "settings_panel_notice",
    "settings_pending_action",
    "user_annotations_data",
    "user_annotations_file_name",
    "user_name",
    "var_choices",
    "var_dropdown1",
    "var_dropdown2",
    "var_dropdown3",
    "var_dropdown4",
    "mapIcons",
    "formIcons"
  )

  for (value_name in reset_names) {
    runtime[[value_name]] <- NULL
  }

  runtime$active_annotations <- shiny::reactiveVal(value = NULL)
  runtime$annotation_cards <- list()
  runtime$annotation_card_observers <- list()
  runtime$remove_leafletMap_item <- shiny::reactiveVal(value = NULL)
  runtime$remove_leaflet360_item <- shiny::reactiveVal(value = NULL)
  runtime$annotation_panel_notice <- NULL
  runtime$map_panel_notice <- NULL
  runtime$image_panel_notice <- NULL
  runtime$settings_panel_notice <- NULL
  runtime$settings_pending_action <- NULL
  runtime$annotation_table_notice <- NULL
  runtime$exiftool_status <- NULL
  runtime$refresh_user_config <- NULL
  runtime$workspace_reset <- NULL
  runtime$current_map_zoom <- 12
  runtime$config <- NULL
  runtime$runtime_context <- NULL
  runtime$var_choices <- NULL
  runtime$var_dropdown1 <- NULL
  runtime$var_dropdown2 <- NULL
  runtime$var_dropdown3 <- NULL
  runtime$var_dropdown4 <- NULL
  runtime$mapIcons <- NULL
  runtime$formIcons <- NULL
  runtime$current_kmz_dir <- NULL
  runtime$current_kmz_name <- NULL

  invisible(runtime)
}


#' @noRd
reset_loaded_kmz_workspace <- function(
    runtime = r,
    message = "Lookup settings changed. Reload a KMZ file to continue with the updated lookup choices.",
    title = "Workspace Reset",
    type = "info"
) {
  shiny::isolate(clear_annotations_form(runtime = runtime))

  runtime$current_annotation_360markers <- NULL
  runtime$current_annotation_360polygons <- NULL
  runtime$current_annotation_markers <- NULL
  runtime$current_annotation_polygons <- NULL
  runtime$current_annotation_whole_images <- NULL
  runtime$current_image <- NULL
  runtime$current_image_metadata <- NULL
  runtime$current_kmz_name <- NULL
  runtime$current_map_zoom <- 12
  runtime$geometry <- NULL
  runtime$imgs_lst <- NULL
  runtime$imgs_metadata <- NULL
  runtime$new_leaflet360_item <- NULL
  runtime$new_leafletMap_item <- NULL
  runtime$current_kmz_dir <- NULL
  runtime$image_panel_notice <- NULL
  runtime$map_panel_notice <- list(
    title = title,
    message = message,
    type = type
  )
  runtime$workspace_reset <- as.character(Sys.time())

  invisible(runtime)
}


#' @noRd
refresh_app_state_assets <- function(runtime = r) {
  config <- tryCatch(
    shiny::isolate(runtime$config),
    error = function(e) NULL
  )
  req(config)

  data_dir <- runtime_data_dir(runtime)

  runtime$var_choices <- load_lookup(
    fileToLoad = config$usernameLookupFile,
    display_column = "user_name",
    value_column = "value",
    data_dir = data_dir
  )

  runtime$var_dropdown1 <- load_lookup(
    fileToLoad = config$lookup1CsvFile,
    display_column = "display",
    value_column = "value",
    data_dir = data_dir
  )

  runtime$var_dropdown2 <- load_lookup(
    fileToLoad = config$lookup2CsvFile,
    display_column = "display",
    value_column = "value",
    data_dir = data_dir
  )

  runtime$var_dropdown3 <- load_lookup(
    fileToLoad = config$lookup3CsvFile,
    display_column = "display",
    value_column = "value",
    data_dir = data_dir
  )

  runtime$var_dropdown4 <- load_lookup(
    fileToLoad = config$lookup4CsvFile,
    display_column = "display",
    value_column = "value",
    data_dir = data_dir
  )

  runtime$mapIcons <- create_map_icons(config = config)
  runtime$formIcons <- create_form_icons(config = config)

  invisible(runtime)
}


#' @noRd
synchronize_runtime_context <- function(
    context,
    runtime = r,
    env = myEnv,
    prepare_temp_assets = TRUE
) {
  reset_runtime_state(runtime)
  rm(list = ls(envir = env), envir = env)

  env$config_dir <- context$config_dir
  env$data_dir <- context$data_dir
  env$project_config_file <- context$config_path
  env$config <- context$config
  env$runtime_context <- context

  runtime$config <- context$config
  runtime$runtime_context <- context

  if (isTRUE(prepare_temp_assets)) {
    prepare_runtime_help_files(context)
  }

  refresh_app_state_assets(runtime)

  invisible(context)
}


#' @noRd
current_runtime_context <- function(
    initialize = FALSE,
    projectSettingsFile = NULL,
    golem_options = golem::get_golem_options()
) {
  if (is.null(projectSettingsFile) || !nzchar(projectSettingsFile)) {
    projectSettingsFile <- golem_options$projectSettingsFile
  }

  if ((is.null(projectSettingsFile) || !nzchar(projectSettingsFile)) &&
      !is.null(golem_options$runtime_context) &&
      identical(golem_options$runtime_context$source, "project")) {
    projectSettingsFile <- golem_options$runtime_context$config_path
  }

  build_runtime_context(
    projectSettingsFile = projectSettingsFile,
    initialize = initialize,
    golem_options = golem_options
  )
}

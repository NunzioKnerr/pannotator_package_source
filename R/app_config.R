#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "pannotator")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

# r for all the reactive values
r <- shiny::reactiveValues()
# track active annotations so we can remove them
r$active_annotations <- reactiveVal(value = NULL)
r$annotation_cards <- list()
r$annotation_card_observers <- list()
r$remove_leafletMap_item <- reactiveVal(value = NULL)
r$remove_leaflet360_item <- reactiveVal(value = NULL)
r$annotation_panel_notice <- NULL
r$map_panel_notice <- NULL
r$image_panel_notice <- NULL
r$settings_panel_notice <- NULL
r$settings_pending_action <- NULL
r$annotation_table_notice <- NULL
r$exiftool_status <- NULL
r$refresh_user_config <- NULL
r$current_map_zoom <-  12
#r$current_image <- reactiveVal(value = NULL)


default_panel_order <- function() {
  c("map", "image", "annotation", "annotation_table")
}


main_panel_width_specs <- function() {
  list(
    mapPanelWidth = c(min = 3L, max = 6L),
    panoPanelWidth = c(min = 3L, max = 6L),
    formPanelWidth = c(min = 2L, max = 4L)
  )
}


balance_panel_widths <- function(widths, target_total, mins, maxs) {
  widths <- pmax(mins, pmin(maxs, as.integer(widths)))
  current_total <- sum(widths)

  if (current_total > target_total) {
    while (current_total > target_total) {
      reducible <- widths - mins
      if (!any(reducible > 0L)) {
        break
      }

      for (field in names(sort(reducible, decreasing = TRUE))) {
        if (current_total <= target_total) {
          break
        }
        if (reducible[[field]] > 0L) {
          widths[[field]] <- widths[[field]] - 1L
          current_total <- current_total - 1L
        }
      }
    }
  } else if (current_total < target_total) {
    while (current_total < target_total) {
      expandable <- maxs - widths
      if (!any(expandable > 0L)) {
        break
      }

      for (field in names(sort(expandable, decreasing = TRUE))) {
        if (current_total >= target_total) {
          break
        }
        if (expandable[[field]] > 0L) {
          widths[[field]] <- widths[[field]] + 1L
          current_total <- current_total + 1L
        }
      }
    }
  }

  widths
}


normalize_main_panel_widths <- function(widths, changed_field = NULL) {
  specs <- main_panel_width_specs()
  fields <- names(specs)
  mins <- stats::setNames(vapply(specs, function(spec) spec[["min"]], integer(1)), fields)
  maxs <- stats::setNames(vapply(specs, function(spec) spec[["max"]], integer(1)), fields)

  normalized_widths <- stats::setNames(
    vapply(fields, function(field) {
      width_value <- suppressWarnings(as.integer(widths[[field]]))
      if (is.na(width_value)) {
        width_value <- mins[[field]]
      }
      width_value
    }, integer(1)),
    fields
  )

  normalized_widths <- pmax(mins, pmin(maxs, normalized_widths))

  if (!is.null(changed_field) && changed_field %in% fields) {
    other_fields <- setdiff(fields, changed_field)
    min_other_total <- sum(mins[other_fields])
    max_other_total <- sum(maxs[other_fields])
    normalized_widths[[changed_field]] <- max(
      mins[[changed_field]],
      min(
        maxs[[changed_field]],
        12L - min_other_total,
        normalized_widths[[changed_field]]
      )
    )
    normalized_widths[[changed_field]] <- min(
      normalized_widths[[changed_field]],
      12L - min_other_total
    )
    normalized_widths[[changed_field]] <- max(
      normalized_widths[[changed_field]],
      12L - max_other_total
    )

    normalized_widths[other_fields] <- balance_panel_widths(
      widths = normalized_widths[other_fields],
      target_total = 12L - normalized_widths[[changed_field]],
      mins = mins[other_fields],
      maxs = maxs[other_fields]
    )
  } else {
    normalized_widths <- balance_panel_widths(
      widths = normalized_widths,
      target_total = 12L,
      mins = mins,
      maxs = maxs
    )
  }

  as.list(normalized_widths)
}


normalize_app_theme_mode <- function(app_theme_mode = NULL) {
  if (is.null(app_theme_mode) || length(app_theme_mode) == 0 || !nzchar(app_theme_mode[[1]])) {
    return("light")
  }

  app_theme_mode <- tolower(as.character(app_theme_mode[[1]]))
  if (!app_theme_mode %in% c("light", "dark")) {
    return("light")
  }

  app_theme_mode
}


normalize_config_boolean <- function(value = NULL, default = FALSE) {
  if (is.null(value) || length(value) == 0) {
    return(isTRUE(default))
  }

  if (is.logical(value)) {
    if (is.na(value[[1]])) {
      return(isTRUE(default))
    }
    return(isTRUE(value[[1]]))
  }

  value <- tolower(trimws(as.character(value[[1]])))
  if (is.na(value)) {
    return(isTRUE(default))
  }

  if (value %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }
  if (value %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }

  isTRUE(default)
}


default_app_config <- function(data_path) {
  list(
    configVersion = 3L,
    panelOrder = default_panel_order(),
    enabledPanels = default_panel_order(),
    settingsPlacement = "drawer",
    settingsDrawerPosition = "right",
    showWorkflowGuidanceNotices = TRUE,
    askGuidedTourOnStartup = TRUE,
    appTheme = "cerulean",
    appThemeMode = "light",
    mapPanelWidth = 5,
    panoPanelWidth = 5,
    formPanelWidth = 2,
    annotationTablePanelWidth = 12,
    mapPanelSource = "Esri.WorldImagery",
    mapAPIKey = "",
    mapIconColour = "green",
    mapMarkerColour = "white",
    mapPolygonStroke = TRUE,
    mapPolygonStrokeColour = "blue",
    mapPolygonStrokeWeight = 2,
    mapPolygonStrokeOpacity = 0.7,
    mapPolygonFill = TRUE,
    mapPolygonFillColour = "navy",
    mapPolygonFillOpacity = 0.3,
    pano360IconColour = "maroon",
    pano360MarkerColour = "white",
    pano360PolygonStroke = TRUE,
    pano360PolygonStrokeColour = "blue",
    pano360PolygonStrokeWeight = 1,
    pano360PolygonStrokeOpacity = 0.9,
    showPano360PolygonStrokeInCropExport = FALSE,
    pano360PolygonFill = TRUE,
    pano360PolygonFillColour = "purple",
    pano360PolygonFillOpacity = 0.1,
    showPano360PolygonFillInCropExport = TRUE,
    projectFolder = data_path,
    annotationsFile = "userAnnotations.rds",
    usernameLookupFile = "username_lookup.csv",
    exportFileFormat = "csv",
    lookup1Label = "Lookup_1",
    lookup1CsvFile = "lookup1.csv",
    lookup1HelpFile = "help1.pdf",
    lookup2Label = "Lookup_2",
    lookup2CsvFile = "lookup2.csv",
    lookup2HelpFile = "help2.pdf",
    lookup2Enabled = FALSE,
    lookup3Label = "Lookup_3",
    lookup3CsvFile = "lookup3.csv",
    lookup3HelpFile = "help3.pdf",
    lookup3Enabled = FALSE,
    lookup4Label = "Lookup_4",
    lookup4CsvFile = "lookup4.csv",
    lookup4HelpFile = "help4.pdf",
    lookup4Enabled = FALSE
  )
}


merge_panel_config <- function(config = list(), data_path = NULL) {
  if (is.null(data_path) || !nzchar(data_path)) {
    data_path <- config$projectFolder
  }

  if (is.null(data_path) || !nzchar(data_path)) {
    data_path <- normalizePath(
      file.path(tools::R_user_dir("pannotator", which = "data")),
      mustWork = FALSE
    )
  }

  defaults <- default_app_config(data_path)

  for (config_name in names(defaults)) {
    if (is.null(config[[config_name]])) {
      config[[config_name]] <- defaults[[config_name]]
    }
  }

  config_version <- suppressWarnings(as.integer(config$configVersion))
  if (is.na(config_version)) {
    config_version <- defaults$configVersion
  }
  is_migrating_to_v3 <- config_version < 3L
  config$configVersion <- max(config_version, defaults$configVersion)

  panel_order <- unique(as.character(unlist(config$panelOrder)))
  panel_order <- panel_order[panel_order %in% default_panel_order()]
  config$panelOrder <- c(panel_order, setdiff(default_panel_order(), panel_order))

  enabled_panels <- unique(as.character(unlist(config$enabledPanels)))
  enabled_panels <- enabled_panels[enabled_panels %in% default_panel_order()]
  if (length(enabled_panels) == 0 || isTRUE(is_migrating_to_v3)) {
    enabled_panels <- c(enabled_panels, setdiff(default_panel_order(), enabled_panels))
  }
  if (length(enabled_panels) == 0) {
    enabled_panels <- default_panel_order()
  }
  config$enabledPanels <- enabled_panels

  normalized_main_widths <- normalize_main_panel_widths(config)
  for (width_field in names(normalized_main_widths)) {
    config[[width_field]] <- normalized_main_widths[[width_field]]
  }

  settings_placement <- config$settingsPlacement
  if (is.null(settings_placement) || !nzchar(settings_placement)) {
    settings_placement <- defaults$settingsPlacement
  }
  settings_placement <- as.character(settings_placement)
  if (!settings_placement %in% c("bottom", "drawer")) {
    settings_placement <- defaults$settingsPlacement
  }
  config$settingsPlacement <- settings_placement

  settings_drawer_position <- config$settingsDrawerPosition
  if (is.null(settings_drawer_position) || !nzchar(settings_drawer_position)) {
    settings_drawer_position <- defaults$settingsDrawerPosition
  }
  settings_drawer_position <- as.character(settings_drawer_position)
  if (!settings_drawer_position %in% c("left", "right")) {
    settings_drawer_position <- defaults$settingsDrawerPosition
  }
  config$settingsDrawerPosition <- settings_drawer_position

  config$showWorkflowGuidanceNotices <- normalize_config_boolean(
    config$showWorkflowGuidanceNotices,
    default = defaults$showWorkflowGuidanceNotices
  )
  config$askGuidedTourOnStartup <- normalize_config_boolean(
    config$askGuidedTourOnStartup,
    default = defaults$askGuidedTourOnStartup
  )
  config$appThemeMode <- normalize_app_theme_mode(config$appThemeMode)

  config
}


# Function to initialize the user config if it doesn't exist
#' @noRd
initialize_config <- function() {
  #config_path <- get_config_path()
  config_path <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "config"), "default-project-config.yml"), , mustWork = FALSE)
  #print(config_path)
  data_path <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data")), mustWork = FALSE)

  if (!file.exists(config_path)) {
    # Create the directory if it doesn't exist
    dir.create(dirname(config_path), recursive = TRUE, showWarnings = TRUE)

    config <- default_app_config(data_path)
    # Write the list to a YAML file
    configr::write.config(config, config_path, write.type = "yaml")

  }

  if (!dir.exists(data_path)) {
    print("No data directory found, creating data directory.")
    dir.create(data_path, recursive = TRUE, showWarnings = TRUE)  # Create the data directory itself
  }

  create_lookup_files <- function(n = 4) {
    # Loop through 1 to n to create lookup files
    for (i in 1:n) {
      # Define the lookup file path
      lookup_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("lookup", i, ".csv")), mustWork = FALSE)

      # Check if the file exists, if not create it
      if (!file.exists(lookup_file)) {
        # Create the dataframe
        df <- data.frame(
          display = paste0("lookup ", i),
          value = paste0("lookup_", i),
          stringsAsFactors = FALSE
        )

        # Write the dataframe to a CSV file
        utils::write.csv(df, file = lookup_file, row.names = FALSE)
        cat("Created:", lookup_file, "\n")
      }
    }
  }

  # Call the function to create 4 lookup files
  create_lookup_files(4)

  lookup_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("username_lookup.csv")), mustWork = FALSE)

  # Check if the file exists, if not create it
  if (!file.exists(lookup_file)) {
    # Create the dataframe
    df <- data.frame(
      user_name = c("Guest Person", "Jane Doh", "Jack Smith"),
      value = c("Guest_Person", "Jane_Doh", "Jack_Smith"),
      stringsAsFactors = FALSE
    )

    # Write the dataframe to a CSV file
    utils::write.csv(df, file = lookup_file, row.names = FALSE)
    cat("Created:", lookup_file, "\n")
  }

  create_help_pdfs <- function(n = 4) {
    # Loop through 1 to n to create PDF files
    for (i in 1:n) {
      # Define the help file path
      help_file <- normalizePath(file.path(tools::R_user_dir("pannotator", which = "data"), paste0("help", i, ".pdf")), mustWork = FALSE)

      # Check if the file exists, if not create it
      if (!file.exists(help_file)) {
        # Create a new PDF file
        grDevices::pdf(file = help_file, width = 8, height = 11)  # Standard letter size

        # Plot the "HELP" text in the center of the page
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste0("HELP ", i), cex = 3, font = 2)  # Centered and large

        # Close the PDF device to save the file
        grDevices::dev.off()

        cat("Created:", help_file, "\n")
      }
    }
  }

  # Call the function to create 4 help PDF files
  create_help_pdfs(4)

}


myEnv <- new.env(parent = emptyenv())

#' @noRd
globalVariables(c("imagefile", "feature_type", "sourcekmz", "."))


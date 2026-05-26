#' Panel layout helpers
#'
#' @noRd

panel_registry <- function() {
  list(
    map = list(
      key = "map",
      title = "Mapping Panel",
      width_field = "mapPanelWidth",
      wrapper_id = "map_panel",
      panel_style = NULL,
      ui = function() {
        mod_leaflet_map_ui("leaflet_map")
      }
    ),
    image = list(
      key = "image",
      title = "Image Panel",
      width_field = "panoPanelWidth",
      wrapper_id = "image_panel",
      panel_style = NULL,
      ui = function() {
        mod_360_image_ui("pano360_image")
      }
    ),
    annotation = list(
      key = "annotation",
      title = "Annotation Panel",
      width_field = "formPanelWidth",
      wrapper_id = "form_panel",
      panel_style = "padding: 20px;",
      ui = function() {
        mod_control_form_ui("control_form")
      }
    ),
    annotation_table = list(
      key = "annotation_table",
      title = "Annotation Table",
      width_field = "annotationTablePanelWidth",
      wrapper_id = "annotation_table_panel",
      panel_style = NULL,
      ui = function() {
        mod_annotation_table_ui("annotation_table")
      }
    )
  )
}


get_panel_order <- function(config = myEnv$config, registry = panel_registry()) {
  enabled_panels <- unique(as.character(unlist(config$enabledPanels)))
  enabled_panels <- enabled_panels[enabled_panels %in% names(registry)]
  if (length(enabled_panels) == 0) {
    enabled_panels <- names(registry)
  }

  panel_order <- unique(as.character(unlist(config$panelOrder)))
  panel_order <- panel_order[panel_order %in% enabled_panels]

  c(panel_order, setdiff(enabled_panels, panel_order))
}


get_panel_width <- function(panel_key, config = myEnv$config, registry = panel_registry()) {
  panel <- registry[[panel_key]]
  if (is.null(panel)) {
    stop("Unknown panel key: ", panel_key, call. = FALSE)
  }

  width_value <- suppressWarnings(as.integer(config[[panel$width_field]]))
  if (is.na(width_value)) {
    width_value <- default_app_config(config$projectFolder)[[panel$width_field]]
  }

  as.integer(width_value)
}


build_panel_slot <- function(panel_key, config = myEnv$config, registry = panel_registry(), extra_class = NULL) {
  panel <- registry[[panel_key]]
  if (is.null(panel)) {
    stop("Unknown panel key: ", panel_key, call. = FALSE)
  }

  div(
    id = paste0("panel-", panel$key),
    class = paste(
      "pannotator-panel-slot",
      paste0("pannotator-panel-slot-", panel$key),
      extra_class
    ),
    wellPanel(
      id = panel$wrapper_id,
      style = panel$panel_style,
      tags$h4(
        panel$title,
        style = "font-size: 13px; text-align: center; margin: 0;"
      ),
      panel$ui()
    )
  )
}


build_panel_column <- function(panel_key, config = myEnv$config, registry = panel_registry()) {
  panel <- registry[[panel_key]]
  if (is.null(panel)) {
    stop("Unknown panel key: ", panel_key, call. = FALSE)
  }

  column(
    width = get_panel_width(panel_key, config = config, registry = registry),
    class = paste("pannotator-panel-column", paste0("pannotator-panel-column-", panel$key)),
    build_panel_slot(panel_key, config = config, registry = registry)
  )
}


split_panel_rows <- function(panel_keys, config = myEnv$config, registry = panel_registry()) {
  rows <- list()
  current_row <- character()
  current_width <- 0L

  for (panel_key in panel_keys) {
    panel_width <- get_panel_width(panel_key, config = config, registry = registry)

    if (length(current_row) > 0 && (current_width + panel_width) > 12L) {
      rows[[length(rows) + 1L]] <- current_row
      current_row <- character()
      current_width <- 0L
    }

    current_row <- c(current_row, panel_key)
    current_width <- current_width + panel_width

    if (current_width >= 12L) {
      rows[[length(rows) + 1L]] <- current_row
      current_row <- character()
      current_width <- 0L
    }
  }

  if (length(current_row) > 0) {
    rows[[length(rows) + 1L]] <- current_row
  }

  rows
}


build_panel_row <- function(panel_keys, config = myEnv$config, registry = panel_registry()) {
  fluidRow(
    class = "pannotator-panel-row",
    lapply(
      panel_keys,
      function(panel_key) {
        build_panel_column(panel_key, config = config, registry = registry)
      }
    )
  )
}


workspace_panel_style <- function(config = myEnv$config, registry = panel_registry()) {
  annotation_width <- if ("annotation" %in% names(registry)) {
    get_panel_width("annotation", config = config, registry = registry)
  } else {
    2L
  }
  annotation_width <- max(2L, min(4L, annotation_width))
  main_width <- 12L - annotation_width

  paste0(
    "--pannotator-map-fr: ", get_panel_width("map", config = config, registry = registry), ";",
    "--pannotator-image-fr: ", get_panel_width("image", config = config, registry = registry), ";",
    "--pannotator-main-fr: ", main_width, ";",
    "--pannotator-annotation-fr: ", annotation_width, ";",
    "grid-template-columns: minmax(0, ", main_width, "fr) minmax(280px, ", annotation_width, "fr);"
  )
}


build_workspace_top_ui <- function(panel_keys, config = myEnv$config, registry = panel_registry()) {
  if (length(panel_keys) == 0) {
    return(NULL)
  }

  grid_columns <- vapply(
    panel_keys,
    function(panel_key) {
      paste0("minmax(0, ", get_panel_width(panel_key, config = config, registry = registry), "fr)")
    },
    character(1)
  )

  div(
    class = "pannotator-workspace-top",
    style = paste0("grid-template-columns: ", paste(grid_columns, collapse = " "), ";"),
    lapply(
      panel_keys,
      function(panel_key) {
        build_panel_slot(panel_key, config = config, registry = registry)
      }
    )
  )
}


build_panel_host_ui <- function(config = myEnv$config, registry = panel_registry(), settings_ui = NULL) {
  panel_order <- get_panel_order(config = config, registry = registry)
  top_panel_keys <- panel_order[panel_order %in% c("map", "image")]
  annotation_enabled <- "annotation" %in% panel_order
  table_enabled <- "annotation_table" %in% panel_order
  extra_panel_keys <- panel_order[!panel_order %in% c("map", "image", "annotation", "annotation_table")]

  left_workspace_children <- list(
    build_workspace_top_ui(top_panel_keys, config = config, registry = registry)
  )

  if (isTRUE(table_enabled)) {
    left_workspace_children[[length(left_workspace_children) + 1L]] <- build_panel_slot(
      "annotation_table",
      config = config,
      registry = registry,
      extra_class = "pannotator-workspace-table"
    )
  }

  if (!is.null(settings_ui)) {
    left_workspace_children[[length(left_workspace_children) + 1L]] <- div(
      id = "panel-settings",
      class = "pannotator-workspace-settings",
      settings_ui
    )
  }

  if (length(extra_panel_keys) > 0) {
    left_workspace_children[[length(left_workspace_children) + 1L]] <- div(
      class = "pannotator-workspace-extra-panels",
      lapply(
        extra_panel_keys,
        function(panel_key) {
          build_panel_slot(panel_key, config = config, registry = registry)
        }
      )
    )
  }

  left_workspace_children <- Filter(Negate(is.null), left_workspace_children)

  tagList(
    div(
      class = paste(
        "pannotator-workspace-layout",
        if (isTRUE(annotation_enabled)) {
          "pannotator-workspace-layout-with-annotation"
        } else {
          "pannotator-workspace-layout-without-annotation"
        }
      ),
      style = if (isTRUE(annotation_enabled)) {
        workspace_panel_style(config = config, registry = registry)
      } else {
        "grid-template-columns: minmax(0, 1fr);"
      },
      div(
        class = "pannotator-workspace-main",
        left_workspace_children
      ),
      if (isTRUE(annotation_enabled)) {
        div(
          class = "pannotator-workspace-annotation",
          build_panel_slot("annotation", config = config, registry = registry)
        )
      }
    )
  )
}


panel_status_message <- function(message) {
  div(
    class = "alert alert-info pannotator-panel-status",
    role = "status",
    message
  )
}


panel_empty_state <- function(title, message) {
  div(
    class = "pannotator-empty-state",
    tags$h4(title, style = "margin-top: 0;"),
    tags$p(message, style = "margin-bottom: 0;")
  )
}

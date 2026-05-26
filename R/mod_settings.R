#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

settings_theme_choices <- function() {
  available_themes <- tryCatch(
    bslib::bootswatch_themes(version = 5),
    error = function(e) character()
  )

  preferred_themes <- c(
    "cerulean", "cosmo", "cyborg", "darkly", "flatly",
    "journal", "litera", "lumen", "lux", "materia",
    "minty", "morph", "pulse", "quartz", "sandstone",
    "simplex", "sketchy", "slate", "solar", "spacelab",
    "superhero", "united", "vapor", "yeti", "zephyr"
  )

  intersect(preferred_themes, available_themes)
}


workspace_reload_setting_fields <- function() {
  c(
    "mapPanelSource",
    "mapIconColour",
    "mapMarkerColour",
    "mapPolygonStroke",
    "mapPolygonStrokeColour",
    "mapPolygonStrokeWeight",
    "mapPolygonStrokeOpacity",
    "mapPolygonFill",
    "mapPolygonFillColour",
    "mapPolygonFillOpacity",
    "pano360IconColour",
    "pano360MarkerColour",
    "pano360PolygonStroke",
    "pano360PolygonStrokeColour",
    "pano360PolygonStrokeWeight",
    "pano360PolygonStrokeOpacity",
    "pano360PolygonFill",
    "pano360PolygonFillColour",
    "pano360PolygonFillOpacity"
  )
}


setting_requires_workspace_reload <- function(config_key) {
  config_key %in% workspace_reload_setting_fields()
}


settings_placement_choices <- function() {
  c(
    "Below Main Panels" = "bottom",
    "Settings Drawer" = "drawer"
  )
}


settings_placement_value <- function(config = myEnv$config) {
  placement <- config$settingsPlacement

  if (is.null(placement) || !placement %in% c("bottom", "drawer")) {
    return("bottom")
  }

  placement
}


settings_drawer_position_choices <- function() {
  c(
    "Left" = "left",
    "Right" = "right"
  )
}


settings_drawer_position_value <- function(config = myEnv$config) {
  drawer_position <- config$settingsDrawerPosition

  if (is.null(drawer_position) || !drawer_position %in% c("left", "right")) {
    return("right")
  }

  drawer_position
}


layout_schematic_box <- function(label, extra_class = NULL) {
  tags$div(
    class = paste("pannotator-layout-box", extra_class),
    span(label)
  )
}


settings_layout_choice_click_js <- function(placement, ns = identity) {
  input_name <- htmltools::htmlEscape(ns("settingsPlacement"), attribute = TRUE)
  input_value <- htmltools::htmlEscape(placement, attribute = TRUE)

  paste0(
    "(function() {",
    "var target = document.querySelector('input[name=\"", input_name, "\"][value=\"", input_value, "\"]');",
    "if (target) { target.click(); }",
    "})();"
  )
}


settings_layout_choice_card <- function(
    title,
    description,
    placement,
    selected_placement,
    drawer_position = "right",
    ns = identity
) {
  click_js <- settings_layout_choice_click_js(placement = placement, ns = ns)

  schematic <- switch(
    placement,
    bottom = div(
      class = "pannotator-layout-swatch pannotator-layout-swatch-bottom",
      div(
        class = "pannotator-layout-swatch-workspace",
        div(
          class = "pannotator-layout-swatch-workspace-main",
          div(
            class = "pannotator-layout-swatch-row",
            layout_schematic_box("Map"),
            layout_schematic_box("Image")
          ),
          layout_schematic_box("Table"),
          layout_schematic_box("Settings", extra_class = "pannotator-layout-box-accent")
        ),
        layout_schematic_box("Annotation", extra_class = "pannotator-layout-box-tall")
      )
    ),
    drawer = div(
      class = paste(
        "pannotator-layout-swatch",
        "pannotator-layout-swatch-drawer",
        paste0("pannotator-layout-swatch-drawer-", drawer_position)
      ),
      if (identical(drawer_position, "left")) {
        tagList(
          div(
            class = "pannotator-layout-swatch-side",
            layout_schematic_box("Settings", extra_class = "pannotator-layout-box-tall pannotator-layout-box-accent")
          ),
          div(
            class = "pannotator-layout-swatch-main",
            div(
              class = "pannotator-layout-swatch-workspace",
              div(
                class = "pannotator-layout-swatch-workspace-main",
                div(
                  class = "pannotator-layout-swatch-row",
                  layout_schematic_box("Map"),
                  layout_schematic_box("Image")
                ),
                layout_schematic_box("Table")
              ),
              layout_schematic_box("Annotation", extra_class = "pannotator-layout-box-tall")
            )
          )
        )
      } else {
        tagList(
          div(
            class = "pannotator-layout-swatch-main",
            div(
              class = "pannotator-layout-swatch-workspace",
              div(
                class = "pannotator-layout-swatch-workspace-main",
                div(
                  class = "pannotator-layout-swatch-row",
                  layout_schematic_box("Map"),
                  layout_schematic_box("Image")
                ),
                layout_schematic_box("Table")
              ),
              layout_schematic_box("Annotation", extra_class = "pannotator-layout-box-tall")
            )
          ),
          div(
            class = "pannotator-layout-swatch-side",
            layout_schematic_box("Settings", extra_class = "pannotator-layout-box-tall pannotator-layout-box-accent")
          )
        )
      }
    )
  )

  div(
    class = paste(
      "pannotator-layout-choice",
      "pannotator-layout-choice-clickable",
      if (identical(placement, selected_placement)) {
        "pannotator-layout-choice-active"
      } else {
        ""
      }
    ),
    role = "button",
    tabindex = "0",
    `aria-label` = paste("Select", title),
    `aria-pressed` = tolower(as.character(identical(placement, selected_placement))),
    onclick = click_js,
    onkeydown = paste0(
      "if (event.key === 'Enter' || event.key === ' ') {",
      "event.preventDefault();",
      click_js,
      "}"
    ),
    tags$h4(title, style = "margin-top: 0; margin-bottom: 6px;"),
    tags$p(description, class = "text-muted", style = "margin-bottom: 10px;"),
    schematic
  )
}


settings_layout_schematic_ui <- function(
    selected_placement = "bottom",
    drawer_position = "right",
    ns = identity
) {
  div(
    class = "pannotator-layout-choice-grid",
    settings_layout_choice_card(
      title = "Below the Workspace",
      description = "Best when you want the widest settings tables and lookup editors.",
      placement = "bottom",
      selected_placement = selected_placement,
      ns = ns
    ),
    settings_layout_choice_card(
      title = "Settings Drawer",
      description = "Keeps the main workspace cleaner and can slide in from the left or right.",
      placement = "drawer",
      selected_placement = selected_placement,
      drawer_position = drawer_position,
      ns = ns
    )
  )
}


settings_tab_section_style <- function(display_mode = "bottom") {
  if (identical(display_mode, "drawer")) {
    return("overflow: visible; padding-right: 0;")
  }

  "max-height: 560px; overflow-y: auto; overflow-x: hidden; padding-right: 8px;"
}


settings_main_accordion_open_values <- function() {
  c(
    "system_dependencies",
    "project_settings",
    "layout_settings",
    "mapping_panel_settings",
    "image_panel_settings",
    "annotation_panel_settings",
    "annotation_table_settings"
  )
}


settings_csv_editor_specs <- function(config = myEnv$config) {
  list(
    username = list(
      key = "username",
      title = "Username Table",
      file_input_id = "usernameLookupFile",
      config_key = "usernameLookupFile",
      default_file_name = "username_lookup.csv",
      output_id = "username_lookup_editor",
      table_output_id = "username_lookup_table",
      columns = c("user_name", "value"),
      column_labels = c(user_name = "User Name", value = "Stored Value")
    ),
    lookup1 = list(
      key = "lookup1",
      title = "Lookup 1 Table",
      file_input_id = "lookup1CsvFile",
      config_key = "lookup1CsvFile",
      default_file_name = "lookup1.csv",
      output_id = "lookup1_csv_editor",
      table_output_id = "lookup1_csv_table",
      columns = c("display", "value"),
      column_labels = c(display = "Display", value = "Value")
    ),
    lookup2 = list(
      key = "lookup2",
      title = "Lookup 2 Table",
      file_input_id = "lookup2CsvFile",
      config_key = "lookup2CsvFile",
      default_file_name = "lookup2.csv",
      output_id = "lookup2_csv_editor",
      table_output_id = "lookup2_csv_table",
      columns = c("display", "value"),
      column_labels = c(display = "Display", value = "Value")
    ),
    lookup3 = list(
      key = "lookup3",
      title = "Lookup 3 Table",
      file_input_id = "lookup3CsvFile",
      config_key = "lookup3CsvFile",
      default_file_name = "lookup3.csv",
      output_id = "lookup3_csv_editor",
      table_output_id = "lookup3_csv_table",
      columns = c("display", "value"),
      column_labels = c(display = "Display", value = "Value")
    ),
    lookup4 = list(
      key = "lookup4",
      title = "Lookup 4 Table",
      file_input_id = "lookup4CsvFile",
      config_key = "lookup4CsvFile",
      default_file_name = "lookup4.csv",
      output_id = "lookup4_csv_editor",
      table_output_id = "lookup4_csv_table",
      columns = c("display", "value"),
      column_labels = c(display = "Display", value = "Value")
    )
  )
}


settings_csv_editor_ui <- function(ns, spec_key, config = myEnv$config) {
  spec <- settings_csv_editor_specs(config = config)[[spec_key]]
  if (is.null(spec)) {
    stop("Unknown CSV editor spec: ", spec_key, call. = FALSE)
  }

  button_label <- paste("Edit", spec$title)

  div(
    class = "pannotator-settings-csv-editor",
    actionButton(
      inputId = ns(settings_csv_editor_button_id(spec)),
      label = button_label,
      class = "btn btn-outline-secondary btn-sm"
    ),
    tags$small(
      "Load the editable table when you want to review or change this CSV in place.",
      class = "text-muted"
    ),
    uiOutput(ns(spec$output_id))
  )
}


settings_csv_editor_button_id <- function(spec) {
  paste0(spec$key, "_load_editor")
}


settings_csv_editor_file_name <- function(spec, config = myEnv$config) {
  file_name <- config[[spec$config_key]]

  if (is.null(file_name) || !nzchar(file_name)) {
    file_name <- spec$default_file_name
  }

  file_name
}


settings_csv_editor_path <- function(spec, config = myEnv$config, data_dir = myEnv$data_dir) {
  normalizePath(
    file.path(data_dir, settings_csv_editor_file_name(spec, config = config)),
    mustWork = FALSE
  )
}


normalize_settings_csv_editor_data <- function(data, columns) {
  if (is.null(data)) {
    data <- data.frame(stringsAsFactors = FALSE)
  }

  normalized_data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  for (column_name in columns) {
    if (!column_name %in% names(normalized_data)) {
      normalized_data[[column_name]] <- ""
    }
  }

  normalized_data <- normalized_data[, columns, drop = FALSE]
  normalized_data[] <- lapply(normalized_data, function(column_values) {
    values <- as.character(column_values)
    values[is.na(values)] <- ""
    trimws(values)
  })

  if (nrow(normalized_data) > 0) {
    keep_rows <- rowSums(normalized_data != "") > 0
    normalized_data <- normalized_data[keep_rows, , drop = FALSE]
  }

  rownames(normalized_data) <- NULL
  normalized_data
}


read_settings_csv_editor_data <- function(spec, config = myEnv$config, data_dir = myEnv$data_dir) {
  file_path <- settings_csv_editor_path(spec, config = config, data_dir = data_dir)

  if (!file.exists(file_path)) {
    return(normalize_settings_csv_editor_data(data.frame(stringsAsFactors = FALSE), spec$columns))
  }

  file_data <- tryCatch(
    utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )

  normalize_settings_csv_editor_data(file_data, spec$columns)
}


write_settings_csv_editor_data <- function(data, spec, config = myEnv$config, data_dir = myEnv$data_dir) {
  normalized_data <- normalize_settings_csv_editor_data(data, spec$columns)
  utils::write.csv(
    normalized_data,
    file = settings_csv_editor_path(spec, config = config, data_dir = data_dir),
    row.names = FALSE
  )

  normalized_data
}


build_settings_csv_editor_hot <- function(table_data, spec) {
  hot <- rhandsontable::rhandsontable(
    table_data,
    stretchH = "all",
    rowHeaders = NULL,
    width = "100%",
    height = 240
  )
  hot <- rhandsontable::hot_table(
    hot,
    contextMenu = TRUE,
    minSpareRows = 1,
    highlightCol = TRUE,
    highlightRow = TRUE,
    manualColumnResize = TRUE
  )

  for (column_name in spec$columns) {
    hot <- rhandsontable::hot_col(
      hot,
      column_name,
      title = spec$column_labels[[column_name]],
      type = "text"
    )
  }

  hot
}


lookup_accordion_open_values <- function(config = myEnv$config) {
  open_values <- "lookup1"

  if (isTRUE(config$lookup2Enabled)) {
    open_values <- c(open_values, "lookup2")
  }
  if (isTRUE(config$lookup3Enabled)) {
    open_values <- c(open_values, "lookup3")
  }
  if (isTRUE(config$lookup4Enabled)) {
    open_values <- c(open_values, "lookup4")
  }

  open_values
}


lookup_settings_panel_ui <- function(ns, lookup_index, enable_toggle = FALSE, config = myEnv$config) {
  lookup_key <- paste0("lookup", lookup_index)
  config <- merge_panel_config(config, data_path = config$projectFolder)
  spec <- settings_csv_editor_specs(config = config)[[lookup_key]]

  tagList(
    if (isTRUE(enable_toggle)) {
      checkboxInput(
        inputId = ns(paste0(lookup_key, "Enabled")),
        label = paste0("Enable Lookup ", lookup_index),
        width = "95%",
        value = config[[paste0(lookup_key, "Enabled")]]
      )
    },
    textInput(
      inputId = ns(paste0(lookup_key, "Label")),
      label = paste0("Lookup ", lookup_index, " Label"),
      value = paste0(config[[paste0(lookup_key, "Label")]]),
      width = "95%"
    ) |> shinyhelper::helper(type = "markdown", content = "lookup_label_help", icon = "question-circle", size = "m"),
    fileInput(
      inputId = ns(paste0(lookup_key, "CsvFile")),
      label = paste0("Lookup ", lookup_index, " csv File"),
      multiple = FALSE,
      accept = ".csv",
      width = "95%",
      buttonLabel = paste0("Lookup ", lookup_index, " csv File..."),
      placeholder = paste0(config[[paste0(lookup_key, "CsvFile")]]),
      capture = NULL
    ) |> shinyhelper::helper(type = "markdown", content = "lookup_csv_help", icon = "question-circle", size = "m"),
    settings_csv_editor_ui(ns, lookup_key, config = config),
    fileInput(
      inputId = ns(paste0(lookup_key, "HelpFile")),
      label = paste0("Lookup ", lookup_index, " Help File"),
      multiple = FALSE,
      accept = ".pdf",
      width = "95%",
      buttonLabel = paste0("Lookup ", lookup_index, " Help File..."),
      placeholder = paste0(config[[paste0(lookup_key, "HelpFile")]]),
      capture = NULL
    ) |> shinyhelper::helper(type = "markdown", content = "lookup_pdf_help", icon = "question-circle", size = "m")
  )
}


mod_settings_ui <- function(
    id,
    config = myEnv$config,
    display_mode = settings_placement_value(config)
) {
  ns <- NS(id)
  config <- merge_panel_config(config, data_path = config$projectFolder)
  display_mode <- match.arg(display_mode, c("bottom", "drawer"))

  tagList(
    tags$div(
      id = ns("settings_panel"),
      class = paste(
        "pannotator-settings-shell",
        if (identical(display_mode, "drawer")) {
          "pannotator-settings-shell-drawer"
        } else {
          "pannotator-settings-shell-bottom"
        }
      ),
      bslib::card(
        full_screen = FALSE,
        bslib::card_header(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 12px;",
            div(
              tags$h3("Settings", style = "margin: 0;"),
              tags$small(
                "Change layout, lookups, and app preferences.",
                style = "display: block; color: #586069;"
              )
            ),
            div(
              style = "display: flex; gap: 8px; flex-wrap: wrap; justify-content: flex-end;",
              if (identical(display_mode, "bottom")) {
                uiOutput(ns("toggle_settings_button"))
              },
              actionButton(
                inputId = ns("start_tour"),
                label = "Guided Tour",
                icon = icon("map-signs"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          )
        ),
        div(
          id = ns("settings_body"),
          class = "pannotator-settings-body",
          bslib::card_body(
            div(
              class = "pannotator-settings-global",
              div(
                class = "pannotator-settings-global-actions",
                div(
                  class = "pannotator-settings-global-toggle",
                  checkboxInput(
                    inputId = ns("showWorkflowGuidanceNotices"),
                    label = "Show workflow guidance notices",
                    width = "100%",
                    value = config$showWorkflowGuidanceNotices
                  ),
                  checkboxInput(
                    inputId = ns("askGuidedTourOnStartup"),
                    label = "Ask for guided tour on startup",
                    width = "100%",
                    value = config$askGuidedTourOnStartup
                  )
                ),
                div(
                  class = "pannotator-settings-global-buttons",
                  actionButton(
                    inputId = ns("clearAllButton"),
                    label = "Clear All Annotations Data",
                    class = "btn btn-outline-danger"
                  ),
                  actionButton(
                    inputId = ns("applySettingsButton"),
                    label = "Apply Changes",
                    class = "btn btn-primary"
                  )
                )
              ),
              uiOutput(ns("settings_notice")),
              uiOutput(ns("settings_pending_action"))
            ),
            tabsetPanel(
              id = ns("settings_tabs"),
              selected = "main",
              tabPanel(
                title = "Main Settings",
                value = "main",
                settings_main_tab_ui(ns, config = config, display_mode = display_mode)
              ),
              tabPanel(
                title = "Lookups",
                value = "lookups",
                settings_lookups_tab_ui(ns, config = config, display_mode = display_mode)
              ),
              tabPanel(
                title = "About This Software",
                value = "about",
                settings_about_tab_ui(ns, display_mode = display_mode)
              )
            )
          )
        )
      )
    )
  )
}


settings_system_dependencies_section_ui <- function(ns) {
  div(
    id = ns("settings_system_dependencies_section"),
    uiOutput(ns("exiftool_status_ui"))
  )
}


settings_project_settings_section_ui <- function(ns) {
  div(
    id = ns("settings_project_settings_section"),
    fluidRow(
      column(
        8,
        tags$p(
          tagList(
            "Export a complete project folder containing the reusable YAML, lookup CSVs, help files, and annotations RDS. The YAML can be used for project launches such as ",
            tags$code('run_app(projectSettingsFile = "C:/path/to/project.yml")'),
            "."
          ),
          style = "margin-bottom: 12px;"
        )
      ),
      column(
        4,
        shinyFiles::shinySaveButton(
          id = ns("export_project_settings"),
          label = "Export All Project Files",
          title = "Save Project YAML And Supporting Files",
          filename = default_project_settings_export_name(),
          class = "btn btn-outline-primary",
          icon = icon("download"),
          style = "float: right; margin-bottom: 12px;"
        )
      )
    ),
    fluidRow(
      column(
        12,
        passwordInput(
          inputId = ns("projectGoogleMapsApiKey"),
          label = "Google Maps API Key For Exported Project YAML (optional)",
          width = "95%",
          value = ""
        ),
        tags$p(
          "If you enter a key here, the exported YAML will be written with mapPanelSource: Google.Maps and the API key. The key is not saved to your regular app settings.",
          class = "text-muted",
          style = "margin-top: -4px; margin-bottom: 16px;"
        )
      )
    )
  )
}


settings_layout_settings_section_ui <- function(
    ns,
    config = myEnv$config,
    display_mode = settings_placement_value(config)
) {
  div(
    id = ns("settings_layout_section"),
    fluidRow(
      column(
        12,
        radioButtons(
          inputId = ns("settingsPlacement"),
          label = "Settings Panel Placement",
          choices = settings_placement_choices(),
          selected = settings_placement_value(config),
          inline = TRUE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("settingsPlacement"), "'] === 'drawer'"),
          radioButtons(
            inputId = ns("settingsDrawerPosition"),
            label = "Drawer Side",
            choices = settings_drawer_position_choices(),
            selected = settings_drawer_position_value(config),
            inline = TRUE
          )
        ),
        tags$p(
          if (identical(display_mode, "drawer")) {
            paste0(
              "Drawer mode is active now from the ",
              settings_drawer_position_value(config),
              ". Click Apply Changes after selecting a new layout to reload the app into that shell."
            )
          } else {
            "Below-panels mode is active now. Choose the drawer if you want settings to slide in from either side."
          },
          class = "text-muted",
          style = "margin-top: -8px; margin-bottom: 12px;"
        ),
        uiOutput(ns("layout_schematic"))
      )
    ),
    selectInput(
      inputId = ns("appTheme"),
      label = "App Theme",
      width = "95%",
      selected = config$appTheme,
      choices = settings_theme_choices(),
      selectize = FALSE
    ),
    div(
      class = "pannotator-theme-mode-control",
      shinyWidgets::switchInput(
        inputId = ns("appThemeMode"),
        label = "Light / Dark Mode",
        value = identical(normalize_app_theme_mode(config$appThemeMode), "dark"),
        onLabel = "Dark",
        offLabel = "Light",
        onStatus = "primary",
        offStatus = "light",
        size = "small",
        labelWidth = "140px",
        handleWidth = "70px",
        width = "95%"
      ),
      tags$p(
        "Choose the appearance mode, then click Apply Changes to update the app theme.",
        class = "text-muted",
        style = "margin-top: -4px; margin-bottom: 16px;"
      )
    ),
    fluidRow(
      column(
        4,
        sliderInput(
          inputId = ns("mapPanelWidth"),
          label = "Mapping Panel Width",
          min = 3,
          max = 6,
          value = config$mapPanelWidth,
          step = 1
        )
      ),
      column(
        4,
        sliderInput(
          inputId = ns("panoPanelWidth"),
          label = "Image Panel Width",
          min = 3,
          max = 6,
          value = config$panoPanelWidth,
          step = 1
        )
      ),
      column(
        4,
        sliderInput(
          inputId = ns("formPanelWidth"),
          label = "Annotation Panel Width",
          min = 2,
          max = 4,
          value = config$formPanelWidth,
          step = 1
        )
      )
    )
  )
}


settings_mapping_panel_section_ui <- function(ns, config = myEnv$config) {
  map_source_choices <- list(
    "Esri WorldImagery" = "Esri.WorldImagery",
    "Esri WorldTopoMap" = "Esri.WorldTopoMap",
    "Esri WorldStreetMap" = "Esri.WorldStreetMap",
    "Open StreetMap" = "OpenStreetMap",
    "Open TopoMap" = "OpenTopoMap"
  )

  if (identical(config$mapPanelSource, "Google.Maps")) {
    map_source_choices <- c(
      "Google Maps (project YAML)" = "Google.Maps",
      map_source_choices
    )
  }

  div(
    id = ns("settings_mapping_panel_section"),
    selectInput(
      inputId = ns("mapPanelSource"),
      label = "Leaflet Map Source",
      width = "95%",
      selected = config$mapPanelSource,
      choices = map_source_choices
    ),
    tags$p(
      "If you need a Google Maps base map, use the Project Settings export field above to write an export-only API key into a project YAML. The regular app settings do not store that key.",
      class = "text-muted",
      style = "margin-top: -8px; margin-bottom: 12px;"
    ),
    fluidRow(
      column(
        6,
        colourpicker::colourInput(
          inputId = ns("mapIconColour"),
          label = "Map Icon Colour",
          palette = "limited",
          showColour = "background",
          returnName = TRUE,
          closeOnClick = TRUE,
          allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
          value = config$mapIconColour
        )
      ),
      column(
        6,
        colourpicker::colourInput(
          inputId = ns("mapMarkerColour"),
          label = "Map Marker Background Colour",
          palette = "limited",
          showColour = "background",
          returnName = TRUE,
          closeOnClick = TRUE,
          allowedCols = c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "lightgray", "black"),
          value = config$mapMarkerColour
        )
      )
    ),
    checkboxInput(
      inputId = ns("mapPolygonStroke"),
      label = "Map Polygon Stroke",
      width = "95%",
      value = config$mapPolygonStroke
    ),
    conditionalPanel(
      condition = paste0("input['", ns("mapPolygonStroke"), "']"),
      div(
        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
        fluidRow(
          column(
            2,
            colourpicker::colourInput(
              inputId = ns("mapPolygonStrokeColour"),
              label = "Stroke Colour",
              palette = "limited",
              showColour = "background",
              returnName = TRUE,
              closeOnClick = TRUE,
              allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
              value = config$mapPolygonStrokeColour
            )
          ),
          column(
            5,
            sliderInput(
              inputId = ns("mapPolygonStrokeWeight"),
              label = "Stroke Weight",
              min = 1,
              max = 6,
              value = config$mapPolygonStrokeWeight,
              step = 1
            )
          ),
          column(
            5,
            sliderInput(
              inputId = ns("mapPolygonStrokeOpacity"),
              label = "Stroke Opacity",
              min = 0.1,
              max = 1,
              value = config$mapPolygonStrokeOpacity,
              step = 0.1
            )
          )
        )
      )
    ),
    checkboxInput(
      inputId = ns("mapPolygonFill"),
      label = "Map Polygon Fill",
      width = "95%",
      value = config$mapPolygonFill
    ),
    conditionalPanel(
      condition = paste0("input['", ns("mapPolygonFill"), "']"),
      div(
        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
        fluidRow(
          column(
            4,
            colourpicker::colourInput(
              inputId = ns("mapPolygonFillColour"),
              label = "Fill Colour",
              palette = "limited",
              showColour = "background",
              returnName = TRUE,
              closeOnClick = TRUE,
              allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
              value = config$mapPolygonFillColour
            )
          ),
          column(
            8,
            sliderInput(
              inputId = ns("mapPolygonFillOpacity"),
              label = "Fill Opacity",
              min = 0.1,
              max = 1,
              value = config$mapPolygonFillOpacity,
              step = 0.1
            )
          )
        )
      )
    )
  )
}


settings_image_panel_section_ui <- function(ns, config = myEnv$config) {
  div(
    id = ns("settings_image_panel_section"),
    fluidRow(
      column(
        6,
        colourpicker::colourInput(
          inputId = ns("pano360IconColour"),
          label = "Image Icon Colour",
          palette = "limited",
          showColour = "background",
          returnName = TRUE,
          closeOnClick = TRUE,
          allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
          value = config$pano360IconColour
        )
      ),
      column(
        6,
        colourpicker::colourInput(
          inputId = ns("pano360MarkerColour"),
          label = "Image Marker Background Colour",
          palette = "limited",
          showColour = "background",
          returnName = TRUE,
          closeOnClick = TRUE,
          allowedCols = c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "lightgray", "black"),
          value = config$pano360MarkerColour
        )
      )
    ),
    checkboxInput(
      inputId = ns("pano360PolygonStroke"),
      label = "Image Polygon Stroke",
      width = "95%",
      value = config$pano360PolygonStroke
    ),
    conditionalPanel(
      condition = paste0("input['", ns("pano360PolygonStroke"), "']"),
      div(
        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
        fluidRow(
          column(
            2,
            colourpicker::colourInput(
              inputId = ns("pano360PolygonStrokeColour"),
              label = "Stroke Colour",
              palette = "limited",
              showColour = "background",
              returnName = TRUE,
              closeOnClick = TRUE,
              allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
              value = config$pano360PolygonStrokeColour
            )
          ),
          column(
            5,
            sliderInput(
              inputId = ns("pano360PolygonStrokeWeight"),
              label = "Stroke Weight",
              min = 1,
              max = 6,
              value = config$pano360PolygonStrokeWeight,
              step = 1
            )
          ),
          column(
            5,
            sliderInput(
              inputId = ns("pano360PolygonStrokeOpacity"),
              label = "Stroke Opacity",
              min = 0.1,
              max = 1,
              value = config$pano360PolygonStrokeOpacity,
              step = 0.1
            )
          )
        ),
        checkboxInput(
          inputId = ns("showPano360PolygonStrokeInCropExport"),
          label = "Show Polygon Stroke In Cropped Image Export",
          width = "95%",
          value = config$showPano360PolygonStrokeInCropExport
        )
      )
    ),
    checkboxInput(
      inputId = ns("pano360PolygonFill"),
      label = "Image Polygon Fill",
      width = "95%",
      value = config$pano360PolygonFill
    ),
    conditionalPanel(
      condition = paste0("input['", ns("pano360PolygonFill"), "']"),
      div(
        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
        fluidRow(
          column(
            4,
            colourpicker::colourInput(
              inputId = ns("pano360PolygonFillColour"),
              label = "Fill Colour",
              palette = "limited",
              showColour = "background",
              returnName = TRUE,
              closeOnClick = TRUE,
              allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
              value = config$pano360PolygonFillColour
            )
          ),
          column(
            8,
            sliderInput(
              inputId = ns("pano360PolygonFillOpacity"),
              label = "Fill Opacity",
              min = 0.1,
              max = 1,
              value = config$pano360PolygonFillOpacity,
              step = 0.1
            )
          )
        ),
        checkboxInput(
          inputId = ns("showPano360PolygonFillInCropExport"),
          label = "Show Polygon Fill In Cropped Image Export",
          width = "95%",
          value = config$showPano360PolygonFillInCropExport
        )
      )
    )
  )
}


settings_annotation_panel_section_ui <- function(ns, config = myEnv$config) {
  div(
    id = ns("settings_annotation_panel_section"),
    fileInput(
      inputId = ns("usernameLookupFile"),
      label = "Username File",
      multiple = FALSE,
      accept = ".csv",
      width = "95%",
      buttonLabel = "Browse...",
      placeholder = paste0(config$usernameLookupFile),
      capture = NULL
    ) |> shinyhelper::helper(type = "markdown", content = "user_name_csv_help", icon = "question-circle", size = "m"),
    settings_csv_editor_ui(ns, "username", config = config),
    selectInput(
      inputId = ns("exportFileFormat"),
      label = "Export File Format",
      width = "95%",
      selected = config$exportFileFormat,
      choices = list("csv" = "csv", "rds" = "rds")
    )
  )
}


settings_annotation_table_section_ui <- function(ns) {
  div(
    id = ns("settings_annotation_table_section"),
    tags$p(
      "The annotation table spans the full row below the main workspace and uses the enabled lookup columns and labels from the Lookups tab. Install rhandsontable to enable in-place table editing.",
      class = "text-muted",
      style = "margin-bottom: 0;"
    )
  )
}


guided_tour_startup_prompt_ui <- function(ns) {
  modalDialog(
    title = "Guided Tour",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Not Now"),
      actionButton(
        inputId = ns("start_startup_tour"),
        label = "Start Guided Tour",
        icon = icon("map-signs"),
        class = "btn btn-primary"
      )
    ),
    tags$p("Would you like a guided tour of the pannotator interface?"),
    tags$p(
      "You can also start the tour later from the Settings panel.",
      class = "text-muted",
      style = "margin-bottom: 0;"
    )
  )
}


settings_main_tab_ui <- function(
    ns,
    config = myEnv$config,
    display_mode = settings_placement_value(config)
) {
  config <- merge_panel_config(config, data_path = config$projectFolder)

  div(
    id = ns("settings_main_section"),
    style = settings_tab_section_style(display_mode),
    bslib::accordion(
      id = ns("main_settings_accordion"),
      multiple = TRUE,
      open = settings_main_accordion_open_values(),
      bslib::accordion_panel(
        title = "System Dependencies",
        value = "system_dependencies",
        settings_system_dependencies_section_ui(ns)
      ),
      bslib::accordion_panel(
        title = "Project Settings",
        value = "project_settings",
        settings_project_settings_section_ui(ns)
      ),
      bslib::accordion_panel(
        title = "Layout Settings",
        value = "layout_settings",
        settings_layout_settings_section_ui(
          ns,
          config = config,
          display_mode = display_mode
        )
      ),
      bslib::accordion_panel(
        title = "Mapping Panel Settings",
        value = "mapping_panel_settings",
        settings_mapping_panel_section_ui(ns, config = config)
      ),
      bslib::accordion_panel(
        title = "Image Panel Settings",
        value = "image_panel_settings",
        settings_image_panel_section_ui(ns, config = config)
      ),
      bslib::accordion_panel(
        title = "Annotation Panel Settings",
        value = "annotation_panel_settings",
        settings_annotation_panel_section_ui(ns, config = config)
      ),
      bslib::accordion_panel(
        title = "Annotation Table Settings",
        value = "annotation_table_settings",
        settings_annotation_table_section_ui(ns)
      )
    )
  )
}


settings_lookups_tab_ui <- function(
    ns,
    config = myEnv$config,
    display_mode = settings_placement_value(config)
) {
  config <- merge_panel_config(config, data_path = config$projectFolder)

  div(
    id = ns("settings_lookups_section"),
    style = settings_tab_section_style(display_mode),
    bslib::accordion(
      id = ns("lookup_accordion"),
      multiple = TRUE,
      open = lookup_accordion_open_values(config = config),
      bslib::accordion_panel(
        title = "Lookup 1",
        value = "lookup1",
        lookup_settings_panel_ui(ns, 1, enable_toggle = FALSE, config = config)
      ),
      bslib::accordion_panel(
        title = "Lookup 2",
        value = "lookup2",
        lookup_settings_panel_ui(ns, 2, enable_toggle = TRUE, config = config)
      ),
      bslib::accordion_panel(
        title = "Lookup 3",
        value = "lookup3",
        lookup_settings_panel_ui(ns, 3, enable_toggle = TRUE, config = config)
      ),
      bslib::accordion_panel(
        title = "Lookup 4",
        value = "lookup4",
        lookup_settings_panel_ui(ns, 4, enable_toggle = TRUE, config = config)
      )
    )
  )
}


settings_about_tab_ui <- function(
    ns,
    display_mode = "bottom"
) {
  div(
    id = ns("settings_about_section"),
    style = settings_tab_section_style(display_mode),
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      tags$img(src = "www/pannotator_hex_icon.png", height = "90px"),
      tags$img(src = "www/CSIRO_Wordmark+ANSA_RGB.png", height = "90px")
    ),
    tags$hr(),
    span(HTML("This R 'shiny' app was developed by Nunzio Knerr & Robert Godfree for immersively visualising, mapping and annotating panospheric imagery. The flexible interface allows annotation of any geocoded images using up to 4 user specified dropdown menus. Key functions include the ability to draw on & export parts of 360 images for downstream applications. Users can also draw polygons and points on map imagery related to the panoramic images and export them for further analysis. Downstream applications include using annotations to train AI/ML models and geospatial modelling and analysis of camera based survey data."), style = "font-size: 18px;"),
    tags$hr(),
    span("To cite this software please use:"),
    span(HTML("Godfree R, Knerr N (2024). Rapid ecological data collection from 360-degree imagery using visualisation and immersive sampling in the R pannotator package. <i>Methods in Ecology & Evolution, volume:</i>")),
    span("This paper contains a detailed description of the package and associated worked examples."),
    span("or:"),
    span(HTML("Knerr N, Godfree R (2024). <i>pannotator: Visualisation & Annotation of 360 Degree Imagery.</i> R package version 1.9.1.9000, https://github.com/nunzioknerr/pannotator")),
    span("to cite the software package itself"),
    tags$hr(),
    span("This software makes extensive use of:"),
    tags$a(href = "https://exiftool.org/", "ExifTool"),
    span("By Phil Harvey"),
    span("and:"),
    tags$a(href = "https://www.leafletjs.com", "Leaflet"),
    span("By Volodymyr Agafonkin"),
    span("and:"),
    tags$a(href = "https://pannellum.org/", "Pannellum"),
    span("By Matthew Petroff"),
    tags$a(href = "https://github.com/mpetroff/pannellum/blob/master/COPYING", "Pannellum License")
  )
}


mod_settings_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    settings_expanded <- reactiveVal(TRUE)
    export_project_settings_roots <- safe_shiny_roots()

    refresh_exiftool_status <- function(show_missing_notice = FALSE) {
      status <- get_exiftool_status()
      r$exiftool_status <- status

      if (isTRUE(show_missing_notice) && !isTRUE(status$installed)) {
        r$settings_panel_notice <- list(
          title = "ExifTool Required",
          message = "ExifTool is required to read image metadata from KMZ images. Install it from the System Dependencies section below before loading a KMZ.",
          type = "warning"
        )
      }

      status
    }

    r$exiftool_status <- get_exiftool_status()

    shinyFiles::shinyFileSave(
      input,
      "export_project_settings",
      roots = export_project_settings_roots,
      session = session,
      defaultPath = "",
      defaultRoot = NULL,
      allowDirCreate = TRUE
    )

    output$exiftool_status_ui <- renderUI({
      status <- r$exiftool_status
      if (is.null(status)) {
        status <- get_exiftool_status()
      }

      build_exiftool_status_ui(ns = ns, status = status)
    })

    output$toggle_settings_button <- renderUI({
      actionButton(
        inputId = ns("toggle_settings"),
        label = if (isTRUE(settings_expanded())) "Hide Settings" else "Show Settings",
        icon = icon(if (isTRUE(settings_expanded())) "chevron-up" else "chevron-down"),
        class = "btn btn-outline-secondary btn-sm"
      )
    })

    output$layout_schematic <- renderUI({
      selected_placement <- input$settingsPlacement
      if (is.null(selected_placement) || !selected_placement %in% c("bottom", "drawer")) {
        selected_placement <- settings_placement_value(r$config)
      }

      selected_drawer_position <- input$settingsDrawerPosition
      if (is.null(selected_drawer_position) || !selected_drawer_position %in% c("left", "right")) {
        selected_drawer_position <- settings_drawer_position_value(r$config)
      }

      settings_layout_schematic_ui(
        selected_placement = selected_placement,
        drawer_position = selected_drawer_position,
        ns = ns
      )
    })

    observeEvent(input$check_exiftool, ignoreInit = TRUE, {
      status <- refresh_exiftool_status(show_missing_notice = FALSE)

      r$settings_panel_notice <- list(
        title = if (isTRUE(status$installed)) "ExifTool Detected" else "ExifTool Missing",
        message = if (isTRUE(status$installed)) {
          paste0("ExifTool version ", status$version, " is available on this system.")
        } else {
          "ExifTool was not found. Install it from the System Dependencies section, then check again."
        },
        type = if (isTRUE(status$installed)) "success" else "warning"
      )
    })

    observeEvent(input$export_project_settings, ignoreInit = TRUE, {
      req(r$config)

      save_selection <- shinyFiles::parseSavePath(
        export_project_settings_roots,
        input$export_project_settings
      )

      export_path <- normalize_project_settings_export_path(save_selection)
      req(export_path)
      export_dir <- dirname(export_path)
      req(export_dir)
      google_maps_api_key <- normalize_export_only_google_maps_key(
        input$projectGoogleMapsApiKey
      )

      export_result <- tryCatch(
        export_project_settings_bundle(
          config = r$config,
          export_dir = export_dir,
          yaml_file_name = basename(export_path),
          source_data_dir = runtime_data_dir(r),
          annotations_data = r$user_annotations_data,
          annotations_file_path = r$user_annotations_file_name,
          google_maps_api_key = google_maps_api_key
        ),
        error = function(e) e
      )

      if (inherits(export_result, "error")) {
        r$settings_panel_notice <- list(
          title = "Project Files Export Failed",
          message = paste0(
            "pannotator could not export the project files: ",
            conditionMessage(export_result)
          ),
          type = "danger"
        )
        return()
      }

      updateTextInput(session, "projectGoogleMapsApiKey", value = "")
      missing_files <- export_result$files[!export_result$files$copied, , drop = FALSE]

      r$settings_panel_notice <- list(
        title = if (nrow(missing_files) > 0) {
          "Project Files Exported With Missing Files"
        } else {
          "Project Files Exported"
        },
        message = HTML(paste0(
          "Saved the project YAML and supporting files to:<br><code>",
          htmltools::htmlEscape(export_result$export_dir),
          "</code><br><br>YAML file:<br><code>",
          htmltools::htmlEscape(export_result$yaml_path),
          "</code><br><br>Launch the app with:<br><code>",
          htmltools::htmlEscape(project_settings_run_app_example(export_result$yaml_path)),
          "</code>",
          if (nrow(missing_files) > 0) {
            paste0(
              "<br><br><strong>Missing files:</strong> ",
              htmltools::htmlEscape(paste(missing_files$file, collapse = ", "))
            )
          } else {
            ""
          },
          if (!is.null(google_maps_api_key)) {
            paste0(
              "<br><br><strong>Google Maps API key note:</strong> The key was written only to the exported YAML file and was not saved to your regular app settings."
            )
          } else {
            ""
          }
        )),
        type = if (nrow(missing_files) > 0) "warning" else "success"
      )
      close_shinyfiles_dialog()
    })

    observeEvent(input$install_exiftool, ignoreInit = TRUE, {
      if (!requireNamespace("exiftoolr", quietly = TRUE)) {
        r$settings_panel_notice <- list(
          title = "Installation Unavailable",
          message = "The 'exiftoolr' package is not available, so pannotator cannot install ExifTool automatically.",
          type = "danger"
        )
        return()
      }

      install_result <- tryCatch(
        {
          withProgress(message = "Installing ExifTool", value = 0, {
            incProgress(0.25, detail = "Downloading and installing ExifTool")
            exiftoolr::install_exiftool(quiet = TRUE)
            incProgress(0.95, detail = "Checking the installed version")
          })
          NULL
        },
        error = function(e) e
      )

      if (inherits(install_result, "error")) {
        refresh_exiftool_status(show_missing_notice = FALSE)
        r$settings_panel_notice <- list(
          title = "ExifTool Installation Failed",
          message = paste0(
            "pannotator could not install ExifTool automatically: ",
            conditionMessage(install_result)
          ),
          type = "danger"
        )
        return()
      }

      status <- refresh_exiftool_status(show_missing_notice = FALSE)
      r$settings_panel_notice <- list(
        title = if (isTRUE(status$installed)) "ExifTool Installed" else "ExifTool Still Missing",
        message = if (isTRUE(status$installed)) {
          paste0(
            "ExifTool version ",
            status$version,
            " is now available. You can load KMZ files normally."
          )
        } else {
          "The install step completed, but ExifTool is still not available to the app. Try checking again or install it manually."
        },
        type = if (isTRUE(status$installed)) "success" else "warning"
      )
    })

    observe({
      if (is.null(r$settings_panel_notice) && !isTRUE(r$exiftool_status$installed)) {
        refresh_exiftool_status(show_missing_notice = TRUE)
      }
    })

    output$settings_notice <- renderUI({
      notice <- r$settings_panel_notice
      if (is.null(notice)) {
        return(NULL)
      }

      panel_notice(
        title = notice$title,
        message = notice$message,
        type = notice$type
      )
    })

    output$settings_pending_action <- renderUI({
      pending_action <- r$settings_pending_action
      if (is.null(pending_action)) {
        return(NULL)
      }

      bslib::card(
        class = "pannotator-inline-notice",
        border = "warning",
        full_screen = FALSE,
        bslib::card_header(pending_action$title),
        bslib::card_body(
          tags$p(pending_action$message, style = "margin-bottom: 12px;"),
          div(
            style = "display: flex; gap: 8px;",
            actionButton(
              inputId = ns("confirm_pending_action"),
              label = "Confirm",
              class = "btn btn-danger btn-sm"
            ),
            actionButton(
              inputId = ns("cancel_pending_action"),
              label = "Cancel",
              class = "btn btn-outline-secondary btn-sm"
            )
          )
        )
      )
    })

    queue_layout_notice <- function() {
      r$settings_panel_notice <- list(
        title = "Layout Changes Pending",
        message = "Layout changes will be saved when you click Apply Changes. The page will reload to apply the new layout.",
        type = "warning"
      )
    }

    queue_workspace_reload_notice <- function() {
      workspace_settings_dirty(TRUE)
      r$settings_panel_notice <- list(
        title = "Workspace Settings Pending",
        message = "Workspace display settings were saved. Click Apply Changes to clear the current KMZ workspace, then reload the KMZ so map and image layers are rebuilt cleanly.",
        type = "warning"
      )
    }

    save_settings_input <- function(config_key, value) {
      req(r$config)

      r$config[[config_key]] <- value
      save_user_config(config_key, runtime = r)

      if (setting_requires_workspace_reload(config_key)) {
        queue_workspace_reload_notice()
      }

      invisible(TRUE)
    }

    width_slider_update_in_progress <- reactiveVal(FALSE)

    sync_main_panel_width_sliders <- function(changed_field = NULL) {
      normalized_widths <- normalize_main_panel_widths(
        widths = list(
          mapPanelWidth = input$mapPanelWidth,
          panoPanelWidth = input$panoPanelWidth,
          formPanelWidth = input$formPanelWidth
        ),
        changed_field = changed_field
      )

      width_slider_update_in_progress(TRUE)
      on.exit(width_slider_update_in_progress(FALSE), add = TRUE)

      if (!identical(input$mapPanelWidth, normalized_widths$mapPanelWidth)) {
        updateSliderInput(session, "mapPanelWidth", value = normalized_widths$mapPanelWidth)
      }
      if (!identical(input$panoPanelWidth, normalized_widths$panoPanelWidth)) {
        updateSliderInput(session, "panoPanelWidth", value = normalized_widths$panoPanelWidth)
      }
      if (!identical(input$formPanelWidth, normalized_widths$formPanelWidth)) {
        updateSliderInput(session, "formPanelWidth", value = normalized_widths$formPanelWidth)
      }

      normalized_widths
    }

    csv_editor_revision <- reactiveVal(0L)
    settings_csv_specs <- settings_csv_editor_specs()
    csv_editor_visible <- reactiveValues(
      username = FALSE,
      lookup1 = FALSE,
      lookup2 = FALSE,
      lookup3 = FALSE,
      lookup4 = FALSE
    )
    lookup_settings_dirty <- reactiveVal(FALSE)
    workspace_settings_dirty <- reactiveVal(FALSE)

    bump_csv_editor_revision <- function() {
      csv_editor_revision(isolate(csv_editor_revision()) + 1L)
    }

    show_lookup_validation_notice <- function(validation_result, title = "Lookup Changes Not Applied") {
      r$settings_panel_notice <- list(
        title = title,
        message = HTML(paste(validation_result$messages, collapse = "<br>")),
        type = "danger"
      )
    }

    validate_lookup_csv_change <- function(spec, table_data, title) {
      validation_result <- validate_settings_lookup_csv_change(
        annotation_data = r$user_annotations_data,
        spec = spec,
        table_data = table_data,
        config = r$config
      )

      if (!isTRUE(validation_result$valid)) {
        show_lookup_validation_notice(validation_result, title = title)
        return(FALSE)
      }

      TRUE
    }

    set_lookup_enabled <- function(lookup_index, enabled) {
      req(r$config)

      config_key <- paste0("lookup", lookup_index, "Enabled")
      previous_value <- isTRUE(r$config[[config_key]])
      proposed_config <- r$config
      proposed_config[[config_key]] <- isTRUE(enabled)

      validation_result <- validate_lookup_settings_against_annotations(
        annotation_data = r$user_annotations_data,
        config = proposed_config,
        data_dir = runtime_data_dir(r)
      )

      if (!isTRUE(validation_result$valid)) {
        updateCheckboxInput(session, config_key, value = previous_value)
        show_lookup_validation_notice(validation_result)
        return(invisible(FALSE))
      }

      r$config[[config_key]] <- isTRUE(enabled)
      save_user_config(config_key, runtime = r)
      lookup_settings_dirty(TRUE)

      lookup_panel <- paste0("lookup", lookup_index)
      if (isTRUE(r$config[[config_key]])) {
        bslib::accordion_panel_open("lookup_accordion", values = lookup_panel, session = session)
      } else {
        bslib::accordion_panel_close("lookup_accordion", values = lookup_panel, session = session)
      }

      invisible(TRUE)
    }

    for (spec_name in names(settings_csv_specs)) {
      local({
        spec <- settings_csv_specs[[spec_name]]

        observeEvent(input[[settings_csv_editor_button_id(spec)]], ignoreInit = TRUE, {
          csv_editor_visible[[spec$key]] <- TRUE
        })

        output[[spec$output_id]] <- renderUI({
          csv_editor_revision()
          if (!isTRUE(csv_editor_visible[[spec$key]])) {
            return(NULL)
          }

          if (!requireNamespace("rhandsontable", quietly = TRUE)) {
            return(panel_status_message(
              "Install the 'rhandsontable' package to edit this CSV in place."
            ))
          }

          rhandsontable::rHandsontableOutput(ns(spec$table_output_id))
        })

        if (requireNamespace("rhandsontable", quietly = TRUE)) {
          output[[spec$table_output_id]] <- rhandsontable::renderRHandsontable({
            req(isTRUE(csv_editor_visible[[spec$key]]))
            csv_editor_revision()

            build_settings_csv_editor_hot(
              table_data = read_settings_csv_editor_data(
                spec,
                config = r$config,
                data_dir = runtime_data_dir(r)
              ),
              spec = spec
            )
          })

          observeEvent(input[[spec$table_output_id]], ignoreInit = TRUE, {
            req(r$config)

            edited_data <- rhandsontable::hot_to_r(input[[spec$table_output_id]])
            req(edited_data)
            normalized_edited_data <- normalize_settings_csv_editor_data(edited_data, spec$columns)

            if (!validate_lookup_csv_change(
              spec = spec,
              table_data = normalized_edited_data,
              title = paste0(spec$title, " Not Saved")
            )) {
              return(invisible(NULL))
            }

            write_settings_csv_editor_data(
              normalized_edited_data,
              spec,
              config = r$config,
              data_dir = runtime_data_dir(r)
            )
            if (startsWith(spec$key, "lookup")) {
              lookup_settings_dirty(TRUE)
            }

            r$settings_panel_notice <- list(
              title = paste0(spec$title, " Saved"),
              message = paste0(
                "Saved edits to ",
            settings_csv_editor_file_name(spec, config = r$config),
                ". Click Apply Changes if you want annotation inputs to reload the updated choices."
              ),
              type = "success"
            )
          })
        }

        observeEvent(input[[spec$file_input_id]], ignoreInit = TRUE, {
          req(r$config, input[[spec$file_input_id]])

          uploaded_data <- tryCatch(
            utils::read.csv(
              input[[spec$file_input_id]]$datapath,
              stringsAsFactors = FALSE,
              check.names = FALSE
            ),
            error = function(e) data.frame(stringsAsFactors = FALSE)
          )
          uploaded_data <- normalize_settings_csv_editor_data(uploaded_data, spec$columns)

          if (!validate_lookup_csv_change(
            spec = spec,
            table_data = uploaded_data,
            title = paste0(spec$title, " Not Loaded")
          )) {
            return(invisible(NULL))
          }

          current_file_name <- settings_csv_editor_file_name(spec, config = r$config)
          r$config[[spec$config_key]] <- current_file_name

          file.copy(
            input[[spec$file_input_id]]$datapath,
            settings_csv_editor_path(
              spec,
              config = r$config,
              data_dir = runtime_data_dir(r)
            ),
            overwrite = TRUE
          )

          write_settings_csv_editor_data(
            read_settings_csv_editor_data(
              spec,
              config = r$config,
              data_dir = runtime_data_dir(r)
            ),
            spec,
            config = r$config,
            data_dir = runtime_data_dir(r)
          )

          save_user_config(spec$config_key, runtime = r)
          bump_csv_editor_revision()
          if (startsWith(spec$key, "lookup")) {
            lookup_settings_dirty(TRUE)
          }

          r$settings_panel_notice <- list(
            title = paste0(spec$title, " Loaded"),
            message = paste0(
              "Loaded ",
              current_file_name,
              " and made it available for in-place editing below."
            ),
            type = "success"
          )
        })
      })
    }

    observeEvent(input$toggle_settings, ignoreInit = TRUE, {
      if (isTRUE(settings_expanded())) {
        settings_expanded(FALSE)
        shinyjs::hide("settings_body", anim = TRUE, animType = "slide")
      } else {
        settings_expanded(TRUE)
        shinyjs::show("settings_body", anim = TRUE, animType = "slide")
      }
    })

    open_settings_drawer_for_tour <- function() {
      config <- runtime_config_value(r)
      if (!identical(settings_placement_value(config), "drawer")) {
        return(FALSE)
      }

      shinyjs::runjs("
        (function() {
          var drawer = document.getElementById('app_settings_drawer');
          var toggle = document.querySelector(
            '.pannotator-drawer-shell .collapse-toggle[aria-controls=\"app_settings_drawer\"]'
          );

          if (drawer && toggle && drawer.hasAttribute('hidden')) {
            toggle.click();
          }
        })();
      ")

      TRUE
    }

    start_guided_tour <- function() {
      if (!requireNamespace("rintrojs", quietly = TRUE)) {
        return(FALSE)
      }

      if (!isTRUE(settings_expanded())) {
        settings_expanded(TRUE)
        shinyjs::show("settings_body", anim = TRUE, animType = "slide")
      }

      drawer_is_active <- open_settings_drawer_for_tour()
      launch_tour <- function() {
        rintrojs::introjs(
          session = session,
          options = list(steps = build_app_tour_steps(ns)),
          events = list(onbeforechange = rintrojs::readCallback("switchTabs"))
        )
      }

      if (isTRUE(drawer_is_active)) {
        shinyjs::delay(450, launch_tour())
      } else {
        launch_tour()
      }

      invisible(TRUE)
    }

    session$onFlushed(function() {
      config <- runtime_config_value(r)
      if (!isTRUE(config$askGuidedTourOnStartup)) {
        return(invisible(NULL))
      }
      if (!requireNamespace("rintrojs", quietly = TRUE)) {
        return(invisible(NULL))
      }

      shinyjs::delay(500, showModal(guided_tour_startup_prompt_ui(ns)))
    }, once = TRUE)

    observeEvent(input$start_tour, {
      start_guided_tour()
    })

    observeEvent(input$start_startup_tour, {
      removeModal()
      start_guided_tour()
    })

    observeEvent(input$clearAllButton, ignoreInit = TRUE, {
      r$settings_pending_action <- list(
        action = "clear_all_annotations",
        title = "Clear All Annotations?",
        message = "This will remove all annotation records from the current data set and cannot be undone."
      )
    })

    observeEvent(input$cancel_pending_action, ignoreInit = TRUE, {
      r$settings_pending_action <- NULL
    })

    observeEvent(input$confirm_pending_action, ignoreInit = TRUE, {
      pending_action <- r$settings_pending_action
      req(pending_action)

      if (identical(pending_action$action, "clear_all_annotations")) {
        r$user_annotations_data <- clear_all_annotation_data(myUserAnnotationsData = r$user_annotations_data)
        clear_annotations_form(runtime = r)
        r$settings_panel_notice <- list(
          title = "Annotations Cleared",
          message = "All annotations were removed from the current session data.",
          type = "warning"
        )
      }

      r$settings_pending_action <- NULL
    })

    observeEvent(input$applySettingsButton, ignoreInit = TRUE, {
      req(r$config)
      normalized_widths <- normalize_main_panel_widths(
        widths = list(
          mapPanelWidth = input$mapPanelWidth,
          panoPanelWidth = input$panoPanelWidth,
          formPanelWidth = input$formPanelWidth
        )
      )
      selected_settings_placement <- input$settingsPlacement
      if (is.null(selected_settings_placement) || !selected_settings_placement %in% c("bottom", "drawer")) {
        selected_settings_placement <- settings_placement_value(r$config)
      }
      selected_drawer_position <- input$settingsDrawerPosition
      if (is.null(selected_drawer_position) || !selected_drawer_position %in% c("left", "right")) {
        selected_drawer_position <- settings_drawer_position_value(r$config)
      }
      selected_app_theme <- input$appTheme
      if (is.null(selected_app_theme) || length(selected_app_theme) == 0 || !nzchar(selected_app_theme[[1]])) {
        selected_app_theme <- r$config$appTheme
      }
      selected_app_theme_mode <- if (isTRUE(input$appThemeMode)) {
        "dark"
      } else {
        "light"
      }

      current_settings_placement <- settings_placement_value(r$config)
      current_drawer_position <- settings_drawer_position_value(r$config)
      drawer_layout_changed <- !identical(selected_drawer_position, current_drawer_position) &&
        ("drawer" %in% c(selected_settings_placement, current_settings_placement))

      layout_changed <- !identical(
        as.integer(unlist(normalized_widths)),
        as.integer(c(r$config$mapPanelWidth, r$config$panoPanelWidth, r$config$formPanelWidth))
      ) || !identical(selected_settings_placement, current_settings_placement) ||
        isTRUE(drawer_layout_changed)

      lookup_changes_pending <- isTRUE(lookup_settings_dirty())
      workspace_changes_pending <- isTRUE(workspace_settings_dirty())
      if (isTRUE(lookup_changes_pending)) {
        validation_result <- validate_lookup_settings_against_annotations(
          annotation_data = r$user_annotations_data,
          config = r$config,
          data_dir = runtime_data_dir(r)
        )

        if (!isTRUE(validation_result$valid)) {
          show_lookup_validation_notice(validation_result)
          return(invisible(NULL))
        }
      }

      r$config["mapPanelWidth"] <- normalized_widths$mapPanelWidth
      r$config["panoPanelWidth"] <- normalized_widths$panoPanelWidth
      r$config["formPanelWidth"] <- normalized_widths$formPanelWidth
      r$config["settingsPlacement"] <- selected_settings_placement
      r$config["settingsDrawerPosition"] <- selected_drawer_position
      r$config["appTheme"] <- selected_app_theme
      r$config["appThemeMode"] <- selected_app_theme_mode

      updateSliderInput(session, "mapPanelWidth", value = normalized_widths$mapPanelWidth)
      updateSliderInput(session, "panoPanelWidth", value = normalized_widths$panoPanelWidth)
      updateSliderInput(session, "formPanelWidth", value = normalized_widths$formPanelWidth)

      save_user_config("mapPanelWidth", runtime = r)
      save_user_config("panoPanelWidth", runtime = r)
      save_user_config("formPanelWidth", runtime = r)
      save_user_config("settingsPlacement", runtime = r)
      save_user_config("settingsDrawerPosition", runtime = r)
      save_user_config("appTheme", runtime = r)
      save_user_config("appThemeMode", runtime = r)

      r$config <- merge_panel_config(r$config, data_path = runtime_data_dir(r))
      update_runtime_context_config(r)
      refresh_app_state_assets(r)
      session$setCurrentTheme(get_app_theme(config = r$config))
      reset_workspace_after_apply <- isTRUE(lookup_changes_pending || workspace_changes_pending)
      if (isTRUE(layout_changed)) {
        lookup_settings_dirty(FALSE)
        workspace_settings_dirty(FALSE)
      } else if (isTRUE(reset_workspace_after_apply)) {
        workspace_reset_message <- if (isTRUE(lookup_changes_pending && workspace_changes_pending)) {
          "Lookup and workspace display settings changed. Reload a KMZ file so the updated choices and map/image layers load cleanly."
        } else if (isTRUE(lookup_changes_pending)) {
          "Lookup settings changed. Reload a KMZ file to continue with the updated lookup choices."
        } else {
          "Workspace display settings changed. Reload a KMZ file so the map and image layers are rebuilt cleanly with the updated settings."
        }

        reset_loaded_kmz_workspace(r, message = workspace_reset_message)
        lookup_settings_dirty(FALSE)
        workspace_settings_dirty(FALSE)
      } else {
        refresh_user_config(session, runtime = r)
      }

      if (isTRUE(layout_changed)) {
        r$settings_panel_notice <- list(
          title = "Layout Updated",
          message = "Panel width changes were saved. Reloading the app to apply the new layout.",
          type = "warning"
        )
        shinyjs::delay(1000, session$reload())
        shinyjs::delay(2000, shinyjs::runjs("window.location.reload();"))
      } else {
        settings_message <- if (isTRUE(lookup_changes_pending && workspace_changes_pending)) {
          "Lookup and workspace display settings were applied. The current KMZ workspace was cleared; reload the KMZ to continue with the updated settings."
        } else if (isTRUE(lookup_changes_pending)) {
          "Lookup settings were applied. The current KMZ workspace was cleared so the updated lookup choices load cleanly with the next KMZ."
        } else if (isTRUE(workspace_changes_pending)) {
          "Workspace display settings were applied. The current KMZ workspace was cleared; reload the KMZ so the updated map and image styling is used."
        } else {
          "Your settings changes were applied."
        }
        r$settings_panel_notice <- list(
          title = "Settings Applied",
          message = settings_message,
          type = "success"
        )
      }
    })

    observeEvent(input$showWorkflowGuidanceNotices, ignoreInit = TRUE, {
      req(r$config)
      r$config["showWorkflowGuidanceNotices"] <- input$showWorkflowGuidanceNotices
      save_user_config("showWorkflowGuidanceNotices", runtime = r)
    })

    observeEvent(input$askGuidedTourOnStartup, ignoreInit = TRUE, {
      req(r$config)
      r$config["askGuidedTourOnStartup"] <- input$askGuidedTourOnStartup
      save_user_config("askGuidedTourOnStartup", runtime = r)
    })

    observeEvent(input$appTheme, ignoreInit = TRUE, {
      req(r$config)
      r$config["appTheme"] <- input$appTheme
      save_user_config("appTheme", runtime = r)
    })

    observeEvent(input$appThemeMode, ignoreInit = TRUE, {
      req(r$config)
      r$config["appThemeMode"] <- if (isTRUE(input$appThemeMode)) {
        "dark"
      } else {
        "light"
      }
      save_user_config("appThemeMode", runtime = r)
    })

    observeEvent(input$mapPanelWidth, ignoreInit = TRUE, {
      if (isTRUE(width_slider_update_in_progress())) {
        return(invisible(NULL))
      }

      sync_main_panel_width_sliders("mapPanelWidth")
      queue_layout_notice()
    })

    observeEvent(input$panoPanelWidth, ignoreInit = TRUE, {
      if (isTRUE(width_slider_update_in_progress())) {
        return(invisible(NULL))
      }

      sync_main_panel_width_sliders("panoPanelWidth")
      queue_layout_notice()
    })

    observeEvent(input$formPanelWidth, ignoreInit = TRUE, {
      if (isTRUE(width_slider_update_in_progress())) {
        return(invisible(NULL))
      }

      sync_main_panel_width_sliders("formPanelWidth")
      queue_layout_notice()
    })

    observeEvent(input$settingsPlacement, ignoreInit = TRUE, {
      req(r$config)
      queue_layout_notice()
    })

    observeEvent(input$settingsDrawerPosition, ignoreInit = TRUE, {
      req(r$config)
      queue_layout_notice()
    })

    observeEvent(input$mapPanelSource, ignoreInit = TRUE, {
      save_settings_input("mapPanelSource", input$mapPanelSource)
    })
    observeEvent(input$mapIconColour, ignoreInit = TRUE, {
      save_settings_input("mapIconColour", input$mapIconColour)
    })
    observeEvent(input$mapMarkerColour, ignoreInit = TRUE, {
      save_settings_input("mapMarkerColour", input$mapMarkerColour)
    })
    observeEvent(input$mapPolygonStroke, ignoreInit = TRUE, {
      save_settings_input("mapPolygonStroke", input$mapPolygonStroke)
    })
    observeEvent(input$mapPolygonStrokeColour, ignoreInit = TRUE, {
      save_settings_input("mapPolygonStrokeColour", input$mapPolygonStrokeColour)
    })
    observeEvent(input$mapPolygonStrokeWeight, ignoreInit = TRUE, {
      save_settings_input("mapPolygonStrokeWeight", input$mapPolygonStrokeWeight)
    })
    observeEvent(input$mapPolygonStrokeOpacity, ignoreInit = TRUE, {
      save_settings_input("mapPolygonStrokeOpacity", input$mapPolygonStrokeOpacity)
    })
    observeEvent(input$mapPolygonFill, ignoreInit = TRUE, {
      save_settings_input("mapPolygonFill", input$mapPolygonFill)
    })
    observeEvent(input$mapPolygonFillColour, ignoreInit = TRUE, {
      save_settings_input("mapPolygonFillColour", input$mapPolygonFillColour)
    })
    observeEvent(input$mapPolygonFillOpacity, ignoreInit = TRUE, {
      save_settings_input("mapPolygonFillOpacity", input$mapPolygonFillOpacity)
    })

    observeEvent(input$pano360IconColour, ignoreInit = TRUE, {
      save_settings_input("pano360IconColour", input$pano360IconColour)
    })
    observeEvent(input$pano360MarkerColour, ignoreInit = TRUE, {
      save_settings_input("pano360MarkerColour", input$pano360MarkerColour)
    })
    observeEvent(input$pano360PolygonStroke, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonStroke", input$pano360PolygonStroke)
    })
    observeEvent(input$pano360PolygonStrokeColour, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonStrokeColour", input$pano360PolygonStrokeColour)
    })
    observeEvent(input$pano360PolygonStrokeWeight, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonStrokeWeight", input$pano360PolygonStrokeWeight)
    })
    observeEvent(input$pano360PolygonStrokeOpacity, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonStrokeOpacity", input$pano360PolygonStrokeOpacity)
    })
    observeEvent(input$showPano360PolygonStrokeInCropExport, ignoreInit = TRUE, {
      req(r$config)
      r$config["showPano360PolygonStrokeInCropExport"] <- input$showPano360PolygonStrokeInCropExport
      save_user_config("showPano360PolygonStrokeInCropExport", runtime = r)
    })
    observeEvent(input$pano360PolygonFill, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonFill", input$pano360PolygonFill)
    })
    observeEvent(input$pano360PolygonFillColour, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonFillColour", input$pano360PolygonFillColour)
    })
    observeEvent(input$pano360PolygonFillOpacity, ignoreInit = TRUE, {
      save_settings_input("pano360PolygonFillOpacity", input$pano360PolygonFillOpacity)
    })
    observeEvent(input$showPano360PolygonFillInCropExport, ignoreInit = TRUE, {
      req(r$config)
      r$config["showPano360PolygonFillInCropExport"] <- input$showPano360PolygonFillInCropExport
      save_user_config("showPano360PolygonFillInCropExport", runtime = r)
    })

    observeEvent(input$exportFileFormat, ignoreInit = TRUE, {
      req(r$config)
      r$config["exportFileFormat"] <- input$exportFileFormat
      save_user_config("exportFileFormat", runtime = r)
    })

    observeEvent(input$lookup1Label, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup1Label"] <- input$lookup1Label
      save_user_config("lookup1Label", runtime = r)
      lookup_settings_dirty(TRUE)
    })
    observeEvent(input$lookup1HelpFile, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup1HelpFile"] <- "help1.pdf"
      file.copy(input$lookup1HelpFile$datapath, normalizePath(file.path(runtime_data_dir(r), "help1.pdf")), overwrite = TRUE)
      save_user_config("lookup1HelpFile", runtime = r)
    })

    observeEvent(input$lookup2Enabled, ignoreInit = TRUE, {
      set_lookup_enabled(2L, input$lookup2Enabled)
    })
    observeEvent(input$lookup2Label, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup2Label"] <- input$lookup2Label
      save_user_config("lookup2Label", runtime = r)
      lookup_settings_dirty(TRUE)
    })
    observeEvent(input$lookup2HelpFile, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup2HelpFile"] <- "help2.pdf"
      file.copy(input$lookup2HelpFile$datapath, normalizePath(file.path(runtime_data_dir(r), "help2.pdf")), overwrite = TRUE)
      save_user_config("lookup2HelpFile", runtime = r)
    })

    observeEvent(input$lookup3Enabled, ignoreInit = TRUE, {
      set_lookup_enabled(3L, input$lookup3Enabled)
    })
    observeEvent(input$lookup3Label, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup3Label"] <- input$lookup3Label
      save_user_config("lookup3Label", runtime = r)
      lookup_settings_dirty(TRUE)
    })
    observeEvent(input$lookup3HelpFile, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup3HelpFile"] <- "help3.pdf"
      file.copy(input$lookup3HelpFile$datapath, normalizePath(file.path(runtime_data_dir(r), "help3.pdf")), overwrite = TRUE)
      save_user_config("lookup3HelpFile", runtime = r)
    })

    observeEvent(input$lookup4Enabled, ignoreInit = TRUE, {
      set_lookup_enabled(4L, input$lookup4Enabled)
    })
    observeEvent(input$lookup4Label, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup4Label"] <- input$lookup4Label
      save_user_config("lookup4Label", runtime = r)
      lookup_settings_dirty(TRUE)
    })
    observeEvent(input$lookup4HelpFile, ignoreInit = TRUE, {
      req(r$config)
      r$config["lookup4HelpFile"] <- "help4.pdf"
      file.copy(input$lookup4HelpFile$datapath, normalizePath(file.path(runtime_data_dir(r), "help4.pdf")), overwrite = TRUE)
      save_user_config("lookup4HelpFile", runtime = r)
    })
  })
}

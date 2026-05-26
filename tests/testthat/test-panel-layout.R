mock_panel_registry <- function() {
  list(
    map = list(
      key = "map",
      title = "Mapping Panel",
      width_field = "mapPanelWidth",
      wrapper_id = "map_panel",
      panel_style = NULL,
      ui = function() shiny::div("Map UI")
    ),
    image = list(
      key = "image",
      title = "Image Panel",
      width_field = "panoPanelWidth",
      wrapper_id = "image_panel",
      panel_style = NULL,
      ui = function() shiny::div("Image UI")
    ),
    annotation = list(
      key = "annotation",
      title = "Annotation Panel",
      width_field = "formPanelWidth",
      wrapper_id = "form_panel",
      panel_style = "padding: 20px;",
      ui = function() shiny::div("Annotation UI")
    ),
    annotation_table = list(
      key = "annotation_table",
      title = "Annotation Table",
      width_field = "annotationTablePanelWidth",
      wrapper_id = "annotation_table_panel",
      panel_style = NULL,
      ui = function() shiny::div("Annotation Table UI")
    )
  )
}


test_that("merge_panel_config adds modular layout defaults to legacy config", {
  merged <- merge_panel_config(
    config = list(
      mapPanelWidth = 6,
      panoPanelWidth = 4,
      formPanelWidth = 2
    ),
    data_path = tempdir()
  )

  expect_equal(merged$configVersion, 3L)
  expect_equal(merged$panelOrder, c("map", "image", "annotation", "annotation_table"))
  expect_equal(merged$enabledPanels, c("map", "image", "annotation", "annotation_table"))
  expect_equal(merged$settingsPlacement, "drawer")
  expect_equal(merged$settingsDrawerPosition, "right")
  expect_equal(merged$appThemeMode, "light")
  expect_true(merged$askGuidedTourOnStartup)
  expect_equal(merged$mapPanelWidth, 6)
  expect_equal(merged$panoPanelWidth, 4)
  expect_equal(merged$formPanelWidth, 2)
  expect_equal(merged$annotationTablePanelWidth, 12)
})


test_that("app theme mode is normalized to supported values", {
  expect_equal(normalize_app_theme_mode("dark"), "dark")
  expect_equal(normalize_app_theme_mode("LIGHT"), "light")
  expect_equal(normalize_app_theme_mode("unknown"), "light")
  expect_equal(normalize_app_theme_mode(NULL), "light")

  merged <- merge_panel_config(
    config = list(appThemeMode = "unexpected"),
    data_path = tempdir()
  )

  expect_equal(merged$appThemeMode, "light")
})


test_that("boolean app config fields are normalized", {
  merged <- merge_panel_config(
    config = list(
      showWorkflowGuidanceNotices = "false",
      askGuidedTourOnStartup = "no"
    ),
    data_path = tempdir()
  )

  expect_false(merged$showWorkflowGuidanceNotices)
  expect_false(merged$askGuidedTourOnStartup)

  merged <- merge_panel_config(
    config = list(
      showWorkflowGuidanceNotices = "unexpected",
      askGuidedTourOnStartup = "unexpected"
    ),
    data_path = tempdir()
  )

  expect_true(merged$showWorkflowGuidanceNotices)
  expect_true(merged$askGuidedTourOnStartup)
})


test_that("default config uses current workflow guidance field names", {
  config <- default_app_config(tempdir())

  expect_true("showWorkflowGuidanceNotices" %in% names(config))
})


test_that("workspace reload settings identify map and image render fields", {
  expect_true(setting_requires_workspace_reload("mapPanelSource"))
  expect_true(setting_requires_workspace_reload("mapIconColour"))
  expect_true(setting_requires_workspace_reload("mapPolygonFillOpacity"))
  expect_true(setting_requires_workspace_reload("pano360IconColour"))
  expect_true(setting_requires_workspace_reload("pano360PolygonStrokeWeight"))

  expect_false(setting_requires_workspace_reload("exportFileFormat"))
  expect_false(setting_requires_workspace_reload("showWorkflowGuidanceNotices"))
  expect_false(setting_requires_workspace_reload("appTheme"))
})


test_that("main panel widths are normalized back to a total of twelve", {
  normalized <- normalize_main_panel_widths(
    list(
      mapPanelWidth = 6,
      panoPanelWidth = 6,
      formPanelWidth = 4
    )
  )

  expect_equal(
    sum(unlist(normalized)),
    12
  )
  expect_gte(normalized$mapPanelWidth, 3)
  expect_gte(normalized$panoPanelWidth, 3)
  expect_gte(normalized$formPanelWidth, 2)
})


test_that("panel order is normalized against enabled panels", {
  config <- merge_panel_config(
    config = list(
      configVersion = 3L,
      panelOrder = c("annotation", "map", "map", "unknown"),
      enabledPanels = c("annotation", "map"),
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  expect_equal(
    get_panel_order(config = config, registry = mock_panel_registry()),
    c("annotation", "map")
  )
  expect_equal(
    get_panel_width("annotation", config = config, registry = mock_panel_registry()),
    2L
  )
})


test_that("panel host anchors map image and table beside the annotation panel", {
  config <- merge_panel_config(
    config = list(
      configVersion = 3L,
      panelOrder = c("annotation", "map", "image", "annotation_table"),
      enabledPanels = c("annotation", "map", "image", "annotation_table"),
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  html <- htmltools::renderTags(
    build_panel_host_ui(config = config, registry = mock_panel_registry())
  )$html

  annotation_pos <- regexpr("id=\"panel-annotation\"", html, fixed = TRUE)[1]
  map_pos <- regexpr("id=\"panel-map\"", html, fixed = TRUE)[1]
  image_pos <- regexpr("id=\"panel-image\"", html, fixed = TRUE)[1]
  table_pos <- regexpr("id=\"panel-annotation_table\"", html, fixed = TRUE)[1]

  expect_match(html, "pannotator-workspace-layout-with-annotation")
  expect_match(html, "pannotator-workspace-main")
  expect_match(html, "pannotator-workspace-annotation")
  expect_gt(annotation_pos, 0)
  expect_gt(map_pos, 0)
  expect_gt(image_pos, 0)
  expect_gt(table_pos, 0)
  expect_true(map_pos < image_pos)
  expect_true(image_pos < table_pos)
  expect_true(table_pos < annotation_pos)
})


test_that("panel rows are split before widths exceed twelve", {
  config <- merge_panel_config(
    config = list(
      configVersion = 3L,
      panelOrder = c("map", "image", "annotation", "annotation_table"),
      enabledPanels = c("map", "image", "annotation", "annotation_table"),
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  rows <- split_panel_rows(
    get_panel_order(config = config, registry = mock_panel_registry()),
    config = config,
    registry = mock_panel_registry()
  )

  expect_equal(rows[[1]], c("map", "image", "annotation"))
  expect_equal(rows[[2]], "annotation_table")
})


test_that("app ui mounts the panel host shell", {
  ui <- app_ui()
  html <- paste(htmltools::renderTags(ui)$html, collapse = "")

  expect_match(html, "panel_host-panel_host")
})


test_that("panel host html includes workspace layout classes", {
  config <- merge_panel_config(
    config = list(
      configVersion = 3L,
      panelOrder = c("map", "image", "annotation", "annotation_table"),
      enabledPanels = c("map", "image", "annotation", "annotation_table"),
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  html <- htmltools::renderTags(
    build_panel_host_ui(config = config, registry = mock_panel_registry())
  )$html

  expect_match(html, "pannotator-workspace-layout")
  expect_match(html, "pannotator-workspace-top")
  expect_match(html, "pannotator-workspace-table")
  expect_match(html, "pannotator-workspace-annotation")
  expect_match(html, "grid-template-columns: minmax(0, 10fr) minmax(280px, 2fr);", fixed = TRUE)
})


test_that("bottom settings render below the table in the left workspace", {
  config <- merge_panel_config(
    config = list(
      settingsPlacement = "bottom",
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  html <- htmltools::renderTags(
    build_panel_host_ui(
      config = config,
      registry = mock_panel_registry(),
      settings_ui = shiny::div(id = "settings-settings_panel", "Settings UI")
    )
  )$html

  table_pos <- regexpr("id=\"panel-annotation_table\"", html, fixed = TRUE)[1]
  settings_pos <- regexpr("id=\"panel-settings\"", html, fixed = TRUE)[1]
  annotation_pos <- regexpr("id=\"panel-annotation\"", html, fixed = TRUE)[1]

  expect_gt(table_pos, 0)
  expect_gt(settings_pos, 0)
  expect_gt(annotation_pos, 0)
  expect_true(table_pos < settings_pos)
  expect_true(settings_pos < annotation_pos)
})


test_that("annotation table panel uses constrained shell classes", {
  html <- htmltools::renderTags(mod_annotation_table_ui("annotation_table"))$html

  expect_match(html, "pannotator-annotation-table-shell")
  expect_match(html, "annotation_table-annotation_table_container")
})


test_that("app ui includes spacing between panel rows", {
  ui <- app_ui()
  rendered <- htmltools::renderTags(ui)
  style_text <- paste(c(rendered$head, rendered$html), collapse = "")

  expect_match(style_text, "pannotator-panel-column", fixed = TRUE)
  expect_match(style_text, "margin-bottom: 20px;", fixed = TRUE)
  expect_match(style_text, "pannotator-annotation-table-widget", fixed = TRUE)
  expect_match(style_text, "overflow-x: auto;", fixed = TRUE)
  expect_match(style_text, "td.currentRow", fixed = TRUE)
  expect_match(style_text, "tbody tr:hover td", fixed = TRUE)
})


test_that("settings column padding does not target colourpicker swatches", {
  ui <- app_ui()
  rendered <- htmltools::renderTags(ui)
  style_text <- paste(c(rendered$head, rendered$html), collapse = "")

  expect_match(
    style_text,
    ".pannotator-settings-shell .row > [class*='col-']",
    fixed = TRUE
  )
  expect_false(
    grepl(
      ".pannotator-settings-shell [class*='col-']",
      style_text,
      fixed = TRUE
    )
  )
})


test_that("app ui enlarges notification progress boxes for wrapped text", {
  ui <- app_ui()
  rendered <- htmltools::renderTags(ui)
  style_text <- paste(c(rendered$head, rendered$html), collapse = "")

  expect_match(style_text, "#shiny-notification-panel", fixed = TRUE)
  expect_match(style_text, "top: 50% !important;", fixed = TRUE)
  expect_match(style_text, "left: 50% !important;", fixed = TRUE)
  expect_match(style_text, "transform: translate(-50%, -50%);", fixed = TRUE)
  expect_match(style_text, "width: min(420px, calc(100vw - 32px));", fixed = TRUE)
  expect_match(style_text, ".shiny-progress-notification .progress-text", fixed = TRUE)
  expect_match(style_text, "min-height: 72px;", fixed = TRUE)
  expect_match(style_text, "min-height: 44px;", fixed = TRUE)
  expect_match(style_text, "line-height: 1.35;", fixed = TRUE)
})


test_that("app ui centers icon-only annotation card action buttons", {
  ui <- app_ui()
  rendered <- htmltools::renderTags(ui)
  style_text <- paste(c(rendered$head, rendered$html), collapse = "")

  expect_match(style_text, ".pannotator-card-actions .pannotator-card-action", fixed = TRUE)
  expect_match(style_text, "width: 34px;", fixed = TRUE)
  expect_match(style_text, "height: 34px;", fixed = TRUE)
  expect_match(style_text, ".action-label", fixed = TRUE)
  expect_match(style_text, "display: none;", fixed = TRUE)
})


test_that("drawer shell renders a sidebar and toggle button", {
  config <- merge_panel_config(
    config = list(
      settingsPlacement = "drawer",
      settingsDrawerPosition = "left",
      mapPanelWidth = 5,
      panoPanelWidth = 5,
      formPanelWidth = 2,
      annotationTablePanelWidth = 12
    ),
    data_path = tempdir()
  )

  html <- htmltools::renderTags(build_app_shell_ui(config = config))$html

  expect_match(html, "app_settings_drawer")
  expect_match(html, "settings-settings_panel")
  expect_match(html, "pannotator-drawer-layout-left")
  expect_match(html, "settings-appThemeMode", fixed = TRUE)
  expect_match(html, "Light / Dark Mode", fixed = TRUE)
  expect_match(html, settings_drawer_width(), fixed = TRUE)
})

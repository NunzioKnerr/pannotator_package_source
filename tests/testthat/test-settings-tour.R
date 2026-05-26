test_that("app tour steps target persistent shell selectors", {
  steps <- build_app_tour_steps(function(id) paste0("settings-", id))

  expect_true(is.data.frame(steps))
  expect_equal(
    steps$element,
    c(
      "#settings-settings_panel",
      "#settings-settings_main_section",
      "#panel-map",
      "#panel-image",
      "#panel-annotation",
      "#panel-annotation_table",
      "#settings-settings_lookups_section"
    )
  )
  expect_true(all(nzchar(steps$intro)))
})


test_that("app ui mounts the settings shell", {
  ui <- app_ui()
  html <- htmltools::renderTags(ui)$html

  expect_match(html, "settings-settings_panel")
  expect_match(html, "settings-main_settings_accordion")
  expect_match(html, "settings-settingsPlacement")
  expect_match(html, "settings-settingsDrawerPosition")
  expect_match(html, "settings-layout_schematic")
  expect_false(grepl("settings-toggle_settings_button", html, fixed = TRUE))
  expect_match(html, "settings-settings_body")
  expect_match(html, "settings-start_tour")
  expect_match(html, "settings-askGuidedTourOnStartup")
  expect_match(html, "settings-export_project_settings")
  expect_match(html, "settings-projectGoogleMapsApiKey")
  expect_match(html, "settings-settings_annotation_table_section")
})


test_that("startup guided tour prompt renders launch controls", {
  html <- htmltools::renderTags(
    guided_tour_startup_prompt_ui(function(id) paste0("settings-", id))
  )$html

  expect_match(html, "Guided Tour")
  expect_match(html, "settings-start_startup_tour")
  expect_match(html, "Start Guided Tour")
})


test_that("settings hide toggle only renders in below-panels mode", {
  drawer_html <- htmltools::renderTags(
    mod_settings_ui(
      "settings",
      config = merge_panel_config(list(settingsPlacement = "drawer"), data_path = tempdir()),
      display_mode = "drawer"
    )
  )$html
  bottom_html <- htmltools::renderTags(
    mod_settings_ui(
      "settings",
      config = merge_panel_config(list(settingsPlacement = "bottom"), data_path = tempdir()),
      display_mode = "bottom"
    )
  )$html

  expect_false(grepl("settings-toggle_settings_button", drawer_html, fixed = TRUE))
  expect_true(grepl("settings-toggle_settings_button", bottom_html, fixed = TRUE))
})


test_that("main settings use an accordion and drawer mode avoids nested tab scrolling", {
  html <- htmltools::renderTags(
    settings_main_tab_ui(
      function(id) paste0("settings-", id),
      config = merge_panel_config(list(), data_path = tempdir()),
      display_mode = "drawer"
    )
  )$html

  expect_match(html, "settings-main_settings_accordion")
  expect_match(html, "settings-settings_system_dependencies_section")
  expect_match(html, "settings-settings_project_settings_section")
  expect_match(html, "settings-settings_layout_section")
  expect_false(grepl("overflow-y: auto", settings_tab_section_style("drawer"), fixed = TRUE))
})

test_that("settings shell shows global controls and notices above tabs", {
  html <- htmltools::renderTags(mod_settings_ui("settings"))$html

  expect_match(html, "pannotator-settings-global")
  expect_match(html, "settings-showWorkflowGuidanceNotices")
  expect_match(html, "settings-clearAllButton")
  expect_match(html, "settings-applySettingsButton")
  expect_match(html, "settings-settings_notice")
  expect_match(html, "settings-settings_pending_action")
  expect_match(html, "settings-settings_tabs")

  global_pos <- regexpr("pannotator-settings-global", html, fixed = TRUE)[1]
  notice_pos <- regexpr("settings-settings_notice", html, fixed = TRUE)[1]
  pending_pos <- regexpr("settings-settings_pending_action", html, fixed = TRUE)[1]
  tabs_pos <- regexpr("settings-settings_tabs", html, fixed = TRUE)[1]

  expect_gt(global_pos, 0)
  expect_gt(notice_pos, 0)
  expect_gt(pending_pos, 0)
  expect_gt(tabs_pos, 0)
  expect_true(global_pos < tabs_pos)
  expect_true(notice_pos < tabs_pos)
  expect_true(pending_pos < tabs_pos)
})


test_that("layout schematic helper marks the selected placement", {
  html <- htmltools::renderTags(
    settings_layout_schematic_ui(
      selected_placement = "drawer",
      drawer_position = "left",
      ns = function(id) paste0("settings-", id)
    )
  )$html

  expect_match(html, "pannotator-layout-choice-grid")
  expect_match(html, "pannotator-layout-choice-active")
  expect_match(html, "pannotator-layout-choice-clickable")
  expect_match(html, "role=\"button\"", fixed = TRUE)
  expect_match(html, "tabindex=\"0\"", fixed = TRUE)
  expect_match(html, "input[name=&quot;settings-settingsPlacement&quot;][value=&quot;bottom&quot;]", fixed = TRUE)
  expect_match(html, "input[name=&quot;settings-settingsPlacement&quot;][value=&quot;drawer&quot;]", fixed = TRUE)
  expect_match(html, "Settings Drawer")
  expect_match(html, "Settings")
})


test_that("layout schematic reflects drawer side selection", {
  left_html <- htmltools::renderTags(
    settings_layout_schematic_ui(
      selected_placement = "drawer",
      drawer_position = "left"
    )
  )$html
  right_html <- htmltools::renderTags(
    settings_layout_schematic_ui(
      selected_placement = "drawer",
      drawer_position = "right"
    )
  )$html

  left_side_pos <- regexpr("pannotator-layout-swatch-side", left_html, fixed = TRUE)[1]
  left_main_pos <- regexpr("pannotator-layout-swatch-main", left_html, fixed = TRUE)[1]
  right_side_pos <- regexpr("pannotator-layout-swatch-side", right_html, fixed = TRUE)[1]
  right_main_pos <- regexpr("pannotator-layout-swatch-main", right_html, fixed = TRUE)[1]

  expect_gt(left_side_pos, 0)
  expect_gt(left_main_pos, 0)
  expect_gt(right_side_pos, 0)
  expect_gt(right_main_pos, 0)
  expect_true(left_side_pos < left_main_pos)
  expect_true(right_main_pos < right_side_pos)
})

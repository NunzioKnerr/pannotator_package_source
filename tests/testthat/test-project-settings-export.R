test_that("default project settings export name uses the project folder name", {
  config <- modifyList(
    default_app_config(tempdir()),
    list(projectFolder = file.path(tempdir(), "example-project"))
  )

  expect_equal(default_project_settings_export_name(config), "example-project.yml")
})


test_that("safe shiny roots falls back to a home root when volume discovery fails", {
  roots <- safe_shiny_roots(volume_getter = function() stop("WMIC unavailable"))

  expect_true(length(roots) >= 1)
  expect_true("Home" %in% names(roots))
  expect_true(nzchar(unname(roots[["Home"]])))
})


test_that("normalize project settings export path appends a yaml extension when needed", {
  save_selection <- data.frame(
    name = "example-project",
    type = "yml",
    datapath = file.path(tempdir(), "example-project"),
    stringsAsFactors = FALSE
  )

  export_path <- normalize_project_settings_export_path(save_selection)

  expect_match(export_path, "example-project[.]yml$")
})


test_that("normalize project settings export path preserves an existing extension", {
  save_selection <- data.frame(
    name = "example-project.yml",
    type = "yaml",
    datapath = file.path(tempdir(), "example-project.yml"),
    stringsAsFactors = FALSE
  )

  export_path <- normalize_project_settings_export_path(save_selection)

  expect_match(export_path, "example-project[.]yml$")
})


test_that("write project settings yaml exports a reusable merged config", {
  project_dir <- file.path(tempdir(), "export-project")
  export_file <- file.path(tempdir(), "exported-project.yml")
  config <- modifyList(
    default_app_config(project_dir),
    list(
      appTheme = "flatly",
      appThemeMode = "dark",
      mapPanelWidth = 6,
      panoPanelWidth = 4,
      formPanelWidth = 2,
      lookup2Enabled = TRUE,
      panelOrder = c("annotation", "map", "image", "annotation_table")
    )
  )

  export_path <- write_project_settings_yaml(config = config, file_path = export_file)
  exported_config <- merge_panel_config(configr::read.config(export_path))

  expect_true(file.exists(export_path))
  expect_equal(exported_config$appTheme, "flatly")
  expect_equal(exported_config$appThemeMode, "dark")
  expect_equal(exported_config$mapPanelWidth, 6)
  expect_true(exported_config$lookup2Enabled)
  expect_equal(
    exported_config$panelOrder,
    c("annotation", "map", "image", "annotation_table")
  )
})


test_that("project settings export config can point at a new project folder", {
  source_dir <- file.path(tempdir(), "source-project")
  export_dir <- file.path(tempdir(), "export-project")
  config <- default_app_config(source_dir)

  export_config <- build_project_settings_export_config(
    config = config,
    data_path = export_dir
  )

  expect_equal(
    normalizePath(export_config$projectFolder, winslash = "/", mustWork = FALSE),
    normalizePath(export_dir, winslash = "/", mustWork = FALSE)
  )
})


test_that("export project settings bundle copies project files and writes reusable yaml", {
  source_dir <- tempfile("source-project-")
  export_dir <- tempfile("export-project-")
  dir.create(source_dir, recursive = TRUE)

  config <- default_app_config(source_dir)
  for (file_name in c(
    config$usernameLookupFile,
    config$lookup1CsvFile,
    config$lookup2CsvFile,
    config$lookup3CsvFile,
    config$lookup4CsvFile
  )) {
    utils::write.csv(
      data.frame(display = "Label", value = "value", stringsAsFactors = FALSE),
      file.path(source_dir, file_name),
      row.names = FALSE
    )
  }

  for (file_name in c(
    config$lookup1HelpFile,
    config$lookup2HelpFile,
    config$lookup3HelpFile,
    config$lookup4HelpFile
  )) {
    writeLines("help", file.path(source_dir, file_name), useBytes = TRUE)
  }

  annotation_data <- create_user_dataframe()
  annotation_data <- rbind(
    annotation_data,
    data.frame(
      user = "Alice",
      id = 1,
      sourcekmz = "survey.kmz",
      imagefile = "image1.jpg",
      feature_type = "Point-map",
      radius = NA_real_,
      geometry = "",
      dd1 = "value",
      dd2 = "",
      dd3 = "",
      dd4 = "",
      stringsAsFactors = FALSE
    )
  )

  export_result <- export_project_settings_bundle(
    config = config,
    export_dir = export_dir,
    source_data_dir = source_dir,
    annotations_data = annotation_data
  )
  exported_config <- configr::read.config(export_result$yaml_path)

  expect_true(file.exists(export_result$yaml_path))
  expect_true(all(export_result$files$copied))
  expect_true(file.exists(file.path(export_dir, config$usernameLookupFile)))
  expect_true(file.exists(file.path(export_dir, config$lookup1CsvFile)))
  expect_true(file.exists(file.path(export_dir, config$lookup1HelpFile)))
  expect_equal(
    normalizePath(exported_config$projectFolder, winslash = "/", mustWork = FALSE),
    normalizePath(export_dir, winslash = "/", mustWork = FALSE)
  )
  expect_equal(
    normalize_annotation_dataframe(readRDS(file.path(export_dir, config$annotationsFile))),
    annotation_data
  )
})


test_that("export project settings bundle creates annotations file when none exists", {
  source_dir <- tempfile("source-project-")
  export_dir <- tempfile("export-project-")
  dir.create(source_dir, recursive = TRUE)

  config <- default_app_config(source_dir)
  export_result <- export_project_settings_bundle(
    config = config,
    export_dir = export_dir,
    source_data_dir = source_dir
  )
  annotations_path <- file.path(export_dir, config$annotationsFile)

  expect_true(file.exists(annotations_path))
  expect_equal(readRDS(annotations_path), create_user_dataframe())
  expect_true(export_result$files$copied[export_result$files$field == "annotationsFile"])
})


test_that("export project settings bundle honours a custom yaml file name", {
  source_dir <- tempfile("source-project-")
  export_dir <- tempfile("export-project-")
  dir.create(source_dir, recursive = TRUE)

  config <- default_app_config(source_dir)
  export_result <- export_project_settings_bundle(
    config = config,
    export_dir = export_dir,
    yaml_file_name = "custom-project-name.yaml",
    source_data_dir = source_dir
  )

  expect_equal(basename(export_result$yaml_path), "custom-project-name.yaml")
  expect_true(file.exists(file.path(export_dir, config$annotationsFile)))
})


test_that("google maps export key only overrides the exported yaml config", {
  project_dir <- file.path(tempdir(), "google-export-project")
  export_file <- file.path(tempdir(), "google-exported-project.yml")
  config <- modifyList(
    default_app_config(project_dir),
    list(
      mapPanelSource = "Esri.WorldImagery",
      mapAPIKey = ""
    )
  )

  export_config <- build_project_settings_export_config(
    config = config,
    google_maps_api_key = "abc123-secret"
  )
  export_path <- write_project_settings_yaml(
    config = config,
    file_path = export_file,
    google_maps_api_key = "abc123-secret"
  )
  exported_yaml <- configr::read.config(export_path)

  expect_equal(config$mapPanelSource, "Esri.WorldImagery")
  expect_equal(config$mapAPIKey, "")
  expect_equal(export_config$mapPanelSource, "Google.Maps")
  expect_equal(export_config$mapAPIKey, "abc123-secret")
  expect_equal(exported_yaml$mapPanelSource, "Google.Maps")
  expect_equal(exported_yaml$mapAPIKey, "abc123-secret")
})


test_that("blank google maps export key is ignored", {
  project_dir <- file.path(tempdir(), "blank-google-export")
  config <- modifyList(
    default_app_config(project_dir),
    list(mapPanelSource = "OpenStreetMap")
  )

  export_config <- build_project_settings_export_config(
    config = config,
    google_maps_api_key = "   "
  )

  expect_equal(export_config$mapPanelSource, "OpenStreetMap")
  expect_equal(export_config$mapAPIKey, "")
})


test_that("project settings run_app example uses a normalized path", {
  example_call <- project_settings_run_app_example(file.path(tempdir(), "example-project.yml"))

  expect_match(example_call, "run_app\\(projectSettingsFile = ")
  expect_match(example_call, "example-project[.]yml")
})


test_that("settings ui labels the project bundle export button", {
  html <- htmltools::renderTags(settings_project_settings_section_ui(shiny::NS("settings")))$html

  expect_match(html, "Export All Project Files")
  expect_match(html, "Save Project YAML And Supporting Files")
  expect_false(grepl("YAML file", html, fixed = TRUE))
  expect_match(html, "lookup CSVs")
  expect_match(html, "run_app\\(projectSettingsFile")
})

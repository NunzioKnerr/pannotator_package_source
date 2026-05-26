test_that("run_app keeps projectSettingsFile as an optional argument", {
  expect_true("projectSettingsFile" %in% names(formals(run_app)))
  expect_null(formals(run_app)$projectSettingsFile)
})


test_that("build_runtime_context loads an explicit project settings file", {
  project_dir <- tempfile("pannotator-project-")
  dir.create(project_dir, recursive = TRUE)
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)
  project_config_path <- file.path(project_dir, "test-project.yml")

  project_config <- default_app_config(normalizePath(data_dir, mustWork = FALSE))
  project_config$appTheme <- "flatly"
  project_config$appThemeMode <- "dark"
  project_config$lookup2Enabled <- TRUE

  configr::write.config(
    project_config,
    project_config_path,
    write.type = "yaml"
  )

  runtime_context <- build_runtime_context(
    projectSettingsFile = project_config_path,
    initialize = FALSE
  )

  expect_equal(runtime_context$source, "project")
  expect_equal(
    runtime_context$projectSettingsFile,
    normalizePath(project_config_path, mustWork = FALSE)
  )
  expect_equal(runtime_context$config$appTheme, "flatly")
  expect_equal(runtime_context$config$appThemeMode, "dark")
  expect_true(isTRUE(runtime_context$config$lookup2Enabled))
  expect_equal(
    runtime_context$data_dir,
    normalizePath(data_dir, mustWork = FALSE)
  )
})


test_that("current_runtime_context rereads the config file instead of using stale launch config", {
  project_dir <- tempfile("pannotator-project-")
  dir.create(project_dir, recursive = TRUE)
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)
  project_config_path <- file.path(project_dir, "test-project.yml")

  initial_config <- default_app_config(normalizePath(data_dir, mustWork = FALSE))
  initial_config$settingsPlacement <- "bottom"
  configr::write.config(
    initial_config,
    project_config_path,
    write.type = "yaml"
  )

  stale_context <- build_runtime_context(
    projectSettingsFile = project_config_path,
    initialize = FALSE
  )

  updated_config <- initial_config
  updated_config$settingsPlacement <- "drawer"
  configr::write.config(
    updated_config,
    project_config_path,
    write.type = "yaml"
  )

  refreshed_context <- current_runtime_context(
    initialize = FALSE,
    golem_options = list(
      projectSettingsFile = project_config_path,
      runtime_context = stale_context
    )
  )

  expect_equal(refreshed_context$config$settingsPlacement, "drawer")
})


test_that("create_app_state creates an isolated session runtime", {
  runtime_context <- build_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)

  expect_s3_class(app_state, "reactivevalues")
  expect_false(identical(app_state, r))
  expect_equal(shiny::isolate(app_state$config$appTheme), runtime_context$config$appTheme)
  expect_equal(shiny::isolate(app_state$runtime_context$source), runtime_context$source)
  expect_true(dir.exists(runtime_context$runtime_temp_dir))
})


test_that("runtime_config_value can read session config outside reactive consumers", {
  runtime_context <- build_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)

  expect_equal(runtime_config_value(app_state)$appTheme, runtime_context$config$appTheme)
})


test_that("runtime helpers derive session-specific KMZ paths and resource urls", {
  runtime_context <- build_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)

  synchronize_runtime_context(runtime_context, runtime = app_state)
  kmz_dir <- new_runtime_kmz_dir(app_state)

  expect_true(dir.exists(kmz_dir))
  expect_match(runtime_kml_path(app_state), "doc\\.kml$")
  expect_match(runtime_kmz_files_dir(app_state), "files$")

  image_url <- runtime_image_url("folder name/image 1.jpg", runtime = app_state)
  expect_match(image_url, "^/temp_dir/")
  expect_match(image_url, "folder%20name")
  expect_match(image_url, "image%201.jpg")
})


test_that("reset_loaded_kmz_workspace clears KMZ state but preserves session data", {
  runtime_context <- build_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)
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
      dd1 = "tree",
      dd2 = "",
      dd3 = "",
      dd4 = "",
      stringsAsFactors = FALSE
    )
  )

  app_state$user_name <- "Alice"
  app_state$user_annotations_data <- annotation_data
  app_state$current_kmz_name <- "survey.kmz"
  app_state$current_image <- "image1.jpg"
  app_state$current_image_metadata <- list(ImageWidth = 100, ImageHeight = 100)
  app_state$imgs_lst <- "image1.jpg"
  app_state$imgs_metadata <- data.frame(SourceFile = "image1.jpg")
  app_state$current_kmz_dir <- tempfile("kmz-")

  reset_loaded_kmz_workspace(app_state)

  expect_equal(shiny::isolate(app_state$user_name), "Alice")
  expect_equal(shiny::isolate(app_state$user_annotations_data), annotation_data)
  expect_null(shiny::isolate(app_state$current_kmz_name))
  expect_null(shiny::isolate(app_state$current_image))
  expect_null(shiny::isolate(app_state$imgs_lst))
  expect_null(shiny::isolate(app_state$current_kmz_dir))
  expect_match(shiny::isolate(app_state$map_panel_notice$title), "Workspace Reset")
  expect_true(nzchar(shiny::isolate(app_state$workspace_reset)))
})


test_that("reset_loaded_kmz_workspace supports context-specific messages", {
  runtime_context <- build_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)

  reset_loaded_kmz_workspace(
    app_state,
    message = "Workspace display settings changed. Reload a KMZ file.",
    title = "Settings Applied",
    type = "warning"
  )

  expect_equal(shiny::isolate(app_state$map_panel_notice$title), "Settings Applied")
  expect_equal(
    shiny::isolate(app_state$map_panel_notice$message),
    "Workspace display settings changed. Reload a KMZ file."
  )
  expect_equal(shiny::isolate(app_state$map_panel_notice$type), "warning")
})


test_that("app_ui reads runtime config without mutating compatibility globals", {
  on.exit(
    synchronize_runtime_context(build_runtime_context(initialize = TRUE)),
    add = TRUE
  )

  rm(list = ls(envir = myEnv), envir = myEnv)
  reset_runtime_state(r)

  ui <- app_ui()

  golem::expect_shinytaglist(ui)
  expect_false(exists("config", envir = myEnv, inherits = FALSE))
  expect_null(shiny::isolate(r$config))
})


test_that("synchronize_runtime_context mirrors bootstrap values for server compatibility", {
  on.exit(
    synchronize_runtime_context(build_runtime_context(initialize = TRUE)),
    add = TRUE
  )

  project_dir <- tempfile("pannotator-project-")
  dir.create(project_dir, recursive = TRUE)
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)
  project_config_path <- file.path(project_dir, "test-project.yml")

  project_config <- default_app_config(normalizePath(data_dir, mustWork = FALSE))
  project_config$appTheme <- "journal"
  project_config$appThemeMode <- "dark"

  configr::write.config(
    project_config,
    project_config_path,
    write.type = "yaml"
  )

  runtime_context <- build_runtime_context(
    projectSettingsFile = project_config_path,
    initialize = FALSE
  )
  synchronize_runtime_context(runtime_context)

  expect_equal(
    myEnv$project_config_file,
    normalizePath(project_config_path, mustWork = FALSE)
  )
  expect_equal(myEnv$data_dir, runtime_context$data_dir)
  expect_equal(myEnv$config$appTheme, "journal")
  expect_equal(myEnv$config$appThemeMode, "dark")
  expect_equal(shiny::isolate(r$config$appTheme), "journal")
  expect_equal(shiny::isolate(r$config$appThemeMode), "dark")
  expect_type(shiny::isolate(r$var_choices), "list")
  expect_type(shiny::isolate(r$mapIcons), "list")
  expect_type(shiny::isolate(r$formIcons), "list")
})

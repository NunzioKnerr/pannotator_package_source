test_that("settings csv editor specs expose username and lookup editors", {
  specs <- settings_csv_editor_specs()

  expect_true(all(c("username", "lookup1", "lookup2", "lookup3", "lookup4") %in% names(specs)))
  expect_equal(specs$username$columns, c("user_name", "value"))
  expect_equal(specs$lookup1$columns, c("display", "value"))
  expect_equal(specs$lookup1$output_id, "lookup1_csv_editor")
})


test_that("settings csv editor data is normalized to required columns", {
  normalized_data <- normalize_settings_csv_editor_data(
    data = data.frame(
      display = c("Tree", "  ", NA),
      extra = c("keep?", "drop", "drop"),
      stringsAsFactors = FALSE
    ),
    columns = c("display", "value")
  )

  expect_equal(names(normalized_data), c("display", "value"))
  expect_equal(nrow(normalized_data), 1)
  expect_equal(normalized_data$display[[1]], "Tree")
  expect_equal(normalized_data$value[[1]], "")
})


test_that("settings csv editor round-trips username data to disk", {
  spec <- settings_csv_editor_specs()$username
  config <- list(usernameLookupFile = "username_lookup.csv")
  data_dir <- tempdir()

  saved_data <- write_settings_csv_editor_data(
    data = data.frame(
      user_name = c("Alice Example", "Bob Example"),
      value = c("Alice_Example", "Bob_Example"),
      stringsAsFactors = FALSE
    ),
    spec = spec,
    config = config,
    data_dir = data_dir
  )

  loaded_data <- read_settings_csv_editor_data(
    spec = spec,
    config = config,
    data_dir = data_dir
  )

  expect_equal(saved_data, loaded_data)
})


test_that("lookup annotation values ignore blanks and missing values", {
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
      dd3 = NA_character_,
      dd4 = "NA",
      stringsAsFactors = FALSE
    )
  )

  expect_equal(lookup_annotation_values(annotation_data, 1), "tree")
  expect_equal(lookup_annotation_values(annotation_data, 2), character(0))
  expect_equal(lookup_annotation_values(annotation_data, 3), character(0))
  expect_equal(lookup_annotation_values(annotation_data, 4), character(0))
})


test_that("lookup csv validation blocks removing values used by annotations", {
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
      dd3 = "nest",
      dd4 = "",
      stringsAsFactors = FALSE
    )
  )

  config <- default_app_config(tempdir())
  config$lookup3Label <- "Habitat"
  spec <- settings_csv_editor_specs(config = config)$lookup3

  validation_result <- validate_settings_lookup_csv_change(
    annotation_data = annotation_data,
    spec = spec,
    table_data = data.frame(
      display = "Burrow",
      value = "burrow",
      stringsAsFactors = FALSE
    ),
    config = config
  )

  expect_false(validation_result$valid)
  expect_match(paste(validation_result$messages, collapse = " "), "Lookup 3")
  expect_match(paste(validation_result$messages, collapse = " "), "nest")
})


test_that("lookup settings validation blocks disabling populated lookup columns", {
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
      dd2 = "healthy",
      dd3 = "",
      dd4 = "",
      stringsAsFactors = FALSE
    )
  )

  data_dir <- tempdir()
  config <- default_app_config(data_dir)
  config$lookup2Enabled <- FALSE
  config$lookup2Label <- "Condition"

  validation_result <- validate_lookup_settings_against_annotations(
    annotation_data = annotation_data,
    config = config,
    data_dir = data_dir
  )

  expect_false(validation_result$valid)
  expect_match(paste(validation_result$messages, collapse = " "), "disabled")
  expect_match(paste(validation_result$messages, collapse = " "), "healthy")
})


test_that("lookup settings validation accepts lookup files that retain annotation values", {
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
      dd2 = "healthy",
      dd3 = "",
      dd4 = "",
      stringsAsFactors = FALSE
    )
  )

  data_dir <- tempfile("lookup-validation-")
  dir.create(data_dir, recursive = TRUE)
  utils::write.csv(
    data.frame(display = "Tree", value = "tree", stringsAsFactors = FALSE),
    file.path(data_dir, "lookup1.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(display = "Healthy", value = "healthy", stringsAsFactors = FALSE),
    file.path(data_dir, "lookup2.csv"),
    row.names = FALSE
  )

  config <- default_app_config(data_dir)
  config$lookup2Enabled <- TRUE

  validation_result <- validate_lookup_settings_against_annotations(
    annotation_data = annotation_data,
    config = config,
    data_dir = data_dir
  )

  expect_true(validation_result$valid)
  expect_equal(validation_result$messages, character(0))
})


test_that("settings ui includes username and lookup csv editor containers", {
  ui <- mod_settings_ui("settings")
  html <- htmltools::renderTags(ui)$html

  expect_match(html, "settings-lookup_accordion")
  expect_match(html, "settings-username_load_editor")
  expect_match(html, "settings-lookup1_load_editor")
  expect_match(html, "settings-username_lookup_editor")
  expect_match(html, "settings-lookup1_csv_editor")
  expect_match(html, "settings-lookup4_csv_editor")
})

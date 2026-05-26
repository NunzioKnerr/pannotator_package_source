test_that("annotation table view data includes enabled lookup columns", {
  config <- modifyList(
    default_app_config(tempdir()),
    list(
      lookup1Label = "Species",
      lookup2Enabled = TRUE,
      lookup2Label = "Condition",
      lookup3Enabled = FALSE,
      lookup4Enabled = FALSE
    )
  )

  annotation_data <- data.frame(
    user = "sam",
    id = "1",
    sourcekmz = "survey_01.kmz",
    imagefile = "IMG_001.JPG",
    feature_type = "Point-map",
    radius = NA_real_,
    geometry = "POINT(1 2)",
    dd1 = "tree",
    dd2 = "healthy",
    dd3 = "",
    dd4 = "",
    stringsAsFactors = FALSE
  )

  table_data <- annotation_table_view_data(
    annotation_data = annotation_data,
    config = config,
    current_image = "IMG_001.JPG",
    current_kmz_name = "survey_01.kmz",
    current_image_only = TRUE
  )

  expect_equal(
    names(table_data),
    c("user", "id", "sourcekmz", "imagefile", "feature_type", "radius", "geometry", "dd1", "dd2")
  )
  expect_equal(table_data$sourcekmz[[1]], "survey_01.kmz")
  expect_equal(table_data$imagefile[[1]], "IMG_001.JPG")
})


test_that("annotation table view data sorts newest annotation ids first", {
  annotation_data <- data.frame(
    user = "sam",
    id = c("2", "20260415-083000000", "10", "20260415-083500000", "no-id"),
    sourcekmz = "survey_01.kmz",
    imagefile = "IMG_001.JPG",
    feature_type = "Point-map",
    radius = NA_real_,
    geometry = "POINT(1 2)",
    dd1 = "tree",
    dd2 = "",
    dd3 = "",
    dd4 = "",
    stringsAsFactors = FALSE
  )

  table_data <- annotation_table_view_data(
    annotation_data = annotation_data,
    config = default_app_config(tempdir())
  )

  expect_equal(
    table_data$id,
    c("20260415-083500000", "20260415-083000000", "10", "2", "no-id")
  )
})


test_that("annotation table rhandsontable enables sorting and row highlighting", {
  skip_if_not_installed("rhandsontable")

  table_data <- annotation_table_view_data(
    annotation_data = data.frame(
      user = "sam",
      id = c("1", "2"),
      sourcekmz = "survey_01.kmz",
      imagefile = "IMG_001.JPG",
      feature_type = "Point-map",
      radius = NA_real_,
      geometry = "POINT(1 2)",
      dd1 = "tree",
      dd2 = "",
      dd3 = "",
      dd4 = "",
      stringsAsFactors = FALSE
    ),
    config = default_app_config(tempdir())
  )

  hot <- build_annotation_table_hot(
    table_data,
    config = default_app_config(tempdir()),
    lookup_choices = list(dd1 = list(Tree = "tree"))
  )

  expect_true(hot$x$columnSorting)
  expect_equal(hot$x$currentRowClassName, "currentRow")
  expect_equal(hot$x$currentColClassName, "currentCol")
})


test_that("annotation table edits update lookup columns by id", {
  config <- modifyList(
    default_app_config(tempdir()),
    list(
      lookup2Enabled = TRUE,
      lookup3Enabled = FALSE,
      lookup4Enabled = FALSE
    )
  )

  existing_data <- data.frame(
    user = "sam",
    id = c("1", "2"),
    sourcekmz = c("survey_01.kmz", "survey_01.kmz"),
    imagefile = c("IMG_001.JPG", "IMG_002.JPG"),
    feature_type = c("Point-map", "Polygon-360"),
    radius = c(NA_real_, NA_real_),
    geometry = c("POINT(1 2)", "POLYGON((1 1,2 2,3 3,1 1))"),
    dd1 = c("tree", "track"),
    dd2 = c("healthy", "rough"),
    dd3 = c("", ""),
    dd4 = c("", ""),
    stringsAsFactors = FALSE
  )

  edited_rows <- data.frame(
    user = "sam",
    id = c("2"),
    sourcekmz = "survey_01.kmz",
    imagefile = "IMG_002.JPG",
    feature_type = "Polygon-360",
    radius = NA_real_,
    geometry = "POLYGON((1 1,2 2,3 3,1 1))",
    dd1 = "road",
    dd2 = "smooth",
    stringsAsFactors = FALSE
  )

  updated_data <- apply_annotation_table_edits(
    existing_data = existing_data,
    edited_rows = edited_rows,
    config = config
  )

  expect_equal(updated_data$dd1[updated_data$id == "2"], "road")
  expect_equal(updated_data$dd2[updated_data$id == "2"], "smooth")
  expect_equal(updated_data$sourcekmz[updated_data$id == "2"], "survey_01.kmz")
  expect_equal(updated_data$geometry[updated_data$id == "2"], "POLYGON((1 1,2 2,3 3,1 1))")
})


test_that("annotation table current-image filter also uses source kmz when available", {
  annotation_data <- data.frame(
    user = c("sam", "sam"),
    id = c("1", "2"),
    sourcekmz = c("survey_01.kmz", "survey_02.kmz"),
    imagefile = c("IMG_001.JPG", "IMG_001.JPG"),
    feature_type = c("Point-map", "Point-map"),
    radius = c(NA_real_, NA_real_),
    geometry = c("POINT(1 2)", "POINT(3 4)"),
    dd1 = c("tree", "road"),
    dd2 = c("healthy", "rough"),
    dd3 = c("", ""),
    dd4 = c("", ""),
    stringsAsFactors = FALSE
  )

  table_data <- annotation_table_view_data(
    annotation_data = annotation_data,
    config = default_app_config(tempdir()),
    current_image = "IMG_001.JPG",
    current_kmz_name = "survey_02.kmz",
    current_image_only = TRUE
  )

  expect_equal(nrow(table_data), 1)
  expect_equal(table_data$sourcekmz[[1]], "survey_02.kmz")
  expect_equal(table_data$id[[1]], "2")
})


test_that("annotation table ui exposes the panel filter control", {
  ui <- mod_annotation_table_ui("annotation_table")
  html <- htmltools::renderTags(ui)$html

  expect_match(html, "annotation_table-current_image_only")
  expect_match(html, "annotation_table-annotation_table_container")
})

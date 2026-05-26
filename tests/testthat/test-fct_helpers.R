test_that("panel_notice renders a bootstrap alert", {
  notice_html <- htmltools::renderTags(
    panel_notice("Export Successful", "Annotations were written to disk.", type = "success")
  )$html

  expect_match(notice_html, "alert-success")
  expect_match(notice_html, "Export Successful")
  expect_match(notice_html, "Annotations were written to disk.")
})


test_that("base helper replacements preserve squish and file-read behavior", {
  temp_file <- tempfile(fileext = ".txt")
  writeBin(charToRaw("alpha\nbeta"), temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  expect_equal(squish_whitespace("  alpha\t beta \n gamma  "), "alpha beta gamma")
  expect_equal(read_text_file(temp_file), "alpha\nbeta")
})


test_that("main workspace viewers use a shared taller height", {
  expect_equal(main_workspace_viewer_height(), "780px")
})


test_that("shinyFiles close script targets the active chooser dialog", {
  close_script <- shinyfiles_close_dialog_script()

  expect_match(close_script, ".sF-modalContainer:visible", fixed = TRUE)
  expect_match(close_script, "#sF-cancelButton:visible", fixed = TRUE)
  expect_match(close_script, ".sF-modalBackdrop", fixed = TRUE)
})


test_that("load_lookup returns an empty list when the csv is missing", {
  expect_equal(
    load_lookup(
      fileToLoad = "missing_lookup.csv",
      display_column = "display",
      value_column = "value",
      data_dir = tempdir()
    ),
    list()
  )
})


test_that("annotation data schema includes sourcekmz and normalizes legacy data", {
  annotation_data <- create_user_dataframe()

  expect_equal(
    names(annotation_data),
    c("user", "id", "sourcekmz", "imagefile", "feature_type", "radius", "geometry", "dd1", "dd2", "dd3", "dd4")
  )

  legacy_data <- data.frame(
    user = "sam",
    id = "1",
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

  normalized_data <- normalize_annotation_dataframe(legacy_data)

  expect_true("sourcekmz" %in% names(normalized_data))
  expect_equal(normalized_data$sourcekmz[[1]], "")
})


test_that("annotation lookup can distinguish repeated image names by source kmz", {
  annotation_data <- data.frame(
    user = c("sam", "sam"),
    id = c("1", "2"),
    sourcekmz = c("first.kmz", "second.kmz"),
    imagefile = c("IMG_001.JPG", "IMG_001.JPG"),
    feature_type = c("Point-map", "Point-map"),
    radius = c(NA_real_, NA_real_),
    geometry = c("POINT(1 2)", "POINT(3 4)"),
    dd1 = c("tree", "road"),
    dd2 = c("", ""),
    dd3 = c("", ""),
    dd4 = c("", ""),
    stringsAsFactors = FALSE
  )

  filtered_data <- check_for_annotations(
    annotation_data,
    myCurrentImage = "IMG_001.JPG",
    mySourceKmz = "second.kmz"
  )

  expect_equal(nrow(filtered_data), 1)
  expect_equal(filtered_data$id[[1]], "2")
})


test_that("annotation card state can be added, updated, and removed", {
  annotation_cards <- upsert_annotation_card_state(
    annotation_cards = list(),
    myId = "card-1",
    myFeatureType = "Point-map",
    myGeometry = "POINT(1 2)",
    myRadius = NA,
    myDD1 = NA,
    myDD2 = "canopy",
    myDD3 = "",
    myDD4 = NULL
  )

  expect_named(annotation_cards, "card-1")
  expect_equal(annotation_cards[["card-1"]]$dd1, "")
  expect_equal(annotation_cards[["card-1"]]$dd2, "canopy")

  annotation_cards[["card-1"]]$collapsed <- TRUE

  annotation_cards <- upsert_annotation_card_state(
    annotation_cards = annotation_cards,
    myId = "card-1",
    myFeatureType = "Point-map",
    myGeometry = "POINT(1 2)",
    myRadius = NA,
    myDD1 = "tree",
    myDD2 = "canopy",
    myDD3 = "",
    myDD4 = NULL
  )

  expect_true(annotation_cards[["card-1"]]$collapsed)
  expect_equal(annotation_cards[["card-1"]]$dd1, "tree")

  annotation_cards <- remove_annotation_card_state(annotation_cards, "card-1")

  expect_length(annotation_cards, 0)
})


test_that("annotation card ui renders namespaced inputs and labels", {
  old_config <- myEnv$config
  old_dropdown1 <- myEnv$var_dropdown1
  old_dropdown2 <- myEnv$var_dropdown2
  old_dropdown3 <- myEnv$var_dropdown3
  old_dropdown4 <- myEnv$var_dropdown4
  old_form_icons <- myEnv$formIcons

  on.exit({
    myEnv$config <- old_config
    myEnv$var_dropdown1 <- old_dropdown1
    myEnv$var_dropdown2 <- old_dropdown2
    myEnv$var_dropdown3 <- old_dropdown3
    myEnv$var_dropdown4 <- old_dropdown4
    myEnv$formIcons <- old_form_icons
  }, add = TRUE)

  myEnv$config <- modifyList(
    default_app_config(tempdir()),
    list(
      lookup1Label = "Primary Lookup",
      lookup2Enabled = FALSE,
      lookup3Enabled = FALSE,
      lookup4Enabled = FALSE
    )
  )
  myEnv$var_dropdown1 <- list(Tree = "tree")
  myEnv$var_dropdown2 <- list()
  myEnv$var_dropdown3 <- list()
  myEnv$var_dropdown4 <- list()
  myEnv$formIcons <- list(
    wholeImageMapFormIcon = "<i class='whole-image'></i>",
    pointMapFormIcon = "<i class='point-map'></i>",
    polygonMapFormIcon = "<i class='polygon-map'></i>",
    point360FormIcon = "<i class='point-360'></i>",
    polygon360FormIcon = "<i class='polygon-360'></i>"
  )

  annotation_card_html <- htmltools::renderTags(
    build_annotation_card_ui(
      ns = function(id) paste0("control_form-", id),
      card = list(
        id = "card-42",
        feature_type = "Point-map",
        geometry = "POINT(1 2)",
        radius = NA,
        dd1 = "tree",
        dd2 = "",
        dd3 = "",
        dd4 = "",
        collapsed = FALSE
      )
    )
  )$html

  expect_match(annotation_card_html, "control_form-annotation_card_card-42")
  expect_match(annotation_card_html, "control_form-annotation_dropdown1_card-42")
  expect_match(annotation_card_html, "Point-map-Primary Lookup")
  expect_match(annotation_card_html, "point-map", fixed = TRUE)
  expect_match(annotation_card_html, "pannotator-card-actions")
  expect_match(annotation_card_html, "btn-outline-secondary")
  expect_match(annotation_card_html, "btn-outline-danger")
})

test_that("get_exiftool_status reports an installed ExifTool version", {
  status <- get_exiftool_status(
    package_available = TRUE,
    exif_version_fn = function(quiet = TRUE) "13.27",
    install_available = TRUE
  )

  expect_true(status$installed)
  expect_true(status$package_available)
  expect_true(status$can_install)
  expect_equal(status$version, "13.27")
  expect_match(status$message, "13.27")
})


test_that("get_exiftool_status reports missing ExifTool when the version check fails", {
  status <- get_exiftool_status(
    package_available = TRUE,
    exif_version_fn = function(quiet = TRUE) stop("ExifTool not found"),
    install_available = TRUE
  )

  expect_false(status$installed)
  expect_true(status$package_available)
  expect_true(status$can_install)
  expect_null(status$version)
  expect_match(status$message, "not currently available")
})


test_that("build_exiftool_status_ui shows an install button when ExifTool is missing", {
  html <- paste(
    htmltools::renderTags(
      build_exiftool_status_ui(
        ns = function(id) paste0("settings-", id),
        status = list(
          installed = FALSE,
          package_available = TRUE,
          can_install = TRUE,
          version = NULL,
          message = "ExifTool is not currently available on this system."
        )
      )
    )$html,
    collapse = ""
  )

  expect_match(html, "ExifTool Dependency")
  expect_match(html, "settings-check_exiftool")
  expect_match(html, "Install ExifTool")
  expect_match(html, "alert-warning")
})


test_that("build_exiftool_status_ui omits the install button when ExifTool is available", {
  html <- paste(
    htmltools::renderTags(
      build_exiftool_status_ui(
        ns = function(id) paste0("settings-", id),
        status = list(
          installed = TRUE,
          package_available = TRUE,
          can_install = TRUE,
          version = "13.27",
          message = "ExifTool is available (version 13.27). KMZ image metadata loading is ready."
        )
      )
    )$html,
    collapse = ""
  )

  expect_match(html, "ExifTool Dependency")
  expect_match(html, "settings-check_exiftool")
  expect_false(grepl("Install ExifTool", html, fixed = TRUE))
  expect_match(html, "alert-success")
})

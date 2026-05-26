#' ExifTool dependency helpers
#'
#' @noRd

get_exiftool_status <- function(
    package_available = requireNamespace("exiftoolr", quietly = TRUE),
    exif_version_fn = NULL,
    install_available = NULL
) {
  if (!isTRUE(package_available)) {
    return(list(
      installed = FALSE,
      package_available = FALSE,
      can_install = FALSE,
      version = NULL,
      message = "The 'exiftoolr' package is not available, so pannotator cannot check or install ExifTool from inside the app."
    ))
  }

  if (is.null(exif_version_fn)) {
    exif_version_fn <- exiftoolr::exif_version
  }

  if (is.null(install_available)) {
    install_available <- exists("install_exiftool", where = asNamespace("exiftoolr"), inherits = FALSE)
  }

  version_result <- tryCatch(
    exif_version_fn(quiet = TRUE),
    error = function(e) e
  )

  if (inherits(version_result, "error")) {
    return(list(
      installed = FALSE,
      package_available = TRUE,
      can_install = isTRUE(install_available),
      version = NULL,
      message = "ExifTool is not currently available on this system. KMZ image metadata loading needs ExifTool before it can read image metadata."
    ))
  }

  version_value <- as.character(version_result)[1]
  if (is.na(version_value) || !nzchar(trimws(version_value))) {
    return(list(
      installed = FALSE,
      package_available = TRUE,
      can_install = isTRUE(install_available),
      version = NULL,
      message = "ExifTool is not currently available on this system. KMZ image metadata loading needs ExifTool before it can read image metadata."
    ))
  }

  list(
    installed = TRUE,
    package_available = TRUE,
    can_install = isTRUE(install_available),
    version = trimws(version_value),
    message = paste0(
      "ExifTool is available (version ",
      trimws(version_value),
      "). KMZ image metadata loading is ready."
    )
  )
}


build_exiftool_status_ui <- function(ns, status = get_exiftool_status()) {
  alert_type <- if (isTRUE(status$installed)) {
    "success"
  } else if (isTRUE(status$package_available)) {
    "warning"
  } else {
    "danger"
  }

  div(
    id = ns("exiftool_dependency"),
    class = paste("alert", paste0("alert-", alert_type), "pannotator-inline-notice"),
    role = "status",
    tags$strong("ExifTool Dependency"),
    tags$p(status$message, style = "margin: 8px 0;"),
    tags$small(
      "Used by KMZ image metadata loading through the exiftoolr package.",
      style = "display: block; margin-bottom: 10px;"
    ),
    div(
      style = "display: flex; gap: 8px; flex-wrap: wrap;",
      actionButton(
        inputId = ns("check_exiftool"),
        label = "Check ExifTool",
        class = "btn btn-outline-secondary btn-sm"
      ),
      if (!isTRUE(status$installed) && isTRUE(status$package_available) && isTRUE(status$can_install)) {
        actionButton(
          inputId = ns("install_exiftool"),
          label = "Install ExifTool",
          class = "btn btn-primary btn-sm"
        )
      }
    )
  )
}

#' Tour helpers
#'
#' @noRd

build_app_tour_steps <- function(settings_ns) {
  data.frame(
    element = c(
      paste0("#", settings_ns("settings_panel")),
      paste0("#", settings_ns("settings_main_section")),
      "#panel-map",
      "#panel-image",
      "#panel-annotation",
      "#panel-annotation_table",
      paste0("#", settings_ns("settings_lookups_section"))
    ),
    intro = c(
      "Use this persistent settings area to configure layout, lookups, and app behavior.",
      "Main settings control panel widths, themes, and map/image styling.",
      "Load KMZ files and overlays here, then use the map to choose images and add map annotations.",
      "The image panel shows the current panorama and supports 360 drawing mode.",
      "The annotation panel is where you choose a user, export records, and manage annotation cards.",
      "The annotation table gives you a single place to review and edit annotation lookup values across images.",
      "Lookup settings let you customize the labels, files, and enabled lookup fields used by annotation forms."
    ),
    position = c("bottom", "bottom", "right", "left", "left", "top", "bottom"),
    stringsAsFactors = FALSE
  )
}

#' panel_host UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_host_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("panel_host"))
  )
}


#' panel_host Server Functions
#'
#' @noRd
mod_panel_host_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    output$panel_host <- renderUI({
      req(r$config)
      settings_ui <- NULL
      if (identical(settings_placement_value(r$config), "bottom")) {
        settings_ui <- mod_settings_ui(
          "settings",
          config = r$config,
          display_mode = "bottom"
        )
      }

      build_panel_host_ui(
        config = r$config,
        settings_ui = settings_ui
      )
    })

    list(
      panel_order = reactive(get_panel_order(config = r$config)),
      enabled_panels = reactive(unique(as.character(unlist(r$config$enabledPanels))))
    )
  })
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  runtime_context <- current_runtime_context(initialize = TRUE)
  app_state <- create_app_state(runtime_context)
  synchronize_runtime_context(runtime_context, runtime = app_state)
  session$userData$runtime_context <- runtime_context
  session$userData$app_state <- app_state

  shinyhelper::observe_helpers(help_dir = app_sys("/app/www/helpfiles"))

  #removeKmzFiles()

  mod_settings_server("settings", app_state)
  mod_panel_host_server("panel_host", app_state)
  mod_control_form_server("control_form", app_state)
  mod_leaflet_map_server("leaflet_map", app_state)
  mod_360_image_server("pano360_image", app_state)
  mod_annotation_table_server("annotation_table", app_state)
}

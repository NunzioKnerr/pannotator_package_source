#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    fluidPage(
      br(),
      fluidRow(
       # uiOutput("map_panel_ui"),
        #uiOutput("pano_panel_ui"),
       # uiOutput("form_panel_ui"),
         column(myEnv$config$mapPanelWidth, wellPanel(
           tags$h4("Mapping Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="map_panel", mod_leaflet_map_ui("leaflet_map"))),
         column(myEnv$config$panoPanelWidth, wellPanel(tags$h4("Image Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="image_panel", mod_360_image_ui("pano360_image"))),
         column(myEnv$config$formPanelWidth, wellPanel(tags$h4("Annotation Panel", style = "font-size: 13px; text-align: center; margin: 0;"), id="form_panel",  style = "padding: 20px;", mod_control_form_ui("control_form"), div(id = "add_here"), ))
      )
    )
  )

}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "extdata",
    app_sys("extdata")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pannotator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

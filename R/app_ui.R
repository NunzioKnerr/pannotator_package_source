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
    #print(the$config$mapPanelWidth),
    # Your application UI logic
    shinyjs::useShinyjs(),
    fluidPage(
      br(),
      fluidRow(
        column(the$config$mapPanelWidth,  wellPanel(id="map_panel",
                                                         # fluidRow(
                                                         #   column(4, align = "left", img(src = "www/UKTNP_logo.png", width = 125, alt = "UKTNP logo")),
                                                         #  column(4, align = "center", img(src = "www/360_icon_circle.png", width = 120, alt = "360 icon")),
                                                         #   column(4, align = "right", img(src = "www/csiro_logo_2020.png", width = 120, alt = "CSIRO logo"))),
                                                         mod_leaflet_map_ui("leaflet_map_1"))),

        column(the$config$panoPanelWidth, wellPanel(id="image_panel", mod_360_image_ui("360_image_1"))),
        column(the$config$formPanelWidth, wellPanel(id="form_panel", mod_control_form_ui("control_form_1"), div(id = "add_here"),#wellPanel(mod_annotation_form_ui("annotation_form_1"))
        ))
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
      app_title = "PannotatoR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

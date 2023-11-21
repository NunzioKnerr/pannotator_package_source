#' 360_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_360_image_ui <- function(id){
  ns <- NS(id)

  tagList(
   shiny::selectInput(ns("img_dd"), "Image To Annotate:", choices = "", selected = NULL)
  %>% shinyhelper::helper(type = "markdown", content = "image_loader", icon = "question-circle"),
    uiOutput(ns("pano_iframe"))
  )
}

#' 360_image Server Functions
#'
#' @noRd
mod_360_image_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #set the image dropdown to load when the kmz is unzipped
    observe({
      #changed this to autmaitcally select the first image as the pin dropping needs it to have one
      shiny::updateSelectInput(session = session, inputId = "img_dd", choices = r$imgs_lst, selected = r$imgs_lst[1])

      shinyWidgets::show_alert(
        title = "ALMOST SET.. Now annotate the images",
        text = "Select from the dropdown in the middle and use the buttons on the right to start annotating.",
        type = "success"
      )
    }) %>% bindEvent(r$imgs_lst)

    #setup to watch for a new image loaded into the 360 viewer and change the metadata
    observe({
    r$current_image <- input$img_dd
    #print(paste0(app_sys("/app/www/files/"),"/",input$img_dd))
    req(r$current_image)
    r$current_image_metadata <- get_image_metadata(r$imgs_metadata, r$current_image)
    req(r$active_modules)
    remove_annotations_form(r)
     }) %>% bindEvent(input$img_dd)

    output$pano_iframe <- renderUI({

      ## Construct the iframe URL
      src_url <- paste0("www/pannellum.htm#panorama=",
                       "files/",input$img_dd,
                        "&autoLoad=true&autoRotate=0&ignoreGPanoXMP=true")

      tags$iframe(src = utils::URLencode(src_url), width = "100%", height = "750px")#%>% shinyhelper::helper(type = "markdown", content = "image_loader", icon = "question-circle fa-lg")
      })
  })
}

## To be copied in the UI
# mod_360_image_ui("360_image_1")

## To be copied in the server
# mod_360_image_server("360_image_1")

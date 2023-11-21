#' leaflet_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'
mod_leaflet_map_ui <- function(id){
  ns <- NS(id)
  options(digits=12)
  options(shiny.maxRequestSize = 300*1024^2) #sets the file size to 300mb
  #shinyWidgets::useSweetAlert()
  #leaflet.extras::leafletExtrasDependencies

  tagList(
    fileInput(ns("kmz_file"), "Load A .kmz File:", accept=c(".kmz"), placeholder="NEXT: Select a google earth (.kmz) file...") |> shinyhelper::helper(type="markdown", content="kmz_file_loader", icon = "question-circle"),
    textOutput(ns("fileDetails")),
    shinyWidgets::progressBar(id = ns("pb1"), value = 0, title = ""),
    leaflet::leafletOutput(ns("mymap"), height = 750),
    fileInput(ns("overlay_file"), "Load An Overlay:", accept = c(".kml"), placeholder="Use this to add an overlay (.kml only...)")#,
  #textOutput(ns("position"))# for testing map pins
  )
}

#' leaflet_map Server Functions
#' @noRd
mod_leaflet_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #shinyjs::disable("kmz_file")
    shinyjs::hide("overlay_file")

    map <- observe({
      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 50, title = "Checking files in kmz & loading map.")
      files_extracted <- unzipKmz(input$kmz_file$datapath)
      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 75, title = "Loading image metadata")
      r$imgs_metadata <- load_image_metadata(app_sys("/app/www/files"))
      shinyWidgets::updateProgressBar(session = session, id = "pb1", value = 100, title = files_extracted)
      r$imgs_lst <- get_image_files(app_sys("/app/www/files"))

      fName <- paste0(app_sys("/app/www/doc.kml"))
      myKml <- readr::read_file(fName)

      output$mymap <- loadBaseMap(kml=myKml, r=r)
        shinyjs::show("overlay_file")

     }) %>% bindEvent(input$kmz_file)

#######################################################################

    #add pin_id number for counter when pins dropped
    r$pin_id <- 0
    # this works on create new features using the leaflet.extras::drawtoolbar
    observe({

      #uses conversion to deal with polygons. had issue with number of decimal places
      #TODO come back to this if we want to add polygons etc.
      #  rm(feature)
      #  rm(myMarker)
      #  rm(geom)
      #  feature <- input$mymap_draw_new_feature
      #  #View(feature)
      #
      # myMarker <- geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T, digits=9))
      # geom <- sf::st_as_text(myMarker$geometry, digits=9)
      # options(digits=9)
      # print(geom)
      #
      #  add_annotations_form(myActiveModules=r$active_modules, myModuleNumber=input$add_module, myFeatureType=feature$properties$feature_type, myGeometry=geom, mySpeciesName=0, myCoverEstimate=0, myImpactEstimate=-999)

    ##########################

       req(r)
       r$pin_id <- r$pin_id + 1
       feature <- input$mymap_draw_new_feature
       geom <- paste0(toupper(input$mymap_draw_new_feature$geometry$type)," (",input$mymap_draw_new_feature$geometry$coordinates[[1]], " ", input$mymap_draw_new_feature$geometry$coordinates[[2]],")")
       #
       add_annotations_form(myActiveModules=r$active_modules, myModuleNumber=paste0("pin_", r$pin_id), myFeatureType=input$mymap_draw_new_feature$properties$feature_type, myGeometry=geom, myDD1=NA, myDD2=NA, myDD3=NA, myDD4=NA)

     #########################
     #print(feature$properties$feature_type)
     #print(feature$properties$`_leaflet_id`)
     #print(feature$geometry$coordinates[[1]])
     #print(feature$geometry$coordinates[[2]])

     #print(feature)
     #print("############################")
#     print(sf::st_coordinates(feature))
#     nc$geom_str <- sf::st_as_text(feature)
#     print(nc$geom_str)
#     #POLYGON (130.8478, -25.29804, 130.8483, -25.29813, 130.8479, -25.29848, 130.8478, -25.29804)
#       if (feature$properties$feature_type == "marker") {
#         print("#######")
#         print(feature$geometry$coordinates[[1]])
#         print("#######")
#         print(feature$geometry$coordinates[[2]])
#
#       }
#     print("**********")
#     #print(str(feature))
#     #testing getting the geometry text to save in the rds
#      test <- geojsonsf::geojson_sf(jsonify::to_json(feature, unbox = T))
#      geom <- sf::st_as_text(test$geometry)
#      print(geom)
#      print(test)
# #     str(test)
# #     test$user <- r$user_name
# #     saveRDS(test, file="test.rds")
     })  %>% bindEvent(input$mymap_draw_new_feature)
#
#     #this works on edit from drawtoolbar
#     observeEvent(input$mymap_draw_edited_features, {
#       print("edited features")
#     })
#
#     #loaded shape/polygon triggered when clicked
# observeEvent(input$mymap_shape_click, {
#   print("loaded pin click")
# })

# observeEvent(input$mymap_marker_click, {
#   print("loaded pin click")
# })
#
#    #triggered whin a loaded marker had been mouseouted.... should work
     # observeEvent(input$mymap_marker_mouseout, {
     #   test <- input$mymap_marker_mouseout
     #   print(str(test))
     #   print("marker mouseout")
     # })

    #added functionality to load overlay to the map
    #if the current image viewed changes then centre on that
    observe({
      #myOverlayMap <- readr::read_file(input$overlay_file$datapath)
      addMapOverlay(input$overlay_file)

      shinyWidgets::show_alert(
        title = "Map Overlay Loaded!",
        text = "Added the map overlay to the map panel.",
        type = "success"
      )
     })%>% bindEvent(input$overlay_file)


    #r$current_image <- reactiveVal()

    #if the current image viewed changes then centre on that
     observe({
    #   #print("current image changed")
       req(r$current_image_metadata)
       addMapLayer(r=r)
      })%>% bindEvent(r$current_image)

    #####shinyFiles Way
    # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    # shinyFiles::shinyFileChoose(input, "kmzfile", roots = volumes, session = session)

    #eventReactive(input$kmzfile, {
    #test <- file_read(input$kmzfile, mode = "r", filter = NULL)
    #d <- tempfile()
    #archive::archive_extract(input$kmzfile$datapath, dir = d, files = NULL)
    #list.files(d)
    #utils::unzip(input$kmzfile$datapath, files = NULL, exdir = ".")
    #})

    # observeEvent(input$kmzfile,
    #              output$zipped <- renderTable({
    #                utils::unzip(input$kmzfile$datapath, list = FALSE, exdir = paste0(app_sys("/app/www"))
    #              })
    # )
    # observe({
    #   cat("\ninput$file value:\n\n")
    #   print(input$kmzfile)
    #  })
    #
    # ## print to browser
    # output$filepaths <- renderPrint({
    #   if (is.integer(input$kmzfile)) {
    #     cat("No files have been selected (shinyFileChoose)")
    #   } else {
    #     shinyFiles::parseFilePaths(volumes, input$kmzfile)
    #   }
    # })

    ##########

    return(
      list(kmzLoaded = reactive({input$kmz_file}))
    )
  })
}

## To be copied in the UI
# mod_leaflet_map_ui("leaflet_map_1")

## To be copied in the server
# mod_leaflet_map_server("leaflet_map_1")

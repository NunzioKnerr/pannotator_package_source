#' helpers
#'
#' @description This function takes a kmz file and unzips it so that the kml can be loaded and the images are available for use
#'
#' @param kmzFile The kmz file to extract.
#'
#' @return A count of the number of images extracted for use
#'
#' @noRd
#'
#'
#'
#'
save_user_config <- function(usr_config){
  #print("user config saved!")
  configr::write.config(config.dat = r$config, file.path = file.path(paste0(app_sys("extdata"), "/user-config.yml")), write.type = "yaml", indent = 4)
  return("user config saved!")
}

unzipKmz <- function(kmzFile){
  kmlFile <- app_sys("/app/www/doc.kml")
  filesFolder <- app_sys("/app/www/files")
  if(file.exists(kmlFile)){
    #print("A kmz has previously been loaded.....")
    unlink(kmlFile)
    unlink(filesFolder, recursive = TRUE)
    #print("The files have been deleted")
  }

  #print("Unzipping new kmz file")
  utils::unzip(kmzFile, list = FALSE, exdir = app_sys("/app/www"))

  num_files <- length(list.files(filesFolder))
  #print(paste0(num_files, " image files extracted"))
  return(paste0(num_files, " image files extracted"))
}

removeKmzFiles <- function(){
  print("Removing old kmz files...")
  unlink(app_sys("/app/www/doc.kml"), force = TRUE)
  unlink(app_sys("/app/www/files"), recursive = TRUE, force = TRUE)
}

#function to load the base map
loadBaseMap <- function(kml, r){
  mymap <- leaflet::renderLeaflet({
    #print(paste0("leaflet::providers$", the$config$user$mapPanelSource))
    leaflet::leaflet() %>%
    leaflet::addProviderTiles(eval(parse(text=paste0("leaflet::providers$", the$config$mapPanelSource)))) %>%
    leaflet.extras::addKML(kml, layerId = "my_kml", group ="360 Images" ,  markerType = "circleMarker",
                           stroke = FALSE, fillColor = "yellow", fillOpacity = 1,
                           markerOptions = leaflet::markerOptions(interactive = TRUE, clickable = TRUE, radius = 5, riseOnHover = TRUE, riseOffset = 250), labelProperty = "name") %>%
      leaflet::addLayersControl(overlayGroups = c("360 Images"), options = leaflet::layersControlOptions(collapsed = FALSE))
  })
  return(mymap)
}

#adds a map overlay to the map for fire scars etc.
addMapOverlay <- function(overlayMap){
  myOverlayMap <- readr::read_file(overlayMap$datapath)
  myMapProxy <- leaflet::leafletProxy("mymap") %>%
    leaflet.extras::addKMLChoropleth(
      myOverlayMap, layerId = "my_overlay",
      valueProperty = NULL,
      color = "#a6f31f", weight = 5, fillOpacity = 0.5)
  return(myMapProxy)
}

addMapLayer <- function(r=r){
  r$annotation_markers <- r$user_annotations_data %>%
    dplyr::filter(imagefile == r$current_image & feature_type %in% c("marker")) %>%
    sf::st_as_sf(., wkt = "geometry")

    lat <- as.numeric(paste0(r$current_image_metadata$Latitude))
    long <- as.numeric(paste0(r$current_image_metadata$Longitude))

  myMapProxy <- leaflet::leafletProxy("mymap") %>%
    leaflet::setView(lng = long, lat = lat, zoom=17) %>%
    leaflet::clearMarkers() %>%
    leaflet::addCircleMarkers(lng = long, lat = lat, fillColor = "darkviolet", radius=12, fillOpacity = 0.1, stroke = T, color = "#03F", weight = 3, opacity = 0.4) %>%
    leaflet::addMarkers(data = r$annotation_markers, group = "editable") %>%
    leaflet.extras::addDrawToolbar(targetGroup = "temp", polylineOptions = FALSE, polygonOptions= FALSE, circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE, editOptions = leaflet.extras::editToolbarOptions())%>%
    leaflet::addMeasure(position = "topright",  primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479")#

  return(myMapProxy)
}

#function to get image files from folder
get_image_files <- function(folderToUse){
  imgs_fn <- list.files(folderToUse, pattern = "JPG$|JPEG$", ignore.case = TRUE, recursive = FALSE, full.names = FALSE)
  golem::invoke_js("showid", "image_panel")
  return(imgs_fn)
}

#function to use exiftools to read image metadata
load_image_metadata <- function(directory){
  file_extension <- "\\.jpg$"
  my_files <- list.files(directory, pattern=paste0(file_extension), all.files=FALSE, full.names=TRUE)
  files_df <- exiftoolr::exif_read(path=my_files, tags = c("-G1", "-a", "-s"))
  #View(files_df)
  return(files_df)
}

get_image_metadata <- function(files_df, imageToGet){
  #print("get_image_metadata called")
  #print(imageToGet)
  colnames(files_df)[which(colnames(files_df)=="FileName")] <- "FileName"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitudeRef")] <- "LatitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitude")] <- "Latitude"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitudeRef")] <- "LongitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitude")] <- "Longitude"
  newdata <- files_df[which(files_df$FileName==imageToGet),]
  #View(newdata)
  return(newdata)
}

#generic function to load lookups to populate dropdown selects from a csv file
load_lookup <- function(fileToLoad, display_column, value_column){
  #print("load lookup called")
  #print(paste0("fileToLoad: ", fileToLoad))
  lookup <- utils::read.csv(file = fileToLoad, header = TRUE, sep = ',')
  my_list <- list()
  for(i in 1:nrow(lookup)) {
    my_list[[i]] <- lookup[i, value_column]
    names(my_list)[i] <- lookup[i, display_column]
  }
  return(my_list)
}

#setup function to remove modules
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

#function to check for saved data
check_for_saved_data <- function(dataFileToFind){
  #print(paste0("looking for: ", dataFileToFind))
  if(file.exists(dataFileToFind)){
    #print("file found!")
    dataFile <- readRDS(dataFileToFind)
  } else {
    #print("No Saved User Data - creating New File!")
    dataFile <- create_user_dataframe()
  }
  return(dataFile)
}

#function to create blank data file
#updated version for pin/polygon drawing
create_user_dataframe <- function(){
  df <- data.frame(user=character(),timestamp=double(),imagefile=character(),leaflet_id=character(),feature_type=character(),radius=numeric(),geometry=character(),dd1=character(),dd2=character(),dd3=character(),dd4=character(),stringsAsFactors=FALSE)

  return(df)
}


add_annotation_data <- function(myUserAnnotationsData, myUser, myTimeStamp, myImage, myLeafletId, myFeatureType, myGeometry, myDD1, myDD2, myDD3, myDD4){

  newdf <- rbind(myUserAnnotationsData, data.frame(user = myUser, timestamp = myTimeStamp, leaflet_id = myLeafletId, imagefile = myImage, feature_type = myFeatureType, radius=NA, geometry = myGeometry, dd1 = myDD1, dd2 = myDD2, dd3 = myDD3, dd4 = myDD4))

  return(newdf)
}

check_for_annotations <- function(myUserAnnotationsData, myCurrentImage){
  newdata <- myUserAnnotationsData[ which(myUserAnnotationsData$imagefile==myCurrentImage), ]
  #str(newdata)
  return(newdata)
}

delete_annotations <- function(myUserAnnotationsData, myCurrentImage){
  newdata <- myUserAnnotationsData[-which(myUserAnnotationsData$imagefile==myCurrentImage), ]
  #print(nrow(newdata))
  #str(newdata)
  if(nrow(newdata > 0)){
    return(newdata)
  } else {
    return(myUserAnnotationsData)
  }

}

save_annotations <- function(myAnnotations, myAnnotationFileName){
  saveRDS(myAnnotations, file = myAnnotationFileName)
}


#this function clears the last annotation from the form
remove_last_annotation <- function(r){
  # only remove a module if there is at least one module shown
  if (length(r$active_modules()) > 0) {
    current_id <- r$active_modules()[1]

    removeUI(
      selector = paste0("#control_form_1-",current_id, "_leaflet_id-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_leaflet_id")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_feature_type-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_feature_type")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_geometry-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_geometry")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown1")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown1-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown2")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown2-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown3")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown3-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown4")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown4-label")
    )

    # remove the inputs on the server side
    # remove_shiny_inputs(
    #    id = current_id,
    #    .input = paste0("#", current_id, "_species_name")
    #   #input
    # )
    # update the list of currently shown modules
    r$active_modules(r$active_modules()[-1])
  }

}


#this function clear all the annotations
remove_annotations_form <- function(r){
  # only remove a module if there is at least one module shown
  for(i in 1:length(r$active_modules())){
    current_id <- r$active_modules()[1]
    #print(current_id)
    #to remove all with one line
    # removeUI(
    #    selector = paste0("div:has(> #control_form_1-id")
    # )
    #########################

    removeUI(
      selector = paste0("#control_form_1-",current_id, "_leaflet_id-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_leaflet_id")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_feature_type-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_feature_type")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_geometry-label")
    )
    removeUI(
      selector = paste0("#control_form_1-",current_id, "_geometry")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown1")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown1-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown2")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown2-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown3")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown3-label")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown4")
    )
    removeUI(
      selector = paste0("#control_form_1-", current_id, "_dropdown4-label")
    )


    # remove the inputs on the server side
    # remove_shiny_inputs(
    #    id = current_id,
    #    .input = paste0("#", current_id, "_species_name")
    # )

    # update the list of currently shown modules
    r$active_modules(r$active_modules()[-1])
  }
}

add_annotations_form <- function(myActiveModules, myModuleNumber, myFeatureType, myGeometry, myDD1, myDD2, myDD3, myDD4){
  current_id <- myModuleNumber
  myActiveModules(c(current_id, myActiveModules()))
  #print(myActiveModules)

  insertUI(
    selector = "#add_here",
    ui =  textInput(
      inputId = paste0("control_form_1-",current_id, "_leaflet_id"),
      label = NULL,
      value = paste0(current_id)
    ) %>% shinyjs::hidden()
  )
  insertUI(
    selector = "#add_here",
    ui =  textInput(
      inputId = paste0("control_form_1-",current_id, "_feature_type"),
      label = NULL,
      value = paste0(myFeatureType)
    ) %>% shinyjs::hidden()
  )
  insertUI(
    selector = "#add_here",
    ui =  textInput(
      inputId =  paste0("control_form_1-",current_id, "_geometry"),
      label = NULL,
      value = paste0(myGeometry)
    ) %>% shinyjs::hidden()
  )
  insertUI(
    selector = "#add_here",
    ui = selectInput(
      inputId =  paste0("control_form_1-",current_id, "_dropdown1"),
      label = paste0(myFeatureType, "-", the$config$lookup1Label),
      choices = the$var_dropdown1,
      selected = myDD1,
      multiple = FALSE,
      selectize = FALSE,
      width = NULL,
      size = NULL
    )
  )
  if(the$config$lookup2Enabled == TRUE){
    insertUI(
      selector = "#add_here",
      ui = selectInput(
        inputId =  paste0("control_form_1-",current_id, "_dropdown2"),
        label = the$config$lookup2Label,
        choices = the$var_dropdown2,
        selected = myDD2,
        multiple = FALSE,
        selectize = FALSE,
        width = NULL,
        size = NULL
      )
    )}
  if(the$config$lookup3Enabled == TRUE){
    insertUI(
      selector = "#add_here",
      ui = selectInput(
        inputId =  paste0("control_form_1-",current_id, "_dropdown3"),
        label = the$config$lookup3Label,
        choices = the$var_dropdown3,
        selected = myDD3,
        multiple = FALSE,
        selectize = FALSE,
        width = NULL,
        size = NULL
      )
    )}
  if(the$config$lookup4Enabled == TRUE){
    insertUI(
      selector = "#add_here",
      ui =   selectInput(
        inputId =  paste0("control_form_1-",current_id, "_dropdown4"),
        label = the$config$lookup4Label,
        choices = the$var_dropdown4,
        selected = myDD4,
        multiple = FALSE,
        selectize = FALSE,
        width = NULL,
        size = NULL
      )
    ) }
}

# get_vignette_link <- function(...) {
#   x <- vignette(...)
#   if (nzchar(out <- x$PDF)) {
#     ext <- tools::file_ext(out)
#     port <- if (tolower(ext) == "html")
#       tools::startDynamicHelp(NA)
#     else 0L
#     if (port > 0L) {
#       out <- sprintf("http://127.0.0.1:%d/library/%s/doc/%s",
#                      port, basename(x$Dir), out)
#       return(out)
#     }
#   }
#   stop("no html help found")
# }

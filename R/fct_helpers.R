#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#'

# save the user config
save_user_config <- function(config_var, runtime = r){
  #print("user config saved!")
  config <- shiny::isolate(runtime$config)
  req(config)
  runtime$config <- merge_panel_config(config, data_path = runtime_data_dir(runtime))
  update_runtime_context_config(runtime)
  configr::write.config(
    config.dat = runtime$config,
    file.path = runtime_config_path(runtime),
    write.type = "yaml",
    indent = 4
  )
  return("user config saved!")
}

# called on clicking the 'Apply Settings' button in the settings form
refresh_user_config <- function(session, runtime = r){
  #print("refreshing user config")
  #session$reload()
  #Use runjs to run JavaScript code for reloading the page
  #shinyjs::runjs('window.location.reload();')
  runtime$refresh_user_config <- utils::timestamp()
  return("user config refreshed!")
}

squish_whitespace <- function(value) {
  gsub("[[:space:]]+", " ", trimws(as.character(value)))
}

read_text_file <- function(file_path) {
  file_info <- file.info(file_path)
  if (is.na(file_info$size) || file_info$size <= 0) {
    return("")
  }

  file_connection <- file(file_path, open = "rb")
  on.exit(close(file_connection), add = TRUE)

  rawToChar(readBin(file_connection, what = "raw", n = file_info$size))
}

main_workspace_viewer_height <- function() {
  "780px"
}

shinyfiles_close_dialog_script <- function() {
  paste(
    "(function() {",
    "  var $modal = $('.sF-modalContainer:visible').first();",
    "  if (!$modal.length) {",
    "    return;",
    "  }",
    "  var $cancel = $modal.find('#sF-cancelButton:visible').first();",
    "  if ($cancel.length) {",
    "    $cancel.trigger('click');",
    "    return;",
    "  }",
    "  var $backdrop = $modal.data('backdrop');",
    "  $modal.remove();",
    "  if ($backdrop && $backdrop.length) {",
    "    $backdrop.remove();",
    "  }",
    "  $('.sF-modalBackdrop').remove();",
    "  $('body').removeClass('modal-open');",
    "})();",
    sep = "\n"
  )
}

close_shinyfiles_dialog <- function() {
  shinyjs::runjs(shinyfiles_close_dialog_script())
  invisible(NULL)
}

# get image files from folder
get_image_files <- function(folderToUse){
  imgs_fn <- list.files(folderToUse, pattern = "JPG$|JPEG$", ignore.case = TRUE, recursive = FALSE, full.names = FALSE)
  #golem::invoke_js("showid", "image_panel")
  return(imgs_fn)
}

# use exiftools to read image metadata
load_image_metadata <- function(directory){
  file_extension <- "\\.jpg$"
  my_files <- list.files(directory, pattern=paste0(file_extension), all.files=FALSE, full.names=TRUE)
  files_df <- exiftoolr::exif_read(path=my_files, tags = c("-G1", "-a", "-s"))
  #View(files_df)
  return(files_df)
}

# get the Image meta data
get_image_metadata <- function(files_df, imageToGet){
  #print("get_image_metadata called")
  #print(imageToGet)
  colnames(files_df)[which(colnames(files_df)=="FileName")] <- "FileName"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitudeRef")] <- "GPSLatitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLatitude")] <- "GPSLatitude"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitudeRef")] <- "GPSLongitudeRef"
  colnames(files_df)[which(colnames(files_df)=="GPSLongitude")] <- "GPSLongitude"
  newdata <- files_df[which(files_df$FileName==imageToGet),]
  #View(newdata)
  return(newdata)
}

# Write latitude and longitude metadata back to an image
write_image_gps_metadata <- function(image_file, latitude, latitude_ref, longitude, longitude_ref) {
  #print("write_image_metadata called")
  # Construct the commands to update the GPS metadata
  gps_latitude_command <- paste0("-GPSLatitude=", latitude)
  gps_latitude_ref_command <- paste0("-GPSLatitudeRef=", latitude_ref)
  gps_longitude_command <- paste0("-GPSLongitude=", longitude)
  gps_longitude_ref_command <- paste0("-GPSLongitudeRef=", longitude_ref)

  # Execute the ExifTool command to update the image's GPS metadata
  exiftoolr::exif_call(
    args = c( "-overwrite_original",
              gps_latitude_command,
              gps_latitude_ref_command,
              gps_longitude_command,
              gps_longitude_ref_command,
              image_file
    )
  )
}

# generic function to load lookups to populate dropdown selects from a csv file
load_lookup <- function(fileToLoad, display_column, value_column, data_dir = myEnv$data_dir){
  #print("load lookup called")
  #print(paste0("fileToLoad: ", fileToLoad))
  full_file_path <- normalizePath(file.path(data_dir, fileToLoad), mustWork = FALSE)
  if (!file.exists(full_file_path)) {
    return(list())
  }
  #full_file_path <- normalizePath(fileToLoad, mustWork = TRUE)
  #print(full_file_path)
  lookup <- utils::read.csv(file = full_file_path, header = TRUE, sep = ',')
  my_list <- list()
  for(i in 1:nrow(lookup)) {
    my_list[[i]] <- lookup[i, value_column]
    names(my_list)[i] <- lookup[i, display_column]
  }
  return(my_list)
}

# check for saved annotations data
check_for_saved_data <- function(dataFileToFind){
  #print(paste0("looking for: ", dataFileToFind))
  if(file.exists(dataFileToFind)){
    #print("file found!")
    dataFile <- normalize_annotation_dataframe(readRDS(dataFileToFind))
  } else {
    #print("No Saved User Data - creating New File!")
    dataFile <- create_user_dataframe()
  }
  return(dataFile)
}

# normalize annotation data to the current schema
normalize_annotation_dataframe <- function(annotation_data) {
  if (is.null(annotation_data)) {
    return(create_user_dataframe())
  }

  normalized_data <- as.data.frame(annotation_data, stringsAsFactors = FALSE, check.names = FALSE)
  template <- create_user_dataframe()
  required_columns <- names(template)
  row_count <- nrow(normalized_data)

  for (column_name in required_columns) {
    if (!column_name %in% names(normalized_data)) {
      normalized_data[[column_name]] <- if (is.character(template[[column_name]])) {
        rep("", row_count)
      } else if (is.integer(template[[column_name]])) {
        rep(NA_integer_, row_count)
      } else if (is.double(template[[column_name]])) {
        rep(NA_real_, row_count)
      } else if (is.logical(template[[column_name]])) {
        rep(NA, row_count)
      } else {
        rep(NA, row_count)
      }
    }
  }

  normalized_data <- normalized_data[, required_columns, drop = FALSE]
  rownames(normalized_data) <- NULL
  normalized_data
}

# create blank annotation data file
create_user_dataframe <- function(){
  df <- data.frame(
    user = character(),
    id = double(),
    sourcekmz = character(),
    imagefile = character(),
    feature_type = character(),
    radius = numeric(),
    geometry = character(),
    dd1 = character(),
    dd2 = character(),
    dd3 = character(),
    dd4 = character(),
    stringsAsFactors = FALSE
  )

  return(df)
}

# check for annotations on image dropdown change
check_for_annotations <- function(myUserAnnotationsData, myCurrentImage, mySourceKmz = NULL){
  myUserAnnotationsData <- normalize_annotation_dataframe(myUserAnnotationsData)
  newdata <- myUserAnnotationsData[which(myUserAnnotationsData$imagefile==myCurrentImage), , drop = FALSE]

  if (!is.null(mySourceKmz) && nzchar(mySourceKmz) && "sourcekmz" %in% names(newdata)) {
    source_values <- trimws(as.character(newdata$sourcekmz))
    newdata <- newdata[source_values == mySourceKmz | !nzchar(source_values), , drop = FALSE]
  }

  #utils::str(newdata)
  return(newdata)
}

# edit annotations data
edit_annotation_data <- function(myUserAnnotationsData, myId,
                                 myUser = NA, myImage = NA,
                                 mySourceKmz = NA,
                                 myFeatureType = NA,
                                 myRadius = NA, myGeometry = NA,
                                 myDD1 = NA, myDD2 = NA, myDD3 = NA, myDD4 = NA) {
  myUserAnnotationsData <- normalize_annotation_dataframe(myUserAnnotationsData)

  # Identify the row to update
  row_to_update <- myUserAnnotationsData$id == myId

  # Function to check if a parameter was provided (is not NA)
  is_provided <- function(x) !is.na(x)

  # Update values only if they are provided
  if (is_provided(myUser)) myUserAnnotationsData[row_to_update, "user"] <- myUser
  if (is_provided(myImage)) myUserAnnotationsData[row_to_update, "imagefile"] <- myImage
  if (is_provided(mySourceKmz)) myUserAnnotationsData[row_to_update, "sourcekmz"] <- mySourceKmz
  if (is_provided(myFeatureType)) myUserAnnotationsData[row_to_update, "feature_type"] <- myFeatureType
  if (is_provided(myRadius)) myUserAnnotationsData[row_to_update, "radius"] <- myRadius
  if (is_provided(myGeometry)) myUserAnnotationsData[row_to_update, "geometry"] <- myGeometry
  if (is_provided(myDD1)) myUserAnnotationsData[row_to_update, "dd1"] <- myDD1
  if (is_provided(myDD2)) myUserAnnotationsData[row_to_update, "dd2"] <- myDD2
  if (is_provided(myDD3)) myUserAnnotationsData[row_to_update, "dd3"] <- myDD3
  if (is_provided(myDD4)) myUserAnnotationsData[row_to_update, "dd4"] <- myDD4

  # Check if the row exists to update or a new row needs to be added
  if (!any(row_to_update)) {
    # Create a new row with provided values, using NA for unspecified fields
    new_values <- data.frame(
      user = myUser,
      id = myId,
      sourcekmz = mySourceKmz,
      imagefile = myImage,
      feature_type = myFeatureType,
      radius = myRadius,
      geometry = myGeometry,
      dd1 = myDD1,
      dd2 = myDD2,
      dd3 = myDD3,
      dd4 = myDD4,
      stringsAsFactors = FALSE
    )
    myUserAnnotationsData <- rbind(myUserAnnotationsData, new_values)
    warning("No matching ID found. Adding as a new row instead.")
  }

  return(normalize_annotation_dataframe(myUserAnnotationsData))
}

# delete annotations from data frame
delete_annotation_data <- function(myUserAnnotationsData, myId) {
  # Filter out the rows where the id matches the specified value
  newdf <- myUserAnnotationsData[myUserAnnotationsData$id != myId,]
  return(newdf)
}

# clear all annotations from data frame
clear_all_annotation_data <- function(myUserAnnotationsData) {
  # remove all rows
  newdf <- myUserAnnotationsData[0,]
  return(newdf)
}

# save annotations to file
save_annotations <- function(myAnnotations, myAnnotationFileName){
  saveRDS(myAnnotations, file = myAnnotationFileName)
}

# reusable inline notice UI
panel_notice <- function(title, message, type = "info") {
  bootstrap_type <- switch(
    type,
    success = "success",
    warning = "warning",
    danger = "danger",
    "info"
  )

  div(
    class = paste("alert", paste0("alert-", bootstrap_type), "pannotator-inline-notice"),
    role = "status",
    tags$strong(title),
    div(message)
  )
}


annotation_value_or_blank <- function(value) {
  if (length(value) == 0 || all(is.na(value))) {
    return("")
  }

  value <- paste0(value[[1]])
  if (!nzchar(value) || identical(value, "NA")) {
    return("")
  }

  value
}


annotation_selected_value <- function(value) {
  selected_value <- annotation_value_or_blank(value)
  if (!nzchar(selected_value)) {
    return(NULL)
  }

  selected_value
}


annotation_card_icon_html <- function(feature_type, form_icons = myEnv$formIcons) {
  if (identical(feature_type, "Point-whole-image-annotation")) {
    return(form_icons$wholeImageMapFormIcon)
  }

  if (identical(feature_type, "Point-map")) {
    return(form_icons$pointMapFormIcon)
  }

  if (identical(feature_type, "Polygon-map")) {
    return(form_icons$polygonMapFormIcon)
  }

  if (identical(feature_type, "Point-360")) {
    return(form_icons$point360FormIcon)
  }

  if (identical(feature_type, "Polygon-360")) {
    return(form_icons$polygon360FormIcon)
  }

  form_icons$wholeImageMapFormIcon
}


upsert_annotation_card_state <- function(annotation_cards, myId, myFeatureType,
                                         myGeometry, myRadius, myDD1, myDD2,
                                         myDD3, myDD4) {
  if (is.null(annotation_cards)) {
    annotation_cards <- list()
  }

  key <- as.character(myId)
  collapsed <- FALSE
  if (!is.null(annotation_cards[[key]]) && isTRUE(annotation_cards[[key]]$collapsed)) {
    collapsed <- TRUE
  }

  annotation_cards[[key]] <- list(
    id = key,
    feature_type = paste0(myFeatureType),
    geometry = paste0(myGeometry),
    radius = myRadius,
    dd1 = annotation_value_or_blank(myDD1),
    dd2 = annotation_value_or_blank(myDD2),
    dd3 = annotation_value_or_blank(myDD3),
    dd4 = annotation_value_or_blank(myDD4),
    collapsed = collapsed
  )

  annotation_cards
}


remove_annotation_card_state <- function(annotation_cards, myId) {
  if (is.null(annotation_cards)) {
    return(list())
  }

  annotation_cards[[as.character(myId)]] <- NULL
  annotation_cards
}


update_annotation_card_field <- function(r, myId, field, value) {
  annotation_cards <- r$annotation_cards
  key <- as.character(myId)
  if (is.null(annotation_cards[[key]])) {
    return(invisible(NULL))
  }

  annotation_cards[[key]][[field]] <- value
  r$annotation_cards <- annotation_cards
  invisible(NULL)
}


toggle_annotation_card_state <- function(r, myId) {
  annotation_cards <- r$annotation_cards
  key <- as.character(myId)
  if (is.null(annotation_cards[[key]])) {
    return(invisible(NULL))
  }

  annotation_cards[[key]]$collapsed <- !isTRUE(annotation_cards[[key]]$collapsed)
  r$annotation_cards <- annotation_cards
  invisible(NULL)
}


destroy_annotation_card_observers <- function(r, myId = NULL) {
  if (is.null(myId)) {
    observer_ids <- names(r$annotation_card_observers)
  } else {
    observer_ids <- as.character(myId)
  }

  for (observer_id in observer_ids) {
    observer_set <- r$annotation_card_observers[[observer_id]]
    if (is.null(observer_set)) {
      next
    }

    for (observer in observer_set) {
      observer$destroy()
    }

    r$annotation_card_observers[[observer_id]] <- NULL
  }

  invisible(NULL)
}


build_annotation_card_ui <- function(
    ns,
    card,
    config = myEnv$config,
    lookup_choices = list(
      dd1 = myEnv$var_dropdown1,
      dd2 = myEnv$var_dropdown2,
      dd3 = myEnv$var_dropdown3,
      dd4 = myEnv$var_dropdown4
    ),
    form_icons = myEnv$formIcons
) {
  if (is.null(card)) {
    return(NULL)
  }

  body_style <- if (isTRUE(card$collapsed)) "display: none;" else NULL
  collapse_icon <- if (isTRUE(card$collapsed)) "chevron-down" else "chevron-up"

  div(
    id = ns(paste0("annotation_card_", card$id)),
    class = "pannotator-annotation-card",
    bslib::card(
      title = paste0(card$feature_type),
      div(
        style = "display: flex; justify-content: space-between; align-items: center; gap: 12px; margin-bottom: 12px;",
        div(
          HTML(annotation_card_icon_html(card$feature_type, form_icons = form_icons)),
          tags$span(card$id, style = "margin-left: 6px; vertical-align: middle;")
        ),
        div(
          class = "pannotator-card-actions",
          actionButton(
            inputId = ns(paste0("annotation_collapse_", card$id)),
            label = "",
            icon = icon(collapse_icon),
            class = "btn btn-outline-secondary btn-sm pannotator-card-action",
            title = if (isTRUE(card$collapsed)) "Expand annotation" else "Collapse annotation"
          ),
          actionButton(
            inputId = ns(paste0("annotation_close_", card$id)),
            label = "",
            icon = icon("trash"),
            class = "btn btn-outline-danger btn-sm pannotator-card-action",
            title = "Delete annotation"
          )
        )
      ),
      div(
        id = ns(paste0("annotation_body_", card$id)),
        style = body_style,
        selectInput(
          inputId = ns(paste0("annotation_dropdown1_", card$id)),
          label = paste0(card$feature_type, "-", config$lookup1Label),
          choices = lookup_choices$dd1,
          selected = annotation_selected_value(card$dd1),
          multiple = FALSE,
          selectize = FALSE
        ),
        if (isTRUE(config$lookup2Enabled)) {
          selectInput(
            inputId = ns(paste0("annotation_dropdown2_", card$id)),
            label = paste0(card$feature_type, "-", config$lookup2Label),
            choices = lookup_choices$dd2,
            selected = annotation_selected_value(card$dd2),
            multiple = FALSE,
            selectize = FALSE
          )
        },
        if (isTRUE(config$lookup3Enabled)) {
          selectInput(
            inputId = ns(paste0("annotation_dropdown3_", card$id)),
            label = paste0(card$feature_type, "-", config$lookup3Label),
            choices = lookup_choices$dd3,
            selected = annotation_selected_value(card$dd3),
            multiple = FALSE,
            selectize = FALSE
          )
        },
        if (isTRUE(config$lookup4Enabled)) {
          selectInput(
            inputId = ns(paste0("annotation_dropdown4_", card$id)),
            label = paste0(card$feature_type, "-", config$lookup4Label),
            choices = lookup_choices$dd4,
            selected = annotation_selected_value(card$dd4),
            multiple = FALSE,
            selectize = FALSE
          )
        }
      ),
      style = "overflow: visible; min-height: 50px;"
    )
  )
}


register_annotation_card_observers <- function(input, r, myId, config = r$config) {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    stop("register_annotation_card_observers() must be called from a Shiny reactive domain.", call. = FALSE)
  }

  key <- as.character(myId)
  observer_set <- r$annotation_card_observers[[key]]
  if (is.null(observer_set)) {
    observer_set <- list()
  }

  if (is.null(observer_set$close)) {
    observer_set$close <- observeEvent(input[[paste0("annotation_close_", key)]], ignoreInit = TRUE, {
      current_annotations <- r$active_annotations()
      if (is.null(current_annotations)) {
        current_annotations <- character(0)
      }

      updated_annotations <- current_annotations[current_annotations != key]
      if (length(updated_annotations) == 0) {
        updated_annotations <- NULL
      }

      r$active_annotations(updated_annotations)
      r$annotation_cards <- remove_annotation_card_state(r$annotation_cards, key)
      r$user_annotations_data <- delete_annotation_data(r$user_annotations_data, key)
      r$remove_leafletMap_item <- key
      r$remove_leaflet360_item <- key
      destroy_annotation_card_observers(r, key)
    })
  }

  if (is.null(observer_set$collapse)) {
    observer_set$collapse <- observeEvent(input[[paste0("annotation_collapse_", key)]], ignoreInit = TRUE, {
      toggle_annotation_card_state(r, key)
    })
  }

  if (is.null(observer_set$dropdown1)) {
    observer_set$dropdown1 <- observeEvent(input[[paste0("annotation_dropdown1_", key)]], ignoreInit = TRUE, {
      new_value <- annotation_value_or_blank(input[[paste0("annotation_dropdown1_", key)]])
      update_annotation_card_field(r, key, "dd1", new_value)
      r$user_annotations_data <- edit_annotation_data(
        myUserAnnotationsData = r$user_annotations_data,
        myId = key,
        myDD1 = new_value
      )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    })
  }

  if (isTRUE(config$lookup2Enabled) && is.null(observer_set$dropdown2)) {
    observer_set$dropdown2 <- observeEvent(input[[paste0("annotation_dropdown2_", key)]], ignoreInit = TRUE, {
      new_value <- annotation_value_or_blank(input[[paste0("annotation_dropdown2_", key)]])
      update_annotation_card_field(r, key, "dd2", new_value)
      r$user_annotations_data <- edit_annotation_data(
        myUserAnnotationsData = r$user_annotations_data,
        myId = key,
        myDD2 = new_value
      )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    })
  }

  if (isTRUE(config$lookup3Enabled) && is.null(observer_set$dropdown3)) {
    observer_set$dropdown3 <- observeEvent(input[[paste0("annotation_dropdown3_", key)]], ignoreInit = TRUE, {
      new_value <- annotation_value_or_blank(input[[paste0("annotation_dropdown3_", key)]])
      update_annotation_card_field(r, key, "dd3", new_value)
      r$user_annotations_data <- edit_annotation_data(
        myUserAnnotationsData = r$user_annotations_data,
        myId = key,
        myDD3 = new_value
      )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    })
  }

  if (isTRUE(config$lookup4Enabled) && is.null(observer_set$dropdown4)) {
    observer_set$dropdown4 <- observeEvent(input[[paste0("annotation_dropdown4_", key)]], ignoreInit = TRUE, {
      new_value <- annotation_value_or_blank(input[[paste0("annotation_dropdown4_", key)]])
      update_annotation_card_field(r, key, "dd4", new_value)
      r$user_annotations_data <- edit_annotation_data(
        myUserAnnotationsData = r$user_annotations_data,
        myId = key,
        myDD4 = new_value
      )
      save_annotations(
        myAnnotations = r$user_annotations_data,
        myAnnotationFileName = r$user_annotations_file_name
      )
    })
  }

  r$annotation_card_observers[[key]] <- observer_set
  invisible(NULL)
}


render_annotation_cards_ui <- function(
    session,
    annotation_cards,
    active_annotations,
    config = myEnv$config,
    lookup_choices = list(
      dd1 = myEnv$var_dropdown1,
      dd2 = myEnv$var_dropdown2,
      dd3 = myEnv$var_dropdown3,
      dd4 = myEnv$var_dropdown4
    ),
    form_icons = myEnv$formIcons
) {
  annotation_ids <- active_annotations()
  if (is.null(annotation_ids) || length(annotation_ids) == 0) {
    return(NULL)
  }

  tagList(
    lapply(annotation_ids, function(annotation_id) {
      build_annotation_card_ui(
        ns = session$ns,
        card = annotation_cards[[as.character(annotation_id)]],
        config = config,
        lookup_choices = lookup_choices,
        form_icons = form_icons
      )
    })
  )
}


# add a new annotation to the control form
add_annotations_form <- function(input, myActiveAnnotations, myId, myFeatureType, myGeometry, myRadius, myDD1, myDD2, myDD3, myDD4, runtime = r){
  current_annotations <- myActiveAnnotations()
  if (is.null(current_annotations)) {
    current_annotations <- character(0)
  }

  current_annotations <- as.character(current_annotations)
  current_annotations <- current_annotations[current_annotations != as.character(myId)]
  myActiveAnnotations(c(as.character(myId), current_annotations))

  runtime$annotation_cards <- upsert_annotation_card_state(
    annotation_cards = runtime$annotation_cards,
    myId = myId,
    myFeatureType = myFeatureType,
    myGeometry = myGeometry,
    myRadius = myRadius,
    myDD1 = myDD1,
    myDD2 = myDD2,
    myDD3 = myDD3,
    myDD4 = myDD4
  )

  register_annotation_card_observers(input = input, r = runtime, myId = myId, config = runtime$config)

  runtime$user_annotations_data <- edit_annotation_data(
    myUserAnnotationsData = runtime$user_annotations_data,
    myUser = runtime$user_name,
    myId = myId,
    mySourceKmz = runtime$current_kmz_name,
    myImage = runtime$current_image,
    myFeatureType = paste0(myFeatureType),
    myGeometry = myGeometry,
    myDD1 = myDD1,
    myDD2 = myDD2,
    myDD3 = myDD3,
    myDD4 = myDD4
  )
}


# clear all annotations from the form NOT the data frame
clear_annotations_form <- function(runtime = r) {
  destroy_annotation_card_observers(runtime)
  runtime$annotation_cards <- list()
  runtime$active_annotations(NULL)
}

# Functions for mapping panel ----

# CHANGE: Replaces leaflet.extras KML rendering with sf + leaflet so we can
# remove leaflet.extras while keeping map behavior in both Google and non-Google modes.
read_kml_to_sf <- function(kml_input, is_file = FALSE) {
  if (is.null(kml_input) || !nzchar(kml_input)) {
    return(NULL)
  }

  kml_path <- kml_input
  if (!isTRUE(is_file)) {
    kml_path <- tempfile(fileext = ".kml")
    writeLines(kml_input, con = kml_path, useBytes = TRUE)
    on.exit(unlink(kml_path), add = TRUE)
  }

  if (!file.exists(kml_path)) {
    return(NULL)
  }

  layer_names <- tryCatch(sf::st_layers(kml_path)$name, error = function(e) character(0))

  sf_layers <- if (length(layer_names) > 0) {
    lapply(layer_names, function(layer_name) {
      tryCatch(sf::st_read(kml_path, layer = layer_name, quiet = TRUE), error = function(e) NULL)
    })
  } else {
    list(tryCatch(sf::st_read(kml_path, quiet = TRUE), error = function(e) NULL))
  }

  sf_layers <- sf_layers[!vapply(sf_layers, is.null, logical(1))]
  if (length(sf_layers) == 0) {
    return(NULL)
  }

  sf_obj <- sf_layers[[1]]
  if (length(sf_layers) > 1) {
    for (i in 2:length(sf_layers)) {
      sf_obj <- dplyr::bind_rows(sf_obj, sf_layers[[i]])
    }
  }

  if (nrow(sf_obj) == 0) {
    return(NULL)
  }

  # CHANGE: Drop Z/M dimensions from KML geometries to avoid leaflet bbox/limits
  # issues when overlays include 3D coordinates.
  sf_obj <- tryCatch(sf::st_zm(sf_obj, drop = TRUE, what = "ZM"), error = function(e) sf_obj)

  crs_obj <- sf::st_crs(sf_obj)
  # CHANGE: Force lon/lat CRS for leaflet even when EPSG is missing but CRS exists.
  if (!is.na(crs_obj) && !sf::st_is_longlat(sf_obj)) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }

  sf_obj
}

# CHANGE: Shared KML-to-leaflet renderer used by both base map and overlay map paths.
add_kml_layer <- function(map, kml_input, group,
                          layer_id_prefix = "kml",
                          label_property = "name",
                          point_color = "yellow",
                          point_fill_color = "yellow",
                          point_fill_opacity = 1,
                          point_radius = 5,
                          point_stroke = FALSE,
                          line_color = "#a6f31f",
                          line_weight = 5,
                          polygon_color = "#a6f31f",
                          polygon_weight = 5,
                          polygon_fill_opacity = 0.5,
                          show_point_labels = TRUE) {
  sf_obj <- read_kml_to_sf(kml_input)
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return(map)
  }

  id_column <- if (label_property %in% names(sf_obj)) label_property else NULL
  if (is.null(id_column) && "Name" %in% names(sf_obj)) {
    id_column <- "Name"
  }

  layer_ids <- if (!is.null(id_column)) as.character(sf_obj[[id_column]]) else rep(NA_character_, nrow(sf_obj))
  missing_id <- is.na(layer_ids) | layer_ids == ""
  layer_ids[missing_id] <- paste0(layer_id_prefix, "-", which(missing_id))

  geom_types <- as.character(sf::st_geometry_type(sf_obj, by_geometry = TRUE))
  is_point <- grepl("POINT", geom_types)
  is_line <- grepl("LINESTRING", geom_types)
  is_polygon <- grepl("POLYGON", geom_types)
  non_empty <- !sf::st_is_empty(sf_obj)
  drawable <- non_empty

  if (any(is_point & drawable)) {
    point_data <- sf_obj[is_point & drawable, ]
    point_ids <- layer_ids[is_point & drawable]
    point_labels <- if (isTRUE(show_point_labels)) point_ids else NULL

    map <- tryCatch(
      map |> leaflet::addCircleMarkers(
        data = point_data,
        layerId = point_ids,
        group = group,
        color = point_color,
        stroke = point_stroke,
        fillColor = point_fill_color,
        fillOpacity = point_fill_opacity,
        radius = point_radius,
        label = point_labels
      ),
      error = function(e) map
    )
  }

  if (any(is_line & drawable)) {
    # CHANGE: Guard against malformed KML line geometries that can break
    # leaflet::addPolylines via bbox expansion.
    line_data <- sf_obj[is_line & drawable, ]
    # CHANGE: Add each line feature independently so a single malformed line
    # does not prevent other polylines in the same KML from rendering.
    for (i in seq_len(nrow(line_data))) {
      map <- tryCatch(
        map |> leaflet::addPolylines(
          data = line_data[i, , drop = FALSE],
          # CHANGE: Do not set layerId for line overlays because repeated KML
          # names can overwrite prior features when IDs collide.
          group = group,
          color = line_color,
          weight = line_weight
        ),
        error = function(e) map
      )
    }
  }

  if (any(is_polygon & drawable)) {
    polygon_data <- sf_obj[is_polygon & drawable, ]
    # CHANGE: Add each polygon independently for the same resilience reason as lines.
    for (i in seq_len(nrow(polygon_data))) {
      map <- tryCatch(
        map |> leaflet::addPolygons(
          data = polygon_data[i, , drop = FALSE],
          # CHANGE: Do not set layerId for polygon overlays because repeated KML
          # names can overwrite prior features when IDs collide.
          group = group,
          color = polygon_color,
          weight = polygon_weight,
          fillColor = polygon_color,
          fillOpacity = polygon_fill_opacity
        ),
        error = function(e) map
      )
    }
  }

  map
}

unzipKmz <- function(kmzFile, runtime = r){
  kmz_dir <- new_runtime_kmz_dir(runtime)

  # Unzip the new KMZ file into a fresh session-specific KMZ directory.
  utils::unzip(kmzFile, list = FALSE, exdir = kmz_dir)

  filesFolder <- runtime_kmz_files_dir(runtime)
  num_files <- length(list.files(filesFolder))
  #print(paste0(num_files, " image files extracted"))
  return(paste0(num_files, " image files extracted"))
}

# removeKmzFiles <- function(){
#   #print("Removing old kmz files...")
#   unlink("/temp_dir/doc.kml", force = TRUE)
#   unlink("/temp_dir/files", recursive = TRUE, force = TRUE)
# }

#adds a map overlay to the map for fire scars etc.
addMapOverlay <- function(overlayMap){
  myOverlayMap <- read_text_file(overlayMap$datapath)
  myMapProxy <- leaflet::leafletProxy("mymap") |>
    # CHANGE: Keep a single overlay render by clearing prior overlay features.
    leaflet::clearGroup("Overlay") |>
    # CHANGE: Previously used leaflet.extras::addKMLChoropleth.
    add_kml_layer(
      kml_input = myOverlayMap,
      group = "Overlay",
      layer_id_prefix = "Overlay",
      point_color = "#a6f31f",
      point_fill_color = "#a6f31f",
      point_fill_opacity = 0.7,
      point_radius = 5,
      point_stroke = FALSE,
      line_color = "#a6f31f",
      line_weight = 5,
      polygon_color = "#a6f31f",
      polygon_weight = 5,
      polygon_fill_opacity = 0.5,
      show_point_labels = FALSE
    ) |>
    leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))
  return(myMapProxy)
}


# Function to load the base map with three groups: '360 images', 'points', and 'polygons'
loadBaseLeafletMap <- function(kml = "", config = myEnv$config) {

  if(config$mapPanelSource == "Google.Maps"){
    #print(myEnv$config$mapPanelSource)

    mymap <- leaflet::renderLeaflet({
      #print("loadBaseLeafletMap called")
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 18)) |>
        leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) |>
        leaflet::addTiles(urlTemplate = paste0("https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}&key=", config$mapAPIKey, maxZoom = 18), attribution = 'Map data &copy; Google') |>
        # CHANGE: Previously used leaflet.extras::addKML for KMZ image markers.
        add_kml_layer(
          kml_input = kml,
          group = "360-Images",
          layer_id_prefix = "my_kml",
          label_property = "name",
          point_color = "yellow",
          point_fill_color = "yellow",
          point_fill_opacity = 1,
          point_radius = 5,
          point_stroke = FALSE,
          line_color = "yellow",
          line_weight = 2,
          polygon_color = "yellow",
          polygon_weight = 2,
          polygon_fill_opacity = 0.4,
          show_point_labels = TRUE
        ) |>
        leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                             toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                       drawPolygon = TRUE,
                                                                       drawPolyline = FALSE,
                                                                       drawCircle = FALSE,
                                                                       editMode = TRUE,
                                                                       cutPolygon = FALSE,
                                                                       removalMode = FALSE
                             ),
                             drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                             editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                                 allowSelfIntersection = FALSE, draggable = FALSE,
                                                                 preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
        ) |>
        leafpm::removePmToolbar() |>
        leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))

    })
  } else {
    mymap <- leaflet::renderLeaflet({
      #print("loadBaseLeafletMap called")
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2, maxZoom = 17)) |>
        leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) |>
        leaflet::addProviderTiles(eval(parse(text=paste0("leaflet::providers$", config$mapPanelSource)))) |>
        # CHANGE: Previously used leaflet.extras::addKML for KMZ image markers.
        add_kml_layer(
          kml_input = kml,
          group = "360-Images",
          layer_id_prefix = "my_kml",
          label_property = "name",
          point_color = "yellow",
          point_fill_color = "yellow",
          point_fill_opacity = 1,
          point_radius = 5,
          point_stroke = FALSE,
          line_color = "yellow",
          line_weight = 2,
          polygon_color = "yellow",
          polygon_weight = 2,
          polygon_fill_opacity = 0.4,
          show_point_labels = TRUE
        ) |>
        leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                             toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                       drawPolygon = TRUE,
                                                                       drawPolyline = FALSE,
                                                                       drawCircle = FALSE,
                                                                       editMode = TRUE,
                                                                       cutPolygon = FALSE,
                                                                       removalMode = FALSE
                             ),
                             drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                             editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                                 allowSelfIntersection = FALSE, draggable = FALSE,
                                                                 preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
        ) |>
        leafpm::removePmToolbar() |>
        leaflet::addLayersControl(overlayGroups = c("360-Images", "Overlay", "Whole-Image-Annotations", "Map-Annotations"), options = leaflet::layersControlOptions(collapsed = TRUE))

    })
  }

  return(mymap)
}

# triggered to add the current image to the map
addCurrentImageToMap <- function(runtime = r){
  #print("addCurrentImageToMap called")
  req(runtime$current_image_metadata, runtime$current_map_zoom)

  lat <- as.numeric(paste0(runtime$current_image_metadata$GPSLatitude))
  long <- as.numeric(paste0(runtime$current_image_metadata$GPSLongitude))
  zoom <- as.numeric(runtime$current_map_zoom)

  myMapProxy <- leaflet::leafletProxy("mymap") |>
    # CHANGE: Do not clear all markers here, because KMZ image markers are now
    # leaflet markers (not a separate geojson layer) and must remain visible.
    leaflet::removeMarker(layerId = "currentImage") |> # remove the purple cirlce marker
    leaflet::clearGroup("Map-Annotations") |>
    leaflet::clearGroup("Whole-Image-Annotations") |>
    leaflet::setView(lng = long, lat = lat, zoom = zoom) |>
    leaflet::addCircleMarkers(lng = long, lat = lat, layerId = "currentImage", group= "360-Images", fillColor = "darkviolet", radius=12, fillOpacity = 0.1, stroke = T, color = "#03F", weight = 3, opacity = 0.4) |>
    leafpm::addPmToolbar(targetGroup = "Map-Annotations",
                         toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                   drawPolygon = TRUE,
                                                                   drawPolyline = FALSE,
                                                                   drawCircle = FALSE,
                                                                   editMode = TRUE,
                                                                   cutPolygon = FALSE,
                                                                   removalMode = FALSE
                         ),
                         drawOptions = leafpm::pmDrawOptions(snappable = FALSE),
                         editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                             allowSelfIntersection = FALSE, draggable = FALSE,
                                                             preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
    ) |>
    leaflet::addMeasure(position = "topright",  primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", activeColor = "#3D535D", completedColor = "#7D4479") |>
    leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)

  return(myMapProxy)
}

# clears annotations from the map on first draw with the toolbar
clear_drawn_annotation_from_map <- function(session, layerId) {
  #print("clear_drawn_annotation_from_leaflet called")
  session$sendCustomMessage("removeleaflet", list(elid = "leaflet_map-mymap", layerId = layerId))
}

# add annotation to map
add_annotations_to_map <- function(runtime = r, config = NULL, map_icons = NULL, form_icons = NULL){
  #print("add_annotations_to_map called")
  if (is.null(config)) {
    config <- runtime$config
  }
  if (is.null(map_icons)) {
    map_icons <- runtime$mapIcons
  }
  if (is.null(form_icons)) {
    form_icons <- runtime$formIcons
  }

  #print("new map layer added")
  # check for whole image annotations
  runtime$current_annotation_whole_images <- runtime$user_annotations_data |>
    dplyr::filter(imagefile == runtime$current_image & feature_type %in% c("Point-whole-image-annotation")) |>
    sf::st_as_sf(wkt = "geometry")
  # check for map annotations
  runtime$current_annotation_markers <- runtime$user_annotations_data |>
    dplyr::filter(imagefile == runtime$current_image & feature_type %in% c("Point-map")) |>
    sf::st_as_sf(wkt = "geometry")
  # check for polygon annotations
  runtime$current_annotation_polygons <- runtime$user_annotations_data |>
    dplyr::filter(imagefile == runtime$current_image & feature_type %in% c("Polygon-map")) |>
    sf::st_as_sf(wkt = "geometry")

  myMapProxy <- leaflet::leafletProxy("mymap")

  #Check and add markers if present
  if(any(sf::st_geometry_type(runtime$current_annotation_markers) %in% c("POINT", "MULTIPOINT"))) {
    myMapProxy <- myMapProxy |>
      leaflet::addAwesomeMarkers(
        data = runtime$current_annotation_markers, #single_feature,
        layerId = ~id,  # Set layerId to the id column
        group = "Map-Annotations",
        icon = map_icons$pointMapIcon,
        label = ~id,
        popup = ~paste(form_icons$pointMapFormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        )
      )
  }

  # Check if r$annotation_polygons contains polygons before adding them
  if(any(sf::st_geometry_type(runtime$current_annotation_polygons) %in% c("POLYGON", "MULTIPOLYGON"))) {
    myMapProxy <- myMapProxy |>
      leaflet::addPolygons( data = runtime$current_annotation_polygons,
                            layerId = ~id,  # Set layerId to the id column
                            group = "Map-Annotations",
                            label = ~id,
                            stroke = config$mapPolygonStroke,
                            color = config$mapPolygonStrokeColour,
                            weight = config$mapPolygonStrokeWeight,
                            opacity = config$mapPolygonStrokeOpacity,
                            fill = config$mapPolygonFill,
                            fillColor = config$mapPolygonFillColour,
                            fillOpacity = config$mapPolygonFillOpacity,
                            dashArray = NULL,
                            smoothFactor = 1,
                            popup = ~paste(form_icons$polygonMapFormIcon,
                                           "ID:", id, "<br>"
                            ),
                            popupOptions = leaflet::popupOptions(
                              maxWidth = 300,
                              minWidth = 50,
                              maxHeight = NULL,
                              autoPan = FALSE,
                              keepInView = TRUE,
                              closeButton = FALSE,
                              closeOnClick = TRUE
                            )
      )
  }

  # Check and add whole image annotations if present
  if(any(sf::st_geometry_type(runtime$current_annotation_whole_images) %in% c("POINT", "MULTIPOINT"))) {
    myMapProxy <- myMapProxy |>
      # Add markers with the Font Awesome "street view" icon
      leaflet::addAwesomeMarkers(
        data = runtime$current_annotation_whole_images,
        layerId = ~id,  # Set layerId to the id column
        group = "Whole-Image-Annotations",
        icon = map_icons$wholeImageMapIcon,
        label = ~id,
        popup = ~paste(form_icons$wholeImageMapFormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        ),
        clusterOptions = leaflet::markerClusterOptions(
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5),
          freezeAtZoom = FALSE
        ),
        clusterId = "Whole-Image-Annotations"
      )
  }
  return(myMapProxy)
}

remove_map_item <- function(runtime = r){
  #print("remove_map_item called")
  myMapProxy <- leaflet::leafletProxy("mymap") |>
    leaflet::removeMarkerFromCluster(layerId=runtime$remove_leafletMap_item, clusterId = "Whole-Image-Annotations") |>
    leaflet::removeMarker(runtime$remove_leafletMap_item) |>
    leaflet::removeShape(runtime$remove_leafletMap_item)

  return(myMapProxy)
}

remove_360_item <- function(runtime = r){
  #print("remove_360_item called")
  my360Proxy <- leaflet::leafletProxy("leaflet360") |>
    leaflet::removeMarker(runtime$remove_leaflet360_item) |>
    leaflet::removeShape(runtime$remove_leaflet360_item)

  return(my360Proxy)
}

# Functions for 360 image panel ----
# Function to load the base 360 leaflet
loadBaseLeaflet360 <- function() {

  #print("LoadBase360 called")
  leaflet360 <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = -2, maxZoom = 4, crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple")))
    #%>%
    #   leafpm::addPmToolbar(targetGroup = "360-Annotations",
    #                        toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
    #                                                                  drawPolygon = TRUE,
    #                                                                  drawPolyline = FALSE,
    #                                                                  drawCircle = FALSE,
    #                                                                  editMode = TRUE,
    #                                                                  cutPolygon = FALSE,
    #                                                                  removalMode = FALSE),
    #                        drawOptions = leafpm::pmDrawOptions(list(draggable = FALSE)),
    #                        editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
    #                                                            allowSelfIntersection = FALSE, draggable = FALSE,
    #                                                            preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)
    #   ) %>%
    #   leaflet::addLayersControl(overlayGroups = c("360-Annotations"), options = leaflet::layersControlOptions(collapsed = FALSE))
    #

  })
  return(leaflet360)
}

# add current image to 360 leaflet
addCurrentImageToLeaflet360 <- function(runtime = r){
  #print(paste0("addCurrentImageToLeaflet360 called: r$current_image: ", r$current_image))
  # Prepare the dynamic image URL
  imageURL <- paste0("'", runtime_image_url(runtime$current_image, runtime = runtime), "'")
  # Define the bounds of the image
  imageWidth <- runtime$current_image_metadata$ImageWidth  # Width of the image
  imageHeight <- runtime$current_image_metadata$ImageHeight  # Height of the image
  imageBounds <- list(c(0, 0), c(imageHeight, imageWidth))
  # Calculate the center of the image
  imageCenter <- c(imageHeight / 2, imageWidth / 2)


  leaflet360 <- leaflet::renderLeaflet({
    leafletMap <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = -2, maxZoom = 5, crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple"))) |>
      leafpm::addPmToolbar(targetGroup = "360-Annotations",
                           toolbarOptions = leafpm::pmToolbarOptions(drawMarker = TRUE,
                                                                     drawPolygon = TRUE,
                                                                     drawPolyline = FALSE,
                                                                     drawCircle = FALSE,
                                                                     editMode = TRUE,
                                                                     cutPolygon = FALSE,
                                                                     removalMode = FALSE),
                           drawOptions = leafpm::pmDrawOptions(list(draggable = FALSE)),
                           editOptions = leafpm::pmEditOptions(snappable = FALSE, snapDistance = 20,
                                                               allowSelfIntersection = FALSE, draggable = FALSE,
                                                               preventMarkerRemoval = FALSE, preventVertexEdit = FALSE)) |>
      leaflet::addLayersControl(overlayGroups = c("360-Annotations"), options = leaflet::layersControlOptions(collapsed = FALSE))


    leafletMap <- htmlwidgets::onRender(
      leafletMap,
      paste0("
        function(el, x) {
          var imageUrl = ", imageURL, ";
          var imageBounds = ", jsonlite::toJSON(imageBounds), ";
          L.imageOverlay(imageUrl, imageBounds, {
            opacity: 1,
            interactive: false
          }).addTo(this);

          Shiny.addCustomMessageHandler('removeleaflet360', function(data){
           var map = HTMLWidgets.find('#' + data.elid).getMap();
           var layer = map._layers[data.layerId];
            if(layer) {
              map.removeLayer(layer);
            }
          });

         }
      ")
    )

    # Set the initial view of the map outside the onRender function
    leafletMap <- leafletMap |>
      leaflet::setView(lng = imageCenter[2], lat = imageCenter[1], zoom = -2)
   })

  return(leaflet360)
}

# add annotation to leaflet 360
add_annotations_to_360 <- function(runtime = r, config = NULL, map_icons = NULL, form_icons = NULL){
  #print("add_annotations_to_360 called")
  #print("new 360 layer added")
  if (is.null(config)) {
    config <- runtime$config
  }
  if (is.null(map_icons)) {
    map_icons <- runtime$mapIcons
  }
  if (is.null(form_icons)) {
    form_icons <- runtime$formIcons
  }

  req(runtime$user_annotations_data)
  # check for map annotations
  runtime$current_annotation_360markers <- runtime$user_annotations_data |>
    dplyr::filter(imagefile == runtime$current_image & feature_type %in% c("Point-360")) |>
    sf::st_as_sf(wkt = "geometry")
  # check for polygon annotations
  runtime$current_annotation_360polygons <- runtime$user_annotations_data |>
    dplyr::filter(imagefile == runtime$current_image & feature_type %in% c("Polygon-360")) |>
    sf::st_as_sf(wkt = "geometry")

  #View(r$current_annotation_360polygons)

  my360Proxy <- leaflet::leafletProxy("leaflet360")

  #Check and add markers if present
  if(any(sf::st_geometry_type(runtime$current_annotation_360markers) %in% c("POINT", "MULTIPOINT"))) {
    my360Proxy <- my360Proxy |>
      leaflet::addAwesomeMarkers(
        data = runtime$current_annotation_360markers, #single_feature,
        layerId = ~id,  # Set layerId to the id column
        group = "360-Annotations",
        icon = map_icons$point360Icon,
        label = ~id,
        popup = ~paste(form_icons$point360FormIcon,
                       "ID:", id, "<br>"
        ),
        popupOptions = leaflet::popupOptions(
          maxWidth = 300,
          minWidth = 50,
          maxHeight = NULL,
          autoPan = FALSE,
          keepInView = TRUE,
          closeButton = FALSE,
          closeOnClick = TRUE
        )
      )
  }

  # Check if r$annotation_polygons contains polygons before adding them
  if(any(sf::st_geometry_type(runtime$current_annotation_360polygons) %in% c("POLYGON", "MULTIPOLYGON"))) {
    my360Proxy <- my360Proxy |>
      leaflet::addPolygons( data = runtime$current_annotation_360polygons,
                            layerId = ~id,  # Set layerId to the id column
                            group = "360-Annotations",
                            label = ~id,
                            stroke = config$pano360PolygonStroke,
                            color = config$pano360PolygonStrokeColour,
                            weight = config$pano360PolygonStrokeWeight,
                            opacity = config$pano360PolygonStrokeOpacity,
                            fill = config$pano360PolygonFill,
                            fillColor = config$pano360PolygonFillColour,
                            fillOpacity = config$pano360PolygonFillOpacity,
                            dashArray = NULL,
                            smoothFactor = 1,
                            popup = ~paste(form_icons$polygon360FormIcon,
                                           "ID:", id, "<br>"
                            ),
                            popupOptions = leaflet::popupOptions(
                              maxWidth = 300,
                              minWidth = 50,
                              maxHeight = NULL,
                              autoPan = FALSE,
                              keepInView = TRUE,
                              closeButton = FALSE,
                              closeOnClick = TRUE
                            )
      )
  }
  return(my360Proxy)
}

# clears drawn item from leaflet so it can be reloaded with date ID
clear_drawn_annotation_from_360 <- function(session, layerId) {
  #print("clear_drawn_annotation_from_360 called")
  session$sendCustomMessage("removeleaflet360", list(elid = "pano360_image-leaflet360", layerId = layerId))
}

# function for outputting cropped polygons
create_cropped_polygons_from_360_images <- function(annotations_export_dir, runtime = r, config = NULL){
  req(runtime$user_annotations_data, runtime$current_annotation_360polygons, runtime$current_image)
  if (is.null(config)) {
    config <- runtime$config
  }

  df_polygons <- runtime$current_annotation_360polygons

  image_path <- file.path(runtime_kmz_files_dir(runtime), runtime$current_image)
  img <- jpeg::readJPEG(image_path)

  img_raster <- grDevices::as.raster(img)
  plot_width <- runtime$current_image_metadata$ImageWidth
  plot_height <- runtime$current_image_metadata$ImageHeight

  if (!is.null(df_polygons)) {
    polygons_sf <- sf::st_as_sf(df_polygons, wkt = "geometry", crs = 4326)  # Ensure to set a valid CRS
    num_polygons <- nrow(polygons_sf)

    withProgress(message = 'Creating crops', value = 0, {


      for (i in seq_len(nrow(polygons_sf))) {
        bbox <- sf::st_bbox(polygons_sf[i, ])

        # Update progress bar
        incProgress(1 / num_polygons, detail = paste("Processing image", i, "of", num_polygons))

        # Initialize the plot with the raster annotation
        p <- ggplot2::ggplot() +
          ggplot2::annotation_raster(img_raster, xmin=0, xmax=plot_width, ymin=0, ymax=plot_height) +
          ggplot2::coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) +
          ggplot2::theme_void()

        # Add the polygon layer conditionally
        if (config$showPano360PolygonStrokeInCropExport && config$showPano360PolygonFillInCropExport) {
          # Both stroke and fill enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = scales::alpha(config$pano360PolygonStrokeColour, config$pano360PolygonStrokeOpacity),
            fill = config$pano360PolygonFillColour,
            linewidth = config$pano360PolygonStrokeWeight,
            alpha = config$pano360PolygonFillOpacity
          )
        } else if (config$showPano360PolygonStrokeInCropExport) {
          # Only stroke enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = scales::alpha(config$pano360PolygonStrokeColour, config$pano360PolygonStrokeOpacity),
            fill = NA,
            linewidth = config$pano360PolygonStrokeWeight#,
            #alpha = myEnv$config$pano360PolygonStrokeOpacity
          )
        } else if (config$showPano360PolygonFillInCropExport) {
          # Only fill enabled
          p <- p + ggplot2::geom_sf(
            data = polygons_sf[i, ],
            color = NA,
            fill = config$pano360PolygonFillColour,
            alpha = config$pano360PolygonFillOpacity
          )
        }

        # Export each plot as a PNG image
        # add id to the filename
        cropped_image_path <- paste0(annotations_export_dir, "/", gsub("\\.\\w+$", paste0("_", polygons_sf[i, "id"], ".png"), runtime$current_image))
        #print(cropped_image_path)

        #ggplot2::ggsave(cropped_image_path, plot = p, width = plot_width, height = plot_height, units = "px", dpi = 96, limitsize = FALSE, bg = "transparent")
        grDevices::png(filename = cropped_image_path, units = "px", type = "cairo-png", bg = "transparent", res = 96)
        print(p)  # This will render the ggplot object to the PNG device
        grDevices::dev.off()

        # get the gps metadata from the r$current_image
        lat <- runtime$current_image_metadata$GPSLatitude
        long <- runtime$current_image_metadata$GPSLongitude
        lat_ref <- runtime$current_image_metadata$GPSLatitudeRef
        long_ref <- runtime$current_image_metadata$GPSLongitudeRef
        # now write the exiftool GPS metadata to the png
        #View(r$current_image_metadata)
        write_image_gps_metadata(image_file=cropped_image_path, latitude=lat, latitude_ref=lat_ref, longitude=long,longitude_ref=long_ref)

      }

    }) #withProgress
  }
  #return("success")
}

# Icon functions ----
create_map_icons <- function(config = myEnv$config) {
  #myEnv$config$mapIconColour <- "DarkRed"
  #myEnv$config$pano360IconColour <- "navy"
  #myEnv$config$pano360MarkerColour <- "white"
  #myEnv$config$mapMarkerColour <- "white"

  myIcons <- leaflet::awesomeIconList(
    wholeImageMapIcon = leaflet::makeAwesomeIcon(icon = "ion-image", library = "ion", iconColor =  config$mapIconColour, markerColor = config$mapMarkerColour),#ios-world-outline
    pointMapIcon = leaflet::makeAwesomeIcon(icon = "map-marked-alt", library = "fa", iconColor =  config$mapIconColour, markerColor = config$mapMarkerColour),
    polygonMapIcon = leaflet::makeAwesomeIcon(icon = "draw-polygon", library = "fa",iconColor =  config$mapIconColour, markerColor = config$mapMarkerColour),
    point360Icon = leaflet::makeAwesomeIcon(icon = "map-marked-alt", library = "fa",iconColor =  config$pano360IconColour, markerColor = config$pano360MarkerColour),
    polygon360Icon = leaflet::makeAwesomeIcon(icon = "draw-polygon", library = "fa",iconColor =  config$pano360IconColour, markerColor = config$pano360MarkerColour)
  )
  return(myIcons)
}

create_form_icons <- function(config = myEnv$config) {
  #myEnv$config$mapIconColour <- "DarkRed"
  #myEnv$config$pano360IconColour <- "navy"
  formIcons <- list(
    wholeImageMapFormIcon = paste0("<i class='ionicons ion-image' style='color: ", config$mapIconColour, "; background-color: transparent;'></i>"),
    pointMapFormIcon = paste0("<i class='fa fa-map-marked-alt' style='color: ", config$mapIconColour, "; background-color: transparent;'></i>"),
    polygonMapFormIcon = paste0("<i class='fa fa-draw-polygon' style='color: ", config$mapIconColour, "; background-color: transparent;'></i>"),
    point360FormIcon = paste0("<i class='fa fa-map-marked-alt' style='color: ", config$pano360IconColour, "; background-color: transparent;'></i>"),
    polygon360FormIcon = paste0("<i class='fa fa-draw-polygon' style='color: ", config$pano360IconColour, "; background-color: transparent;'></i>")
  )
  return(formIcons)
}

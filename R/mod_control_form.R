#' control_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

lookup_help_button_specs <- function(config = myEnv$config) {
  Filter(
    f = function(spec) isTRUE(spec$enabled),
    x = list(
    list(index = 1L, input_id = "lookup1_help", label = config$lookup1Label, enabled = TRUE),
    list(index = 2L, input_id = "lookup2_help", label = config$lookup2Label, enabled = isTRUE(config$lookup2Enabled)),
    list(index = 3L, input_id = "lookup3_help", label = config$lookup3Label, enabled = isTRUE(config$lookup3Enabled)),
    list(index = 4L, input_id = "lookup4_help", label = config$lookup4Label, enabled = isTRUE(config$lookup4Enabled))
    )
  )
}


build_lookup_help_buttons_ui <- function(ns, config = myEnv$config) {
  specs <- lookup_help_button_specs(config = config)

  if (length(specs) == 0) {
    return(NULL)
  }

  tagList(
    tags$h2("Help Files:"),
    lapply(specs, function(spec) {
      help_file_name <- config[[paste0("lookup", spec$index, "HelpFile")]]
      if (is.null(help_file_name) || !nzchar(help_file_name)) {
        help_file_name <- paste0("help", spec$index, ".pdf")
      }

      actionButton(
        inputId = ns(spec$input_id),
        label = paste0(spec$label, " Help"),
        icon = icon("question-circle"),
        onclick = paste0(
          "window.open('./app_data/",
          utils::URLencode(help_file_name, reserved = TRUE),
          "', '_blank')"
        )
      )
    })
  )
}


mod_control_form_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      hr {border-top: 1px solid #000000;}
      .text-content {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .text-content small {
        white-space: normal;
      }
    "))
    ),
    # username select ----
    uiOutput(ns("user_name_input")),

    htmlOutput(ns("infoText")),
    uiOutput(ns("annotation_notice")),

    uiOutput(ns("lookup_help_buttons")),

    tags$hr(),

    tags$div(style="align-content:end",
             #actionButton(inputId = ns("save_annotations"), label = "Save All Records", icon = icon("save"), style = "margin-bottom: 5px;"),
             shinyFiles::shinyDirButton(id=ns("export_annotations"), label='Export All Records', title='Please select a folder to export the annotations into :)', icon=icon("download"), multiple=FALSE, viewtype="list", style = "margin-bottom: 5px;"),

             actionButton(inputId = ns("add_whole_image_annotation"), label = "Add A Whole Image Annotation", icon = icon("plus"), style = "margin-bottom: 5px;"),
             #actionButton(inputId = ns("remove_all_annotations_for_image"), label = "Delete All Annotations For Image", icon = icon("trash")),
    ),
    tags$hr(),
    uiOutput(ns("annotation_cards"))
  )
}

#' control_form Server Functions
#'
#' @noRd
mod_control_form_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Clean up when the app close ----
    onStop(function(){
      #print("Doing application cleanup\n")
      r$imgs_lst <- NULL
      r$current_image <- NULL
      })

    #disable the save button at first
    ####shinyjs::disable("save_annotations")
    shinyjs::disable("export_annotations")
    shinyjs::disable("add_whole_image_annotation")

    initial_config <- runtime_config_value(r)
    if (isTRUE(initial_config$showWorkflowGuidanceNotices)) {
      r$annotation_panel_notice <- list(
        title = "Configure The App",
        message = "Use the settings panel above to set your custom user files before you start annotating.",
        type = "info"
      )
    }

    output$user_name_input <- renderUI({
      choices <- r$var_choices
      if (is.null(choices)) {
        choices <- list()
      }

      shinyWidgets::pickerInput(
        inputId = ns("user_name"),
        label = "User Name",
        choices = choices,
        selected = if (!is.null(r$user_name) && nzchar(r$user_name)) r$user_name else NULL,
        multiple = FALSE,
        width = "100%",
        options = list(container = "body", title = "FIRST: Select Your Name")
      )
    })

    output$lookup_help_buttons <- renderUI({
      config <- r$config
      if (is.null(config)) {
        config <- myEnv$config
      }

      build_lookup_help_buttons_ui(ns = session$ns, config = config)
    })

    output$annotation_notice <- renderUI({
      notice <- r$annotation_panel_notice
      if (is.null(notice)) {
        return(NULL)
      }

      panel_notice(
        title = notice$title,
        message = notice$message,
        type = notice$type
      )
    })

    output$annotation_cards <- renderUI({
      active_annotations <- r$active_annotations()

      if (is.null(r$user_name) || !nzchar(r$user_name)) {
        return(panel_empty_state(
          title = "Annotation Cards",
          message = "Select a user name to begin an annotation session."
        ))
      }

      if (is.null(r$current_image) || !nzchar(r$current_image)) {
        return(panel_empty_state(
          title = "Annotation Cards",
          message = "Choose an image in the Image Panel or click an image marker on the map to view its annotation records."
        ))
      }

      if (is.null(active_annotations) || length(active_annotations) == 0) {
        return(panel_empty_state(
          title = "No Annotations Yet",
          message = "Use the map, image viewer, or whole-image button above to create annotation records for the current image."
        ))
      }

      render_annotation_cards_ui(
        session = session,
        annotation_cards = r$annotation_cards,
        active_annotations = r$active_annotations,
        config = r$config,
        lookup_choices = list(
          dd1 = r$var_dropdown1,
          dd2 = r$var_dropdown2,
          dd3 = r$var_dropdown3,
          dd4 = r$var_dropdown4
        ),
        form_icons = r$formIcons
      )
    })

    #event triggered on selecting username ----
    observe({
      r$user_name <- squish_whitespace(input$user_name)
      req(r$user_name, r$config$annotationsFile)
      r$user_annotations_file_name <- normalizePath(file.path(runtime_data_dir(r), r$config$annotationsFile), mustWork = FALSE)
      #print(r$user_annotations_file_name)
      r$user_annotations_data <- check_for_saved_data(r$user_annotations_file_name)
      if(isTRUE(r$config$showWorkflowGuidanceNotices)){
        r$annotation_panel_notice <- list(
          title = "Session Ready",
          message = "Next, load a .kmz file in the Mapping Panel. That will populate the map and image list for annotation.",
          type = "info"
        )
      }
    }) |> bindEvent(input$user_name)

    # output for text info ----
    output$infoText <- renderUI({
      req(r$user_name, r$current_image )
      if(nchar(r$user_name)>0){
        if(nchar(r$current_image)>0){
          shinyjs::enable("export_annotations")
          shinyjs::enable("add_whole_image_annotation")
          str1 <- paste0("<b>Annotation File:</b> ", r$user_name, "s_annotations.rds")
          str2 <- paste0("<b>Image File:</b> <small>", r$current_image, "</small><hr>")
          HTML(paste(str1, str2, sep ='<br/>'))
        }
        else {
          shinyjs::disable("export_annotations")
          shinyjs::disable("add_whole_image_annotation")
        }
      }
    })

    #add new whole image annotation record button clicked ----
    observe({
      #print("add a whole image annotation clicked")
      req(r$current_image, r$current_image_metadata, r$user_name)

      myId <- gsub("\\.", "",format(Sys.time(), "%Y%m%d-%H%M%OS3"))
      geomType <- "Point-whole-image-annotation"
      lat <- r$current_image_metadata$GPSLatitude
      long <- r$current_image_metadata$GPSLongitude
      geom <- paste0("POINT(", long, " ", lat, ")")
      #
      feature <- list(
        type = "Feature",
        geometry = list(
          type = "Point",
          coordinates = c(long, lat)
        ),
        properties = list(
          id = myId,
          layerId = myId,
          edit_id = myId,
          feature_type = geomType
        )
      )

      r$new_leafletMap_item <- feature

    }) |> bindEvent(input$add_whole_image_annotation)

    # when new map item added ----
    # listening for both button clicked in form OR item drawn in map panel
    observe({
      #print("new map item added: mod_control_form")

      #str <- sprintf("new feature with layerId: %s", r$new_leafletMap_item)
      #print(str)

      # Convert the feature with the new ID to a sf object
      myMarker <- geojsonsf::geojson_sf(jsonify::to_json(r$new_leafletMap_item, unbox = TRUE, digits=9))
      geom <- sf::st_as_text(myMarker$geometry, digits=9)
      geomType <- r$new_leafletMap_item$properties$feature_type

      # add annotations form and update the active annotations list
      add_annotations_form(
        input = input,
        myActiveAnnotations = r$active_annotations,
        myId = r$new_leafletMap_item$properties$id,
        #myLeafletId = r$new_leafletMap_item$properties$id,
        myFeatureType = geomType,
        myGeometry = geom,
        myRadius = NA,
        myDD1 = NA,
        myDD2 = NA,
        myDD3 = NA,
        myDD4 = NA,
        runtime = r
      )

    }) |> bindEvent(r$new_leafletMap_item)

    # when new 360 item added, listening for drawing in 360 panel
    observe({
      #print("new 360 item added: mod_control_form")

      #str <- sprintf("new feature with layerId: %s", r$new_leaflet360_item)
      #print(str)

      # Convert the feature with the new ID to a sf object
      myMarker <- geojsonsf::geojson_sf(jsonify::to_json(r$new_leaflet360_item, unbox = TRUE, digits=9))
      geom <- sf::st_as_text(myMarker$geometry, digits=9)
      geomType <- r$new_leaflet360_item$properties$feature_type

      # add annotations form and update the active annotations list
      add_annotations_form(
        input = input,
        myActiveAnnotations = r$active_annotations,
        myId = r$new_leaflet360_item$properties$id,
        #myLeafletId = r$new_leaflet360_item$properties$id,
        myFeatureType = geomType,
        myGeometry = geom,
        myRadius = NA,
        myDD1 = NA,
        myDD2 = NA,
        myDD3 = NA,
        myDD4 = NA,
        runtime = r
      )

    }) |> bindEvent(r$new_leaflet360_item)


    #check if there are any annotations for a selected image already ----
    observe({
      #print("current image changed: mod_control_form")
      req(r$user_annotations_data, r$current_image)

      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
      clear_annotations_form(runtime = r)

      previous_annotations <- check_for_annotations(
        r$user_annotations_data,
        r$current_image,
        mySourceKmz = r$current_kmz_name
      )

        if(nrow(previous_annotations) >= 1){
        #print("annotations already exist")
        for(i in 1:nrow(previous_annotations)){
          #View(previous_annotations)
          add_annotations_form(input=input, myActiveAnnotations=r$active_annotations, myId=previous_annotations[i, "id"], myFeatureType=previous_annotations[i, "feature_type"], myRadius=previous_annotations[i, "radius"], myGeometry= previous_annotations[i, "geometry"], myDD1= previous_annotations[i, "dd1"],myDD2= previous_annotations[i, "dd2"], myDD3=previous_annotations[i, "dd3"], myDD4=previous_annotations[i, "dd4"], runtime = r)
        }

        if(isTRUE(r$config$showWorkflowGuidanceNotices)){
          r$annotation_panel_notice <- list(
            title = "Annotations Loaded",
            message = "Saved annotations already existed for this image, so they have been loaded into the panel.",
            type = "success"
          )
        }
      } else {
        r$annotation_panel_notice <- NULL

      }
    }) |> bindEvent(r$current_image)

    #export annotations button ----
    observe({
      req(r$user_annotations_file_name,  r$user_annotations_data)
      #home_dir <- fs::path_home()
      #documents_dir <- file.path(home_dir)
      #volumes <- c(Documents = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

      # Create volumes list containing only the Documents folder
      volumes <- c(shinyFiles::getVolumes()())

      if (is.integer(input$export_annotations)) {
        cat("No directory has been selected (shinyDirChoose)")
        shinyFiles::shinyDirChoose(input,"export_annotations", roots = volumes, session = session)
      } else {
        annotations_export_dir <- shinyFiles::parseDirPath(volumes, input$export_annotations)
        annotations_export_full_path_rds <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.rds")
        annotations_export_full_path_csv <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.csv")

        save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

        export_success <- FALSE

        if(isTRUE(r$config$exportFileFormat == "rds")){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user","id", "sourcekmz", "imagefile", "feature_type", "radius", "geometry", paste0(r$config$lookup1Label), paste0(r$config$lookup2Label), paste0(r$config$lookup3Label), paste0(r$config$lookup4Label))
          saveRDS(temp_df, file=annotations_export_full_path_rds)
          rm(temp_df)
          r$annotation_panel_notice <- list(
            title = "Export Successful",
            message = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_rds)),
            type = "success"
          )
          export_success <- TRUE
        }
        else if (isTRUE(r$config$exportFileFormat == "csv")){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user", "id", "sourcekmz", "imagefile", "feature_type", "radius", "geometry", paste0(r$config$lookup1Label), paste0(r$config$lookup2Label), paste0(r$config$lookup3Label), paste0(r$config$lookup4Label))
          utils::write.csv(temp_df, annotations_export_full_path_csv, fileEncoding = "UTF-8", row.names=FALSE)
          rm(temp_df)
          r$annotation_panel_notice <- list(
            title = "Export Successful",
            message = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_csv)),
            type = "success"
          )
          export_success <- TRUE
        }

        if (isTRUE(export_success)) {
          close_shinyfiles_dialog()
        }

      }

    }) |> bindEvent(input$export_annotations)

    # refresh form on apply settings button ----
    observe({
      #print("refresh_for_item: control_form")
      req(r$refresh_user_config, r$user_annotations_data, r$current_image)

      #req(r$user_annotations_data, r$current_image)

      #call the functions to create the icons using the colours etc from the settings panel
      refresh_app_state_assets(r)

      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
      clear_annotations_form(runtime = r)

      previous_annotations <- check_for_annotations(
        r$user_annotations_data,
        r$current_image,
        mySourceKmz = r$current_kmz_name
      )

      if(nrow(previous_annotations) >= 1){
        #print("annotations already exist")
        for(i in 1:nrow(previous_annotations)){
          #View(previous_annotations)
          add_annotations_form(input=input, myActiveAnnotations=r$active_annotations, myId=previous_annotations[i, "id"], myFeatureType=previous_annotations[i, "feature_type"], myRadius=previous_annotations[i, "radius"], myGeometry= previous_annotations[i, "geometry"], myDD1= previous_annotations[i, "dd1"],myDD2= previous_annotations[i, "dd2"], myDD3=previous_annotations[i, "dd3"], myDD4=previous_annotations[i, "dd4"], runtime = r)
        }

      }

    }) |> bindEvent(r$refresh_user_config)


  })
}

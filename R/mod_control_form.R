#' control_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_control_form_ui <- function(id){
  ns <- NS(id)

  myEnv$config <- configr::read.config(app_sys("extdata/user-config.yml"))

  #the environment setup in fct_helpers.R file
  myEnv$var_choices <- load_lookup(
    #fileToLoad = app_sys(myEnv$config$usernameLookupFile),
    fileToLoad = app_sys(paste0("extdata/", myEnv$config$usernameLookupFile)),
    display_column = "user_name",
    value_column = "value")

  myEnv$var_dropdown1 <- load_lookup(
    fileToLoad = app_sys(paste0("extdata/", myEnv$config$lookup1CsvFile)),
    display_column = "display",
    value_column = "value")

  myEnv$var_dropdown2 <- load_lookup(
    fileToLoad = app_sys(paste0("extdata/", myEnv$config$lookup2CsvFile)),
    display_column = "display",
    value_column = "value")

  myEnv$var_dropdown3 <- load_lookup(
    fileToLoad = app_sys(paste0("extdata/", myEnv$config$lookup3CsvFile)),
    display_column = "display",
    value_column = "value"
  )

  myEnv$var_dropdown4 <- load_lookup(
    fileToLoad = app_sys(paste0("extdata/", myEnv$config$lookup4CsvFile)),
    display_column = "display",
    value_column = "value"
  )

  #call the functions to create the icons using the colours etc from the settings panel
  myEnv$mapIcons <- create_map_icons()
  myEnv$formIcons <- create_form_icons()

  tagList(
    tags$head( tags$style(HTML("
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

    tags$div(style="margin:5px",

    ###################################
    # config form in drop down button #
    ###################################
    shinyWidgets::dropdownButton(

      navbarPage(
        title = "pannotator",
        id = "tabset-default-id",
        selected = "Main Settings",
        collapsible = TRUE,
        theme = shinythemes::shinytheme(myEnv$config$appTheme),
        tabPanel(
          title = "Main Settings",
          gridlayout::grid_container(
            layout = c(
              "mainSettings ."
            ),
            gap_size = "10px",
            col_sizes = c(
              "560px"
            ),
            row_sizes = c(
              "790px"
            ),
            gridlayout::grid_card(
              area = "mainSettings",
              bslib::card_body(
                fillable = TRUE,
                fill = TRUE,
                max_height = "790px",
                padding = 10,
              fluidRow(
                  column(12,
                h2(strong("Layout Settings"))#|>shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m")
                )),
              fluidRow(
                column(10,  checkboxInput(
                  inputId = ns("showPopupAlerts"),
                  label = "Show info popup alert windows",
                  width = "95%",
                  value = myEnv$config$showPopupAlerts
                )
                ),
                column(2, actionButton(
                  inputId = ns("applySettingsButton"),
                  label = "Apply Changes",
                  style = "float: right; margin-bottom: 20px; margin-right: 5px; overflow-x: hidden !important;"
                ))
              ),
                 selectInput(
                   inputId = ns("appTheme"),
                   label = "App Theme",
                   width = "95%",
                   selected = myEnv$config$appTheme,
                   choices = shinythemes:::allThemes(),
                   selectize = FALSE
                 ),
              tags$script(
                "$('#control_form-appTheme')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
             ),
              fluidRow(
                column(4,
               sliderInput(
                  inputId = ns("mapPanelWidth"),
                  label = "Mapping Panel Width",
                  min = 3,
                  max = 6,
                  value = myEnv$config$mapPanelWidth,
                  step = 1,
                )
               ),
               column(4,
               sliderInput(
                  inputId = ns("panoPanelWidth"),
                  label = "Image Panel Width",
                  min = 3,
                  max = 6,
                  value = myEnv$config$panoPanelWidth,
                  step = 1,
                 )
                ),
               column(4,
                sliderInput(
                  inputId = ns("formPanelWidth"),
                  label = "Annotation Panel Width",
                  min = 2,
                  max = 4,
                  value = myEnv$config$formPanelWidth,
                  step = 1,
                )
                )),
                h2(strong("Mapping Panel Settings")), #|> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                selectInput(
                  inputId = ns("mapPanelSource"),
                  label = "Leaflet Map Source",
                  width = "95%",
                  selected = myEnv$config$mapPanelSource,
                  choices = list("Esri WorldImagery" = "Esri.WorldImagery", "Esri WorldTopoMap" = "Esri.WorldTopoMap",        "Esri WorldStreetMap" = "Esri.WorldStreetMap", "Open StreetMap" = "OpenStreetMap", "Open TopoMap" = "OpenTopoMap")
                ),
              fluidRow(
                column(6,  colourpicker::colourInput(
                  inputId = ns("mapIconColour"),
                  label = "Map Icon Colour",
                  palette = "limited",
                  showColour = "background",
                  returnName = TRUE,
                  closeOnClick = TRUE,
                  allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
                  value = myEnv$config$mapIconColour
                )
                ),
                column(6, colourpicker::colourInput(
                  inputId = ns("mapMarkerColour"),
                  label = "Map Marker Background Colour",
                  palette = "limited",
                  showColour = "background",
                  returnName = TRUE,
                  closeOnClick = TRUE,
                  allowedCols = c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "lightgray", "black"),
                  value =myEnv$config$mapMarkerColour,
                ))
              ),
              checkboxInput(
                inputId = ns("mapPolygonStroke"),
                label = "Map Polygon Stroke",
                width = "95%",
                value = myEnv$config$mapPolygonStroke
              ) ,
              fluidRow(
                column(12,
                       conditionalPanel(
                         condition = paste0("input['" ,ns("mapPolygonStroke"), "']"),
                         div(
                           style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
                           fluidRow(
                             column(2,  colourpicker::colourInput(
                               inputId = ns("mapPolygonStrokeColour"),
                               label = "Stroke Colour",
                               palette = "limited",
                               showColour = "background",
                               returnName = TRUE,
                               closeOnClick = TRUE,
                               allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
                               value = myEnv$config$mapPolygonStrokeColour
                             )),
                             column(5,   sliderInput(
                               inputId = ns("mapPolygonStrokeWeight"),
                               label = "Stroke Weight",
                               min = 1,
                               max = 6,
                               value = myEnv$config$mapPolygonStrokeWeight,
                               step = 1,
                             )),
                             column(5,   sliderInput(
                               inputId = ns("mapPolygonStrokeOpacity"),
                               label = "Stroke Opacity",
                               min = 0.1,
                               max = 1,
                               value = myEnv$config$mapPolygonStrokeOpacity,
                               step = 0.1,
                             ))
                           )
                         )
                       )
                )
              ),
              checkboxInput(
                inputId = ns("mapPolygonFill"),
                label = "Map Polygon Fill",
                width = "95%",
                value = myEnv$config$mapPolygonFill
              ) ,
              fluidRow(
                column(12,
                       conditionalPanel(
                         condition = paste0("input['" ,ns("mapPolygonFill"), "']"),
                         div(
                           style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
                           fluidRow(
                             column(4,  colourpicker::colourInput(
                               inputId = ns("mapPolygonFillColour"),
                               label = "Fill Colour",
                               palette = "limited",
                               showColour = "background",
                               returnName = TRUE,
                               closeOnClick = TRUE,
                               allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
                               value = myEnv$config$mapPolygonFillColour
                             )),
                             column(8,   sliderInput(
                               inputId = ns("mapPolygonFillOpacity"),
                               label = "Fill Opacity",
                               min = 0.1,
                               max = 1,
                               value = myEnv$config$mapPolygonFillOpacity,
                               step = 0.1,
                             ))
                           )
                         )
                       )
                )
              ),
           h2(strong("Image Panel Settings")),# |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
           fluidRow(
             column(6,  colourpicker::colourInput(
               inputId = ns("pano360IconColour"),
               label = "Image Icon Colour",
               palette = "limited",
               showColour = "background",
               returnName = TRUE,
               closeOnClick = TRUE,
               allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
               value = myEnv$config$pano360IconColour,
             )
             ),
             column(6, colourpicker::colourInput(
               inputId = ns("pano360MarkerColour"),,
               label = "Image Marker Background Colour",
               palette = "limited",
               showColour = "background",
               returnName = TRUE,
               closeOnClick = TRUE,
               allowedCols = c("red", "darkred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "pink", "cadetblue", "white", "gray", "lightgray", "black"),
               value = myEnv$config$pano360MarkerColour
             )),
           ),
           checkboxInput(
             inputId = ns("pano360PolygonStroke"),
             label = "Image Polygon Stroke",
             width = "95%",
             value = myEnv$config$pano360PolygonStroke
           ) ,

           fluidRow(
             column(12,
                    conditionalPanel(
                      condition = paste0("input['" ,ns("pano360PolygonStroke"), "']"),
                      div(
                        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
                        fluidRow(
                          column(2,  colourpicker::colourInput(
                            inputId = ns("pano360PolygonStrokeColour"),
                            label = "Stroke Colour",
                            palette = "limited",
                            showColour = "background",
                            returnName = TRUE,
                            closeOnClick = TRUE,
                            allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
                            value = myEnv$config$pano360PolygonStrokeColour
                          )),
                          column(5,   sliderInput(
                            inputId = ns("pano360PolygonStrokeWeight"),
                            label = "Stroke Weight",
                            min = 1,
                            max = 6,
                            value = myEnv$config$pano360PolygonStrokeWeight,
                            step = 1
                          )),
                          column(5,   sliderInput(
                            inputId = ns("pano360PolygonStrokeOpacity"),
                            label = "Stroke Opacity",
                            min = 0.1,
                            max = 1,
                            value = myEnv$config$pano360PolygonStrokeOpacity,
                            step = 0.1
                          ))
                        ),
                        checkboxInput(
                          inputId = ns("showPano360PolygonStrokeInCropExport"),
                          label = "Show Polygon Stroke In Cropped Image Export",
                          width = "95%",
                          value = myEnv$config$showPano360PolygonStrokeInCropExport)
                      )
                    )
             )
           ),
           checkboxInput(
             inputId = ns("pano360PolygonFill"),
             label = "Image Polygon Fill",
             width = "95%",
             value = myEnv$config$pano360PolygonFill
           ) ,
           fluidRow(
             column(12,
                    conditionalPanel(
                      condition = paste0("input['" ,ns("pano360PolygonFill"), "']"),
                      div(
                        style = "border: 1px solid #ccc; padding: 10px; box-shadow: 0px 2px 2px #eee; border-radius: 5px;",
                        fluidRow(
                          column(4,  colourpicker::colourInput(
                            inputId = ns("pano360PolygonFillColour"),
                            label = "Fill Colour",
                            palette = "limited",
                            showColour = "background",
                            returnName = TRUE,
                            closeOnClick = TRUE,
                            allowedCols = c("black", "gray", "white", "navy", "blue", "purple", "green", "maroon", "red", "yellow"),
                            value = myEnv$config$pano360PolygonFillColour
                          )),
                          column(8,   sliderInput(
                            inputId = ns("pano360PolygonFillOpacity"),
                            label = "Fill Opacity",
                            min = 0.1,
                            max = 1,
                            value = myEnv$config$pano360PolygonFillOpacity,
                            step = 0.1,
                          ))
                        ),
                        checkboxInput(
                          inputId = ns("showPano360PolygonFillInCropExport"),
                          label = "Show Polygon Fill In Cropped Image Export",
                          width = "95%",
                          value = myEnv$config$showPano360PolygonFillInCropExport)
                      )
                    )
             )
           ),
      h2(strong("Annotation Panel Settings")),# |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                fileInput(
                  inputId= ns("usernameLookupFile"),
                  label= "Username File",
                  multiple = FALSE,
                  accept = ".csv",
                  width = "95%",
                  buttonLabel = "Browse...",
                  placeholder = paste0(myEnv$config$usernameLookupFile),
                  capture = NULL
                ) |> shinyhelper::helper(type = "markdown", content = "user_name_csv_help", icon = "question-circle", size = "m"),
                selectInput(
                  inputId = ns("exportFileFormat"),
                  label = "Export File Format",
                  width = "95%",
                  selected = myEnv$config$exportFileFormat,
                  choices = list("csv" = "csv", "rds" = "rds")
                ) #|> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m")
              )
            )
          )
        ),
        tabPanel(
          title = "Lookups",
          gridlayout::grid_container(
            layout = c(
              "lookupSettings ."
            ),
            gap_size = "10px",
            col_sizes = c(
              "560px"
            ),
            row_sizes = c(
              "790px"
            ),
            gridlayout::grid_card(
              area = "lookupSettings",
              bslib::card_body(
                fillable = TRUE,
                fill = TRUE,
                padding = 10,
                max_height = "790px",
                textInput(
                  inputId = ns("lookup1Label"),
                  label = "Lookup 1 Label",
                  value = paste0(myEnv$config$lookup1Label),
                  width = "95%"
                ) |> shinyhelper::helper(type = "markdown", content = "lookup_label_help", icon = "question-circle", size = "m"),
                fileInput(
                  inputId= ns("lookup1CsvFile"),
                  label= "Lookup 1 csv File",
                  multiple = FALSE,
                  accept = ".csv",
                  width = "95%",
                  buttonLabel = "Lookup 1 csv File...",
                  placeholder = paste0(myEnv$config$lookup1CsvFile),
                  capture = NULL
                ) |> shinyhelper::helper(type = "markdown", content = "lookup_csv_help", icon = "question-circle", size = "m"),
                fileInput(
                  inputId= ns("lookup1HelpFile"),
                  label= "Lookup 1 Help File",
                  multiple = FALSE,
                  accept = ".pdf",
                  width = "95%",
                  buttonLabel = "Lookup 1 Help File...",
                  placeholder = paste0(myEnv$config$lookup1HelpFile),
                  capture = NULL
                ) |> shinyhelper::helper(type = "markdown", content = "lookup_pdf_help", icon = "question-circle", size = "m"),
                checkboxInput(
                  inputId = ns("lookup2Enabled"),
                  label = "Enable Lookup 2",
                  width = "95%",
                  value = myEnv$config$lookup2Enabled
                ),
                textInput(
                  inputId = ns("lookup2Label"),
                  label = "Lookup 2 Label",
                  value = paste0(myEnv$config$lookup2Label),
                  width = "95%"
                ),
                fileInput(
                  inputId= ns("lookup2CsvFile"),
                  label= "Lookup 2 csv File",
                  multiple = FALSE,
                  accept = ".csv",
                  width = "95%",
                  buttonLabel = "Lookup 2 csv File...",
                  placeholder = paste0(myEnv$config$lookup2CsvFile),
                  capture = NULL
                ),
                fileInput(
                  inputId= ns("lookup2HelpFile"),
                  label= "Lookup 2 Help File",
                  multiple = FALSE,
                  accept = ".pdf",
                  width = "95%",
                  buttonLabel = "Lookup 2 Help File...",
                  placeholder = paste0(myEnv$config$lookup2HelpFile),
                  capture = NULL
                ),
                checkboxInput(
                  inputId = ns("lookup3Enabled"),
                  label = "Enable Lookup 3",
                  width = "95%",
                  value = myEnv$config$lookup3Enabled
                ),
                textInput(
                  inputId = ns("lookup3Label"),
                  label = "Lookup 3 Label",
                  value = paste0(myEnv$config$lookup3Label),
                  width = "95%"
                ),
                fileInput(
                  inputId= ns("lookup3CsvFile"),
                  label= "Lookup 3 csv File",
                  multiple = FALSE,
                  accept = ".csv",
                  width = "95%",
                  buttonLabel = "Lookup 3 csv File...",
                  placeholder = paste0(myEnv$config$lookup3CsvFile),
                  capture = NULL
                ),
                fileInput(
                  inputId= ns("lookup3HelpFile"),
                  label= "Lookup 1 Help File",
                  multiple = FALSE,
                  accept = ".pdf",
                  width = "95%",
                  buttonLabel = "Lookup 3 Help File...",
                  placeholder = paste0(myEnv$config$lookup3HelpFile),
                  capture = NULL
                ),
                checkboxInput(
                  inputId = ns("lookup4Enabled"),
                  label = "Enable Lookup 4",
                  value = myEnv$config$lookup4Enabled
                ),
                textInput(
                  inputId = ns("lookup4Label"),
                  label = "Lookup 4 Label",
                  value = paste0(myEnv$config$lookup4Label),
                  width = "95%"
                ),
                fileInput(
                  inputId= ns("lookup4CsvFile"),
                  label= "Lookup 4 csv File",
                  multiple = FALSE,
                  accept = ".csv",
                  width = "95%",
                  buttonLabel = "Lookup 4 csv File...",
                  placeholder = paste0(myEnv$config$lookup4CsvFile),
                  capture = NULL
                ),
                fileInput(
                  inputId= ns("lookup4HelpFile"),
                  label= "Lookup 4 Help File",
                  multiple = FALSE,
                  accept = ".pdf",
                  width = "95%",
                  buttonLabel = "Lookup 4 Help File...",
                  placeholder = paste0(myEnv$config$lookup4HelpFile),
                  capture = NULL
                )
              )
            )
          )
        ),
        tabPanel(
          title = "About This Software",
          gridlayout::grid_container(
            layout = c(
              "aboutSettings ."
            ),
            gap_size = "10px",
            col_sizes = c(
              "560px"
            ),
            row_sizes = c(
              "790px"
            ),
            gridlayout::grid_card(
              area = "aboutSettings",
              bslib::card_header(""),
              bslib::card_body(
                fillable = TRUE,
                fill = TRUE,
                padding = 10,
                max_height = "790px",
                span("This was developed to assist in the annotation of panospheric imagery collected during vegetation surveys."),
                span("It makes extensive use of:"),
                tags$a(href="https://exiftool.org/", "ExifTool"),
                span("By Phil Harvey"),
                span("and:"),
                tags$a(href="https://www.leafletjs.com", "Leaflet"),
                span("By Volodymyr Agafonkin"),
                span("and:"),
                tags$a(href="https://pannellum.org/", "Pannellum"),
                span("By Matthew Petroff"),
                tags$a(href="https://github.com/mpetroff/pannellum/blob/master/COPYING", "Pannellum License")
              )
            )
          )
        )
      ),
      circle = TRUE, status = "primary", size="xs",
      icon = icon("gear"), right=TRUE,
      margin="5px",
      width="600px",
      inputId="settingsBtn",
      tooltip = shinyWidgets::tooltipOptions(title = "Click to set app configurations!")
    )
    ),
    #########################
    # end of dropdownButton #
    #########################

    shinyWidgets::pickerInput(
      inputId =  ns("user_name"),
      label = "User Name",
      choices = myEnv$var_choices,
      #selected = myEnv$var_choices[1],
      multiple = FALSE,
      width = "100%",
      options = list(container = "body", title = "FIRST: Select Your Name")
    ), #%>% shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle"),

    htmlOutput(ns("infoText")),

    tags$h2("Help Files:"),


    actionButton(inputId = ns("lookup1_help"), label = paste0(myEnv$config$lookup1Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", myEnv$config$lookup1HelpFile,"', '_blank')")),
    actionButton(inputId = ns("lookup2_help"), label = paste0(myEnv$config$lookup2Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", myEnv$config$lookup2HelpFile,"', '_blank') ")),
    actionButton(inputId = ns("lookup3_help"), label = paste0(myEnv$config$lookup3Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", myEnv$config$lookup3HelpFile,"', '_blank') ")),
    actionButton(inputId = ns("lookup4_help"), label = paste0(myEnv$config$lookup4Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", myEnv$config$lookup4HelpFile,"', '_blank') ")),
    tags$hr(),

    tags$div(style="align-content:end",
    #actionButton(inputId = ns("save_annotations"), label = "Save All Records", icon = icon("save"), style = "margin-bottom: 5px;"),
    shinyFiles::shinyDirButton(id=ns("export_annotations"), label='Export All Records', title='Please select a folder to export the annotations into :)',icon=icon("download"), multiple=FALSE, viewtype="list", style = "margin-bottom: 5px;"),

    actionButton(inputId = ns("add_whole_image_annotation"), label = "Add A Whole Image Annotation", icon = icon("plus"), style = "margin-bottom: 5px;"),
    #actionButton(inputId = ns("remove_all_annotations_for_image"), label = "Delete All Annotations For Image", icon = icon("trash")),
    ),
    #div(id = ns("card_container")),
  )
}

#' control_form Server Functions
#'
#' @noRd
mod_control_form_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # hide the map and image panels
    golem::invoke_js("hideid", "map_panel")
    golem::invoke_js("hideid", "image_panel")
    #disable the save button at first
    ####shinyjs::disable("save_annotations")
    shinyjs::disable("export_annotations")
    shinyjs::disable("add_whole_image_annotation")
    #shinyjs::disable("applySettingsButton")

    if(myEnv$config$lookup2Enabled == FALSE){
      shinyjs::hide("lookup2_help")
    }
    if(myEnv$config$lookup3Enabled == FALSE){
      shinyjs::hide("lookup3_help")
    }
    if(myEnv$config$lookup4Enabled == FALSE){
      shinyjs::hide("lookup4_help")
    }


    if(myEnv$config$showPopupAlerts == TRUE){
      shinyWidgets::show_alert(
        title = "Configure The App",
        text = "Use the cog icon (top right) to set your custom user files before you start annotating!!!",
        type = "info"
      )
    }


    #event triggered on selecting username
    observe({
      r$user_name <- stringr::str_squish(input$user_name)
      req(r$user_name)
      #print(paste0(app_sys("/app/www/"),"/", r$user_name, "s_annotations.rds"))
      r$user_annotations_file_name <- paste0(app_sys("/app/www/"),"/", r$user_name, "s_annotations.rds")
      r$user_annotations_data <- check_for_saved_data(r$user_annotations_file_name)
      golem::invoke_js("showid", "map_panel")
      if(myEnv$config$showPopupAlerts == TRUE){
       shinyWidgets::show_alert(
         title = "Next.. Select a google earth file (.kmz)",
         text = "A .kmz file contains several images to annotate, once loaded you can select each image to annotate it.",
         type = "info"
       )
      }

    }) %>% bindEvent(input$user_name)

    # output for text info
    output$infoText <- renderUI({
      req(r$user_name, r$current_image )
      if(nchar(r$user_name)>0){
        if(nchar(r$current_image)>0){
          shinyjs::enable("export_annotations")
          shinyjs::enable("add_whole_image_annotation")
          #shinyjs::enable("applySettingsButton")
          str1 <- paste0("<b>Annotation File:</b> ", r$user_name, "s_annotations.rds")
          str2 <- paste0("<b>Image File:</b> <small>", r$current_image, "</small><hr>")
          HTML(paste(str1, str2, sep ='<br/>'))
        }
        else {
          shinyjs::disable("export_annotations")
          shinyjs::disable("add_whole_image_annotation")
          #shinyjs::disable("applySettingsButton")
        }
      }
    })

#####################################
#    Observers for form settings    #
#####################################

    #event triggered on apply settings button
    observe({
      #print("Apply Settings Button Clicked")
      myEnv$config <- configr::read.config(app_sys("extdata/user-config.yml"))
      #r$refresh_user_config <- TRUE
      refresh_user_config(session)
    }) %>% bindEvent(input$applySettingsButton)

    #event triggered on showPopupAlerts checkbox
    observe({
      req(r$config)
      r$config["showPopupAlerts"] <- input$showPopupAlerts
      #print("showPopupAlerts changed")
      save_user_config("showPopupAlerts")
    }) %>% bindEvent(input$showPopupAlerts)


    #changes to config form
    observe({
      req(r$config)
      r$config["appTheme"] <- input$appTheme
      #print("appTheme changed")
      save_user_config("appTheme")
    }) %>% bindEvent(input$appTheme)

    ########################################
    # dynamically change the sliders
    ########################################
    # observe for mapPanelWidth
    observe({
      req(r$config)
      #r$config["mapPanelWidth"] <- input$mapPanelWidth

      # Calculate the total width of all panels
      totalWidth <- input$mapPanelWidth + input$panoPanelWidth + input$formPanelWidth
      #print(paste0("TotalWidth: ",totalWidth))

      # Adjust panoPanelWidth to ensure the sum is 12
      if (totalWidth > 12) {
        excessWidth <- totalWidth - 12
        #print(paste0("excessWidth: ", excessWidth))
        # Calculate new value for panoPanelWidth, ensuring it does not fall below its minimum
        newPanoWidth <- max(3, input$panoPanelWidth - excessWidth)
        updateSliderInput(session, "panoPanelWidth", value = newPanoWidth)
        r$config["panoPanelWidth"] <- newPanoWidth  # Update server-side configuration
      } else if (totalWidth < 12) {
        missingWidth <- 12 - totalWidth
        #print(paste0("missingWidth: ", missingWidth))
        # Calculate new value for panoPanelWidth, ensuring it does not exceed its maximum
        newPanoWidth <- min(6, input$panoPanelWidth + missingWidth)
        updateSliderInput(session, "panoPanelWidth", value = newPanoWidth)
        r$config["panoPanelWidth"] <- newPanoWidth  # Update server-side configuration
      }

      # Save configuration after adjustment
      #save_user_config("mapPanelWidth")
      #save_user_config("panoPanelWidth")  # Save the panoPanelWidth if it was adjusted

      # Trigger SweetAlert confirmation popup
      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirm_change"),
        title = "Resize Panels?",
        text = "Are you sure you want to change the panels layout? The page will reload",
        type = "warning",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#B00225", "#2A52BE"),
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        allowEscapeKey = TRUE,
        cancelOnDismiss = TRUE,
        html = FALSE
      )


    }) %>% bindEvent(input$mapPanelWidth, ignoreInit = TRUE)

    # Respond to SweetAlert confirmation
    observe({
      if (isTRUE(input$confirm_change)) {
        # If user clicked 'Yes', reload the session
        r$config["mapPanelWidth"] <- input$mapPanelWidth
        r$config["panoPanelWidth"] <- input$panoPanelWidth
        r$config["formPanelWidth"] <- input$formPanelWidth
        save_user_config("mapPanelWidth")
        save_user_config("panoPanelWidth")
        save_user_config("formPanelWidth")

        myEnv$config$mapPanelWidth <- input$mapPanelWidth
        myEnv$config$panoPanelWidth <- input$panoPanelWidth
        myEnv$config$formPanelWidth <- input$formPanelWidth
        shinyjs::delay(1000, session$reload())
        shinyjs::delay(2000, shinyjs::runjs('window.location.reload();'))
      } else {
        # If user clicked 'No', revert to the previous selection

      }
    }) %>% bindEvent(input$confirm_change)

    # observe for panoPanelWidth
    observe({
      req(r$config)
      #r$config["panoPanelWidth"] <- input$panoPanelWidth

      # Calculate the total width of all panels
      totalWidth <- input$mapPanelWidth + input$panoPanelWidth + input$formPanelWidth
      #print(paste0("TotalWidth: ", totalWidth))

      # Adjust mapPanelWidth to ensure the sum is 12
      if (totalWidth > 12) {
        excessWidth <- totalWidth - 12
        #print(paste0("excessWidth: ", excessWidth))
        # Calculate new value for mapPanelWidth, ensuring it does not fall below its minimum
        newMapWidth <- max(3, input$mapPanelWidth - excessWidth)
        updateSliderInput(session, "mapPanelWidth", value = newMapWidth)
        r$config["mapPanelWidth"] <- newMapWidth  # Update server-side configuration
      } else if (totalWidth < 12) {
        missingWidth <- 12 - totalWidth
        #print(paste0("missingWidth: ", missingWidth))
        # Calculate new value for mapPanelWidth, ensuring it does not exceed its maximum
        newMapWidth <- min(6, input$mapPanelWidth + missingWidth)
        updateSliderInput(session, "mapPanelWidth", value = newMapWidth)
        r$config["mapPanelWidth"] <- newMapWidth  # Update server-side configuration
      }

      # Save configuration after adjustment
      #save_user_config("panoPanelWidth")
      #save_user_config("mapPanelWidth")  # Save the mapPanelWidth if it was adjusted
    }) %>% bindEvent(input$panoPanelWidth, ignoreInit = TRUE)

    # observe for formPanelWidth
    observe({
      req(r$config)
      #r$config["formPanelWidth"] <- input$formPanelWidth

      # Calculate the total width of all panels
      totalWidth <- input$mapPanelWidth + input$panoPanelWidth + input$formPanelWidth
      #print(paste0("TotalWidth: ", totalWidth))

      # Adjust panoPanelWidth to ensure the sum is 12
      if (totalWidth > 12) {
        excessWidth <- totalWidth - 12
        #print(paste0("excessWidth: ", excessWidth))
        # Calculate new value for panoPanelWidth, ensuring it does not fall below its minimum
        newPanoWidth <- max(3, input$panoPanelWidth - excessWidth)
        updateSliderInput(session, "panoPanelWidth", value = newPanoWidth)
        r$config["panoPanelWidth"] <- newPanoWidth  # Update server-side configuration
      } else if (totalWidth < 12) {
        missingWidth <- 12 - totalWidth
        #print(paste0("missingWidth: ", missingWidth))
        # Calculate new value for panoPanelWidth, ensuring it does not exceed its maximum
        newPanoWidth <- min(6, input$panoPanelWidth + missingWidth)
        updateSliderInput(session, "panoPanelWidth", value = newPanoWidth)
        r$config["panoPanelWidth"] <- newPanoWidth  # Update server-side configuration
      }

      # Save configuration after adjustment
      #save_user_config("formPanelWidth")
      #save_user_config("panoPanelWidth")  # Save the panoPanelWidth if it was adjusted

      # Trigger SweetAlert confirmation popup
      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("confirm_change"),
        title = "Resize Panels?",
        text = "Are you sure you want to change the panels layout? The app will reload",
        type = "warning",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#B00225", "#2A52BE"),
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        allowEscapeKey = TRUE,
        cancelOnDismiss = TRUE,
        html = FALSE
      )


    }) %>% bindEvent(input$formPanelWidth, ignoreInit = TRUE)


    #map settings observers
    ##########################
    observe({
      req(r$config)
      r$config["mapPanelSource"] <- input$mapPanelSource
      #print("mapPanelSource changed")
      save_user_config("mapPanelSource")
    }) %>% bindEvent(input$mapPanelSource)

    observe({
      req(r$config)
      r$config["mapIconColour"] <- input$mapIconColour
      #print("mapIconColour changed")
      save_user_config("mapIconColour")
    }) %>% bindEvent(input$mapIconColour)

    observe({
      req(r$config)
      r$config["mapMarkerColour"] <- input$mapMarkerColour
      #print("mapMarkerColour changed")
      save_user_config("mapMarkerColour")
    }) %>% bindEvent(input$mapMarkerColour)

    observe({
      req(r$config)
      r$config["mapPolygonStroke"] <- input$mapPolygonStroke
      #print("mapPolygonStroke changed")
      save_user_config("mapPolygonStroke")
    }) %>% bindEvent(input$mapPolygonStroke)

    observe({
      req(r$config)
      r$config["mapPolygonStrokeColour"] <- input$mapPolygonStrokeColour
      #print("mapPolygonStrokeColour changed")
      save_user_config("mapPolygonStrokeColour")
    }) %>% bindEvent(input$mapPolygonStrokeColour)

    observe({
      req(r$config)
      r$config["mapPolygonStrokeWeight"] <- input$mapPolygonStrokeWeight
      #print("mapPolygonStrokeWeight changed")
      save_user_config("mapPolygonStrokeWeight")
    }) %>% bindEvent(input$mapPolygonStrokeWeight)

    observe({
      req(r$config)
      r$config["mapPolygonStrokeOpacity"] <- input$mapPolygonStrokeOpacity
      #print("mapPolygonStrokeOpacity changed")
      save_user_config("mapPolygonStrokeOpacity")
    }) %>% bindEvent(input$mapPolygonStrokeOpacity)

    observe({
      req(r$config)
      r$config["mapPolygonFill"] <- input$mapPolygonFill
      #print("mapPolygonFill changed")
      save_user_config("mapPolygonFill")
    }) %>% bindEvent(input$mapPolygonFill)

    observe({
      req(r$config)
      r$config["mapPolygonFillColour"] <- input$mapPolygonFillColour
      #print("mapPolygonFillColour changed")
      save_user_config("mapPolygonFillColour")
    }) %>% bindEvent(input$mapPolygonFillColour)

    observe({
      req(r$config)
      r$config["mapPolygonFillOpacity"] <- input$mapPolygonFillOpacity
      #print("mapPolygonFillOpacity changed")
      save_user_config("mapPolygonFillOpacity")
    }) %>% bindEvent(input$mapPolygonFillOpacity)

    #Pano 360 Panel observes
    ########################
    observe({
      req(r$config)
      r$config["pano360IconColour"] <- input$pano360IconColour
      #print("pano360IconColour changed")
      save_user_config("pano360IconColour")
    }) %>% bindEvent(input$pano360IconColour)

    observe({
      req(r$config)
      r$config["pano360MarkerColour"] <- input$pano360MarkerColour
      #print("pano360MarkerColour changed")
      save_user_config("pano360MarkerColour")
    }) %>% bindEvent(input$pano360MarkerColour)

    observe({
      req(r$config)
      r$config["pano360PolygonStroke"] <- input$pano360PolygonStroke
      #print("pano360PolygonStroke changed")
      save_user_config("pano360PolygonStroke")
    }) %>% bindEvent(input$pano360PolygonStroke)

    observe({
      req(r$config)
      r$config["pano360PolygonStrokeColour"] <- input$pano360PolygonStrokeColour
      #print("pano360PolygonStrokeColour changed")
      save_user_config("pano360PolygonStrokeColour")
    }) %>% bindEvent(input$pano360PolygonStrokeColour)

    observe({
      req(r$config)
      r$config["pano360PolygonStrokeWeight"] <- input$pano360PolygonStrokeWeight
      #print("pano360PolygonStrokeWeight changed")
      save_user_config("pano360PolygonStrokeWeight")
    }) %>% bindEvent(input$pano360PolygonStrokeWeight)

    observe({
      req(r$config)
      r$config["pano360PolygonStrokeOpacity"] <- input$pano360PolygonStrokeOpacity
      #print("pano360PolygonStrokeOpacity changed")
      save_user_config("pano360PolygonStrokeOpacity")
    }) %>% bindEvent(input$pano360PolygonStrokeOpacity)

    observe({
      req(r$config)
      r$config["showPano360PolygonStrokeInCropExport"] <- input$showPano360PolygonStrokeInCropExport
      #print("showPano360PolygonStrokeInCropExport changed")
      save_user_config("showPano360PolygonStrokeInCropExport")
    }) %>% bindEvent(input$showPano360PolygonStrokeInCropExport)

    observe({
      req(r$config)
      r$config["pano360PolygonFill"] <- input$pano360PolygonFill
      #print("pano360PolygonFill changed")
      save_user_config("pano360PolygonFill")
    }) %>% bindEvent(input$pano360PolygonFill)

    observe({
      req(r$config)
      r$config["pano360PolygonFillColour"] <- input$pano360PolygonFillColour
      #print("pano360PolygonFillColour changed")
      save_user_config("pano360PolygonFillColour")
    }) %>% bindEvent(input$pano360PolygonFillColour)

    observe({
      req(r$config)
      r$config["pano360PolygonFillOpacity"] <- input$pano360PolygonFillOpacity
      #print("pano360PolygonFillOpacity changed")
      save_user_config("pano360PolygonFillOpacity")
    }) %>% bindEvent(input$pano360PolygonFillOpacity)

    observe({
      req(r$config)
      r$config["showPano360PolygonFillInCropExport"] <- input$showPano360PolygonFillInCropExport
      #print("showPano360PolygonFillInCropExport changed")
      save_user_config("showPano360PolygonFillInCropExport")
    }) %>% bindEvent(input$showPano360PolygonFillInCropExport)

    #username lookups observer
    ##################
    observe({
      req(r$config)
      r$config["usernameLookupFile"] <- paste0(input$usernameLookupFile$name)
      #print("usernameLookupFile changed")
      #print(input$usernameLookupFile$name) #print(input$usernameLookupFile$datapath)
      file.copy(input$usernameLookupFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$usernameLookupFile$name)), overwrite = TRUE)
      save_user_config("usernameLookupFile")
    }) %>% bindEvent(input$usernameLookupFile)

    observe({
      req(r$config)
      r$config["exportFileFormat"] <- input$exportFileFormat
      #print("mapPanelSource changed")
      save_user_config("exportFileFormat")
    }) %>% bindEvent(input$exportFileFormat)

    # look ups settings observers
    #############################
    observe({
      req(r$config)
      r$config["lookup1Label"] <- input$lookup1Label
      #print("lookup1Label changed")
      save_user_config("lookup1Label")
    }) %>% bindEvent(input$lookup1Label)

    observe({
      req(r$config)
      r$config["lookup1CsvFile"] <- paste0(input$lookup1CsvFile$name)
      #print("usernameLookupFile changed")
      #print(input$lookup1CsvFile$datapath) #print(input$lookup1CsvFile$name)
      file.copy(input$lookup1CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup1CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup1CsvFile")
    }) %>% bindEvent(input$lookup1CsvFile)

    observe({
      req(r$config)
      r$config["lookup1HelpFile"] <- input$lookup1HelpFile$name
      #print("Lookup1HelpFile changed") #print(input$lookup1HelpFile$datapath)
      file.copy(input$lookup1HelpFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup1HelpFile$name)), overwrite = TRUE)
      save_user_config("lookup1HelpFile")
    }) %>% bindEvent(input$lookup1HelpFile)

    observe({
      req(r$config)
      r$config["lookup2Enabled"] <- input$lookup2Enabled
      #print("lookup2Enabled changed")
      save_user_config("lookup2Enabled")
      if(r$config["lookup2Enabled"] == TRUE){
        shinyjs::enable("lookup2Label")
        shinyjs::enable("lookup2CsvFile")
        shinyjs::enable("lookup2HelpFile")
        shinyjs::show("lookup2Label")
        shinyjs::show("lookup2CsvFile")
        shinyjs::show("lookup2HelpFile")
      } else {
        shinyjs::disable("lookup2Label")
        shinyjs::disable("lookup2CsvFile")
        shinyjs::disable("lookup2HelpFile")
        shinyjs::hide("lookup2Label")
        shinyjs::hide("lookup2CsvFile")
        shinyjs::hide("lookup2HelpFile")
      }
    }) %>% bindEvent(input$lookup2Enabled)

    observe({
      req(r$config)
      r$config["lookup2Label"] <- input$lookup2Label
      #print("lookup2Label changed")
      save_user_config("lookup2Label")
    }) %>% bindEvent(input$lookup2Label)

    observe({
      req(r$config)
      r$config["lookup2CsvFile"] <- input$lookup2CsvFile$name
      #print("Lookup2CsvFile changed") #print(input$lookup2CsvFile$datapath)
      file.copy(input$lookup2CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup2CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup2CsvFile")
    }) %>% bindEvent(input$lookup2CsvFile)

    observe({
      req(r$config)
      r$config["lookup2HelpFile"] <- input$lookup2HelpFile$name
      #print("Lookup2HelpFile changed") #print(input$lookup2HelpFile$datapath)
      file.copy(input$lookup2HelpFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup2HelpFile$name)), overwrite = TRUE)
      save_user_config("lookup2HelpFile")
    }) %>% bindEvent(input$lookup2HelpFile)

    observe({
      req(r$config)
      r$config["lookup3Enabled"] <- input$lookup3Enabled
      #print("lookup3Enabled changed")
      save_user_config("lookup3Enabled")
      if(r$config["lookup3Enabled"] == TRUE){
        shinyjs::enable("lookup3Label")
        shinyjs::enable("lookup3CsvFile")
        shinyjs::enable("lookup3HelpFile")
        shinyjs::show("lookup3Label")
        shinyjs::show("lookup3CsvFile")
        shinyjs::show("lookup3HelpFile")
      } else {
        shinyjs::disable("lookup3Label")
        shinyjs::disable("lookup3CsvFile")
        shinyjs::disable("lookup3HelpFile")
        shinyjs::hide("lookup3Label")
        shinyjs::hide("lookup3CsvFile")
        shinyjs::hide("lookup3HelpFile")
      }
    }) %>% bindEvent(input$lookup3Enabled)

    observe({
      req(r$config)
      r$config["lookup3Label"] <- input$lookup3Label
      #print("lookup3Label changed")
      save_user_config("lookup3Label")
    }) %>% bindEvent(input$lookup3Label)

    observe({
      req(r$config)
      r$config["lookup3CsvFile"] <- input$lookup3CsvFile$name
      #print("Lookup3CsvFile changed") #print(input$lookup3CsvFile$datapath)
      file.copy(input$lookup3CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup3CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup3CsvFile")
    }) %>% bindEvent(input$lookup3CsvFile)

    observe({
      req(r$config)
      r$config["lookup3HelpFile"] <- input$lookup3HelpFile$name
      #print("Lookup3HelpFile changed") #print(input$lookup3HelpFile$datapath)
      file.copy(input$lookup3HelpFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup3HelpFile$name)), overwrite = TRUE)
      save_user_config("lookup3HelpFile")
    }) %>% bindEvent(input$lookup3HelpFile)

    observe({
      req(r$config)
      r$config["lookup4Enabled"] <- input$lookup4Enabled
      #print("lookup4Enabled changed")
      save_user_config("lookup4Enabled")
      if(r$config["lookup4Enabled"] == TRUE){
        shinyjs::enable("lookup4Label")
        shinyjs::enable("lookup4CsvFile")
        shinyjs::enable("lookup4HelpFile")
        shinyjs::show("lookup4Label")
        shinyjs::show("lookup4CsvFile")
        shinyjs::show("lookup4HelpFile")
      } else {
        shinyjs::disable("lookup4Label")
        shinyjs::disable("lookup4CsvFile")
        shinyjs::disable("lookup4HelpFile")
        shinyjs::hide("lookup4Label")
        shinyjs::hide("lookup4CsvFile")
        shinyjs::hide("lookup4HelpFile")
      }
    }) %>% bindEvent(input$lookup4Enabled)

    observe({
      req(r$config)
      r$config["lookup4Label"] <- input$lookup4Label
      #print("lookup4Label changed")
      save_user_config("lookup4Label")
    }) %>% bindEvent(input$lookup4Label)

    observe({
      req(r$config)
      r$config["lookup4CsvFile"] <- input$lookup4CsvFile$name
      #print("Lookup4CsvFile changed") #print(input$lookup4CsvFile$datapath)
      file.copy(input$lookup4CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup4CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup4CsvFile")
    }) %>% bindEvent(input$lookup4CsvFile)

    observe({
      req(r$config)
      r$config["lookup4HelpFile"] <- input$lookup4HelpFile$name
      #print("Lookup4HelpFile changed") #print(input$lookup4HelpFile$datapath)
      file.copy(input$lookup4HelpFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup4HelpFile$name)), overwrite = TRUE)
      save_user_config("lookup4HelpFile")
    }) %>% bindEvent(input$lookup4HelpFile)

# end settings panels observers
####################################

    #add new whole image annotation record button clicked
    observe({
      #print("add a whole image annotation clicked")
      req(r$current_image, r$current_image_metadata, r$user_name)

      myId <- gsub("\\.", "",format(Sys.time(), "%Y%m%d-%H%M%OS3"))
      geomType <- "Point-whole-image-annotation"
      lat <- r$current_image_metadata$Latitude
      long <- r$current_image_metadata$Longitude
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

   }) %>% bindEvent(input$add_whole_image_annotation)

  # when new map item added, listening for both button clicked in form OR item drawn in map panel
  observe({
    #print("new map item added: mod_control_form")

    #str <- sprintf("new feature with layerId: %s", r$new_leafletMap_item)
    #print(str)

    # Convert the feature with the new ID to a sf object
    options(digits=9)
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
      myDD4 = NA
    )

  }) %>% bindEvent(r$new_leafletMap_item)

  # when new 360 item added, listening for drawing in 360 panel
  observe({
    #print("new 360 item added: mod_control_form")

    #str <- sprintf("new feature with layerId: %s", r$new_leaflet360_item)
    #print(str)

    # Convert the feature with the new ID to a sf object
    options(digits=9)
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
      myDD4 = NA
    )

  }) %>% bindEvent(r$new_leaflet360_item)

 # when save annotations button is clicked
  # observe({
  #     print("save annotations clicked")
  #     req(r$user_annotations_data, r$user_annotations_file_name)
  #     save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
  #  if(myEnv$config$showPopupAlerts == TRUE){
  #if(myEnv$config$showPopupAlerts == TRUE){
  #     shinyWidgets::show_alert(
  #       title = "Annotation Saved!",
  #       text = "Awesome, saved the annotation, select another image and annotate it.",
  #       type = "success"
  #     )
  #}
  #  }
  #
  # }) %>% bindEvent(input$save_annotations)


#######################################

    #check if there are any annotations for a selected image already
    observe({
      #print("current image changed: mod_control_form")
      req(r$user_annotations_data, r$current_image)

      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
      clear_annotations_form()

      previous_annotations <- check_for_annotations(r$user_annotations_data, r$current_image)

      if(nrow(previous_annotations > 1)){
        #print("annotations already exist")
        for(i in 1:nrow(previous_annotations)){
          #View(previous_annotations)
          add_annotations_form(input=input, myActiveAnnotations=r$active_annotations, myId=previous_annotations[i, "id"], myFeatureType=previous_annotations[i, "feature_type"], myRadius=previous_annotations[i, "radius"], myGeometry= previous_annotations[i, "geometry"], myDD1= previous_annotations[i, "dd1"],myDD2= previous_annotations[i, "dd2"], myDD3=previous_annotations[i, "dd3"], myDD4=previous_annotations[i, "dd4"])
           }

         if(myEnv$config$showPopupAlerts == TRUE){
            #tell the user annotations already exist
          shinyWidgets::show_alert(
           title = "Annotations Already Exist!",
           text = "It looks like you've already done this one :) I've loaded that data....",
           type = "info"
          )
         }

      }
    }) %>% bindEvent(r$current_image)

    #export annotations button
    observe({
      req(r$user_annotations_file_name,  r$user_annotations_data)
      home_dir <- fs::path_home()
      documents_dir <- file.path(home_dir, "Documents")

      # Create volumes list containing only the Documents folder
      volumes <- c(Documents = documents_dir, shinyFiles::getVolumes()())
      #volumes <- c(Documents = fs::path_home(), "R Installation" = R.home(),shinyFiles::getVolumes()())

      if (is.integer(input$export_annotations)) {
        cat("No directory has been selected (shinyDirChoose)")
        shinyFiles::shinyDirChoose(input,"export_annotations",roots = volumes, session = session, restrictions = system.file(package = "base"))
      } else {
        annotations_export_dir <- shinyFiles::parseDirPath(volumes, input$export_annotations)
        annotations_export_full_path_rds <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.rds")
        annotations_export_full_path_csv <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.csv")

        save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

        if(myEnv$config$exportFileFormat == "rds"){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user","id", "imagefile", "feature_type", "radius", "geometry", paste0(myEnv$config$lookup1Label), paste0(myEnv$config$lookup2Label), paste0(myEnv$config$lookup3Label), paste0(myEnv$config$lookup4Label))
          saveRDS(temp_df, file=annotations_export_full_path_rds)
          rm(temp_df)
          #file.copy(r$user_annotations_file_name, annotations_export_full_path_rds, overwrite = TRUE)
          #if(myEnv$config$showPopupAlerts == TRUE){
            shinyWidgets::show_alert(
              title = "Export Successful!",
              text = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_rds )),
              html = TRUE,
              type = "success"
            )
          #}
        }
        else if (myEnv$config$exportFileFormat == "csv"){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user", "id", "imagefile", "feature_type", "radius", "geometry", paste0(myEnv$config$lookup1Label), paste0(myEnv$config$lookup2Label), paste0(myEnv$config$lookup3Label), paste0(myEnv$config$lookup4Label))
          utils::write.csv(temp_df, annotations_export_full_path_csv, fileEncoding = "UTF-8", row.names=FALSE)
          rm(temp_df)
          #if(myEnv$config$showPopupAlerts == TRUE){
            shinyWidgets::show_alert(
              title = "Export Successful!",
              text = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_csv )),
              html = TRUE,
              type = "success"
            )
          #}
        }

      }

    }) %>% bindEvent(input$export_annotations)



    # refresh_the form triggered when the apply settings button is clicked and user changes settings
    observe({
      #print("refresh_for_item: control_form")
      req(r$refresh_user_config, r$user_annotations_data, r$current_image)

      #req(r$user_annotations_data, r$current_image)

      #call the functions to create the icons using the colours etc from the settings panel
      myEnv$mapIcons <- create_map_icons()
      myEnv$formIcons <- create_form_icons()

      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
      clear_annotations_form()

      previous_annotations <- check_for_annotations(r$user_annotations_data, r$current_image)

      if(nrow(previous_annotations > 1)){
        #print("annotations already exist")
        for(i in 1:nrow(previous_annotations)){
          #View(previous_annotations)
          add_annotations_form(input=input, myActiveAnnotations=r$active_annotations, myId=previous_annotations[i, "id"], myFeatureType=previous_annotations[i, "feature_type"], myRadius=previous_annotations[i, "radius"], myGeometry= previous_annotations[i, "geometry"], myDD1= previous_annotations[i, "dd1"],myDD2= previous_annotations[i, "dd2"], myDD3=previous_annotations[i, "dd3"], myDD4=previous_annotations[i, "dd4"])
        }

      }


    }) %>% bindEvent(r$refresh_user_config)


  })
}

## To be copied in the UI
# mod_control_form_ui("control_form")

## To be copied in the server
# mod_control_form_server("control_form")

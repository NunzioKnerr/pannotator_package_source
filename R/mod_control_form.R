#' control_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyhelper
#' @import shinyWidgets
#' @import shinyjs

mod_control_form_ui <- function(id){
  ns <- NS(id)

  the$config <- configr::read.config(app_sys("extdata/user-config.yml"))

  #the environment setup in fct_helpers.R file
  the$var_choices <- load_lookup(
    fileToLoad = app_sys(the$config$usernameLookupFile),
    display_column = "user_name",
    value_column = "value")

  the$var_dropdown1 <- load_lookup(
    fileToLoad = app_sys(the$config$lookup1CsvFile),
    display_column = "display",
    value_column = "value")

  the$var_dropdown2 <- load_lookup(
    fileToLoad = app_sys(the$config$lookup2CsvFile),
    display_column = "display",
    value_column = "value")

  the$var_dropdown3 <- load_lookup(
    fileToLoad = app_sys(the$config$lookup3CsvFile),
    display_column = "display",
    value_column = "value"
  )

  the$var_dropdown4 <- load_lookup(
    fileToLoad = app_sys(the$config$lookup4CsvFile),
    display_column = "display",
    value_column = "value"
  )

  tagList(
    tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

    ###################################
    # config form in drop down button #
    ###################################
    tags$div(style="margin:10px",
             dropdownButton(

               navbarPage(
                 title = "PannotatoR",
                 id = "tabset-default-id",
                 selected = "Main Settings",
                 collapsible = TRUE,
                 #theme = shinythemes::shinytheme(the$config$appTheme),
                 #theme =  bslib::bs_theme(version = 3, bootswatch = "default"),
                 #theme = bslib::bs_theme(bootswatch = the$config$appTheme),
                 tabPanel(
                   title = "Main Settings",
                   gridlayout::grid_container(
                     layout = c(
                       "mainSettings ."
                     ),
                     gap_size = "10px",
                     col_sizes = c(
                       "550px"
                     ),
                     row_sizes = c(
                       "790px"
                     ),
                     gridlayout::grid_card(
                       area = "mainSettings",
                       bslib::card_header("Note: Changes will be applied after you relaunch the app -- :)"),
                       bslib::card_body(
                         fillable = TRUE,
                         fill = TRUE,
                         max_height = "790px",
                         padding = 10,
                         h2(strong("Layout Settings")),
                         # selectInput(
                         #   inputId = ns("appTheme"),
                         #   label = "App Theme",
                         #   width = "95%",
                         #   selected = the$config$appTheme,
                         #   choices = list("cerulean" = "cerulean", "cosmo" = "cosmo", "flatly" = "flatly", "paper" = "paper", "yeti" = "yeti")
                         # ),
                         sliderInput(
                           inputId = ns("mapPanelWidth"),
                           label = "Map Panel Width",
                           min = 3,
                           max = 7,
                           value = the$config$mapPanelWidth,
                           step = 1,
                           width = "95%"
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         sliderInput(
                           inputId = ns("panoPanelWidth"),
                           label = "Pano 360 Panel Width",
                           min = 3,
                           max = 7,
                           value = the$config$panoPanelWidth,
                           step = 1,
                           width = "95%"
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         sliderInput(
                           inputId = ns("formPanelWidth"),
                           label = "Form Panel Width",
                           min = 1,
                           max = 3,
                           value = the$config$formPanelWidth,
                           step = 1,
                           width = "95%"
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         h2(strong("Mapping Panel Settings")),
                         selectInput(
                           inputId = ns("mapPanelSource"),
                           label = "Leaflet Map Source",
                           width = "95%",
                           selected = the$config$mapPanelSource,
                           choices = list("Esri WorldImagery" = "Esri.WorldImagery", "Esri WorldTopoMap" = "Esri.WorldTopoMap", "OpenStreetMap" = "OpenStreetMap", "Stamen Terrain" = "Stamen.Terrain")
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         h2(strong("Form Settings")),
                         fileInput(
                           inputId= ns("usernameLookupFile"),
                           label= "Username File",
                           multiple = FALSE,
                           accept = ".csv",
                           width = "95%",
                           buttonLabel = "Browse...",
                           placeholder = paste0(the$config$usernameLookupFile),
                           capture = NULL
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         selectInput(
                           inputId = ns("exportFileFormat"),
                           label = "Export File Format",
                           width = "95%",
                           selected = the$config$exportFileFormat,
                           choices = list("csv" = "csv", "rds" = "rds")
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m")
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
                     gap_size = "15px",
                     col_sizes = c(
                       "550px"
                     ),
                     row_sizes = c(
                       "790px"
                     ),
                     gridlayout::grid_card(
                       area = "lookupSettings",
                       bslib::card_header("Note: Changes will be applied after you relaunch the app -- :)"),
                       bslib::card_body(
                         fillable = TRUE,
                         fill = TRUE,
                         padding = 10,
                         max_height = "790px",
                         textInput(
                           inputId = ns("lookup1Label"),
                           label = "Lookup 1 Label",
                           value = paste0(the$config$lookup1Label),
                           width = "95%"
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         fileInput(
                           inputId= ns("lookup1CsvFile"),
                           label= "Lookup 1 csv File",
                           multiple = FALSE,
                           accept = ".csv",
                           width = "95%",
                           buttonLabel = "Lookup 1 csv File...",
                           placeholder = paste0(the$config$lookup1CsvFile),
                           capture = NULL
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         fileInput(
                           inputId= ns("lookup1HelpFile"),
                           label= "Lookup 1 Help File",
                           multiple = FALSE,
                           accept = ".pdf",
                           width = "95%",
                           buttonLabel = "Lookup 1 Help File...",
                           placeholder = paste0(the$config$lookup1HelpFile),
                           capture = NULL
                         ) |> shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle", size = "m"),
                         checkboxInput(
                           inputId = ns("lookup2Enabled"),
                           label = "Enable Lookup 2",
                           value = the$config$lookup2Enabled
                         ),
                         textInput(
                           inputId = ns("lookup2Label"),
                           label = "Lookup 2 Label",
                           value = paste0(the$config$lookup2Label),
                           width = "95%"
                         ),
                         fileInput(
                           inputId= ns("lookup2CsvFile"),
                           label= "Lookup 2 csv File",
                           multiple = FALSE,
                           accept = ".csv",
                           width = "95%",
                           buttonLabel = "Lookup 2 csv File...",
                           placeholder = paste0(the$config$lookup2CsvFile),
                           capture = NULL
                         ),
                         fileInput(
                           inputId= ns("lookup2HelpFile"),
                           label= "Lookup 2 Help File",
                           multiple = FALSE,
                           accept = ".pdf",
                           width = "95%",
                           buttonLabel = "Lookup 2 Help File...",
                           placeholder = paste0(the$config$lookup2HelpFile),
                           capture = NULL
                         ),
                         checkboxInput(
                           inputId = ns("lookup3Enabled"),
                           label = "Enable Lookup 3",
                           value = the$config$lookup3Enabled
                         ),
                         textInput(
                           inputId = ns("lookup3Label"),
                           label = "Lookup 3 Label",
                           value = paste0(the$config$lookup3Label),
                           width = "95%"
                         ),
                         fileInput(
                           inputId= ns("lookup3CsvFile"),
                           label= "Lookup 3 csv File",
                           multiple = FALSE,
                           accept = ".csv",
                           width = "95%",
                           buttonLabel = "Lookup 3 csv File...",
                           placeholder = paste0(the$config$lookup3CsvFile),
                           capture = NULL
                         ),
                         fileInput(
                           inputId= ns("lookup3HelpFile"),
                           label= "Lookup 1 Help File",
                           multiple = FALSE,
                           accept = ".pdf",
                           width = "95%",
                           buttonLabel = "Lookup 3 Help File...",
                           placeholder = paste0(the$config$lookup3HelpFile),
                           capture = NULL
                         ),
                         checkboxInput(
                           inputId = ns("lookup4Enabled"),
                           label = "Enable Lookup 4",
                           value = the$config$lookup4Enabled
                         ),
                         textInput(
                           inputId = ns("lookup4Label"),
                           label = "Lookup 4 Label",
                           value = paste0(the$config$lookup4Label),
                           width = "95%"
                         ),
                         fileInput(
                           inputId= ns("lookup4CsvFile"),
                           label= "Lookup 4 csv File",
                           multiple = FALSE,
                           accept = ".csv",
                           width = "95%",
                           buttonLabel = "Lookup 4 csv File...",
                           placeholder = paste0(the$config$lookup4CsvFile),
                           capture = NULL
                         ),
                         fileInput(
                           inputId= ns("lookup4HelpFile"),
                           label= "Lookup 4 Help File",
                           multiple = FALSE,
                           accept = ".pdf",
                           width = "95%",
                           buttonLabel = "Lookup 4 Help File...",
                           placeholder = paste0(the$config$lookup4HelpFile),
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
                     gap_size = "15px",
                     col_sizes = c(
                       "550px"
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
               tooltip = tooltipOptions(title = "Click to set app configurations!")
             ),
             ##################
             #tags$div(style="margin:10px",
             shinyWidgets::pickerInput(
               inputId =  ns("user_name"),
               label = "User Name",
               choices = the$var_choices,
               multiple = FALSE,
               width = "100%",
               options = list(container = "body", title = "FIRST: Select Your Name")
             ), #%>% shinyhelper::helper(type = "markdown", content = "user_name", icon = "question-circle"),
             # shiny::selectInput(inputId = ns("user_name"),
             #                    label = "User Name",
             #                    choices =  the$var_choices,
             #                    selected = NULL,
             #                    multiple = FALSE,
             #                    width = "100%"),
             htmlOutput(ns("infoText")),
             tags$h2("Help Files:"),
             actionButton(inputId = ns("lookup1_help"), label = paste0(the$config$lookup1Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", basename(the$config$lookup1HelpFile),"', '_blank') ")),
             actionButton(inputId = ns("lookup2_help"), label = paste0(the$config$lookup2Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", basename(the$config$lookup2HelpFile),"', '_blank') ")),
             actionButton(inputId = ns("lookup3_help"), label = paste0(the$config$lookup3Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", basename(the$config$lookup3HelpFile),"', '_blank') ")),
             actionButton(inputId = ns("lookup4_help"), label = paste0(the$config$lookup4Label, " Help"), icon = icon("question-circle"), onclick =paste0("window.open(' ./extdata/", basename(the$config$lookup4HelpFile),"', '_blank') ")),
             tags$hr(),
             tags$div(style="align-content:end",
             actionButton(inputId = ns("save_annotation"), label = "Save Record", icon = icon("save")),
             shinyFiles::shinyDirButton(id=ns("export_annotations"), label='Export Records', title='Please select a folder to export the annotations into :)',icon=icon("download"), multiple=FALSE, viewtype="list")
             ),
             tags$hr(),
             tags$div(
               actionButton(inputId = ns("add_module"), label = "Add New Record", icon = icon("plus")),
               tags$b("      "),
               actionButton(inputId = ns("remove_module"), label = "Remove Last Record", icon = icon("trash")),
               tableOutput(ns("show_inputs"))
             )
    )
  )
}

#' control_form Server Functions
#'
#' @noRd
mod_control_form_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #addCssClass(class = "bttn bttn-unite bttn-default bttn-no-outline", selector = ".btn-file")
    #to chenge the browse buttons to look like the shinywidgets
    #addCssClass(class = "bttn bttn-fill bttn-md bttn-default shiny-bound-input", selector = ".btn-file")
    #r$config <- get_user_config()

    golem::invoke_js("hideid", "map_panel")
    golem::invoke_js("hideid", "image_panel")

    if(the$config$lookup2Enabled == FALSE){
      shinyjs::hide("lookup2_help")
    }
    if(the$config$lookup3Enabled == FALSE){
      shinyjs::hide("lookup3_help")
    }
    if(the$config$lookup4Enabled == FALSE){
      shinyjs::hide("lookup4_help")
    }

    shinyWidgets::show_alert(
      title = "Configure The App",
      text = "Use the cog icon (top right) to set your custom user files before you start annotating!!!",
      type = "success"
    )

    # shinyWidgets::show_alert(
    #   title = "FIRST: Select Your <br> User Name",
    #   text = "That way I can save the data you create. Happy Annotating!!!",
    #   type = "success"
    # )

    #disable the save button at first
    shinyjs::disable("save_annotation")
    shinyjs::disable("export_annotations")
    shinyjs::disable("add_module")
    shinyjs::disable("remove_module")

    #event triggered on selecting username
    observe({
      r$user_name <- stringr::str_squish(input$user_name)
      req(r$user_name)
      #print(paste0(app_sys("/app/www/"),"/", r$user_name, "s_annotations.rds"))
      r$user_annotations_file_name <- paste0(app_sys("/app/www/"),"/", r$user_name, "s_annotations.rds")
      r$user_annotations_data <- check_for_saved_data(r$user_annotations_file_name)
      golem::invoke_js("showid", "map_panel")
      shinyWidgets::show_alert(
        title = "Next.. Select a google earth file (.kmz)",
        text = "A .kmz file contains several images to annotate, once loaded you can select each image to annotate it.",
        type = "success"
      )
    }) %>% bindEvent(input$user_name)

    #changes to config form
    # observe({
    #   req(r$config)
    #   r$config["appTheme"] <- input$appTheme
    #   print("appTheme changed")
    #   save_user_config("appTheme")
    #   #session$setCurrentTheme(bslib::bs_theme(bootswatch = input$appTheme))
    # }) %>% bindEvent(input$appTheme)

    observe({
      req(r$config)
      r$config["mapPanelWidth"] <- input$mapPanelWidth
      #print("MapPanelWidth changed")
      #print(r$config["mapPanelWidth"])
      save_user_config("mapPanelWidth")
    }) %>% bindEvent(input$mapPanelWidth)

    observe({
      req(r$config)
      r$config["panoPanelWidth"] <- input$panoPanelWidth
      #print("panoPanelWidth changed")
      save_user_config("panoPanelWidth")
    }) %>% bindEvent(input$panoPanelWidth)

    observe({
      req(r$config)
      r$config["formPanelWidth"] <- input$formPanelWidth
      #print("formPanelWidth changed")
      save_user_config("formPanelWidth")
    }) %>% bindEvent(input$formPanelWidth)

    observe({
      req(r$config)
      r$config["mapPanelSource"] <- input$mapPanelSource
      #print("mapPanelSource changed")
      save_user_config("mapPanelSource")
    }) %>% bindEvent(input$mapPanelSource)

    observe({
      req(r$config)
      r$config["usernameLookupFile"] <- paste0("extdata","/",input$usernameLookupFile$name)
      #print("usernameLookupFile changed")
      #print(input$usernameLookupFile$datapath)
      #print(input$usernameLookupFile$name)
      #print(paste0(app_sys("./extdata"),"/",input$usernameLookupFile$name))
      file.copy(input$usernameLookupFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$usernameLookupFile$name)), overwrite = TRUE)
      save_user_config("usernameLookupFile")
    }) %>% bindEvent(input$usernameLookupFile)

    observe({
      req(r$config)
      r$config["exportFileFormat"] <- input$exportFileFormat
      #print("mapPanelSource changed")
      save_user_config("exportFileFormat")
    }) %>% bindEvent(input$exportFileFormat)

    observe({
      req(r$config)
      r$config["lookup1Label"] <- input$lookup1Label
      #print("lookup1Label changed")
      save_user_config("lookup1Label")
    }) %>% bindEvent(input$lookup1Label)

    observe({
      req(r$config)
      r$config["lookup1CsvFile"] <- paste0("./extdata","/",input$lookup1CsvFile$name)
      print("usernameLookupFile changed")
      #print(input$lookup1CsvFile$datapath)
      #print(input$lookup1CsvFile$name)
      #print(paste0(app_sys("./extdata"),"/",input$lookup1CsvFile$name))
      file.copy(input$lookup1CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup1CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup1CsvFile")
    }) %>% bindEvent(input$lookup1CsvFile)

    observe({
      req(r$config)
      r$config["lookup1HelpFile"] <- paste0("./extdata","/",input$lookup1HelpFile$name)
      #print("Lookup1HelpFile changed")
      #print(input$lookup1HelpFile$datapath)
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
      r$config["lookup2CsvFile"] <- paste0("./extdata","/",input$lookup2CsvFile$name)
      #print("Lookup2CsvFile changed")
      #print(input$lookup2CsvFile$datapath)
      file.copy(input$lookup2CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup2CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup2CsvFile")
    }) %>% bindEvent(input$lookup2CsvFile)

    observe({
      req(r$config)
      r$config["lookup2HelpFile"] <- paste0("./extdata","/",input$lookup2HelpFile$name)
      #print("Lookup2HelpFile changed")
      #print(input$lookup2HelpFile$datapath)
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
      r$config["lookup3CsvFile"] <-paste0("./extdata","/",input$lookup3CsvFile$name)
      #print("Lookup3CsvFile changed")
      #print(input$lookup3CsvFile$datapath)
      file.copy(input$lookup3CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup3CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup3CsvFile")
    }) %>% bindEvent(input$lookup3CsvFile)

    observe({
      req(r$config)
      r$config["lookup3HelpFile"] <- paste0("./extdata","/",input$lookup3HelpFile$name)
      #print("Lookup3HelpFile changed")
      #print(input$lookup3HelpFile$datapath)
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
      r$config["lookup4CsvFile"] <- paste0("./extdata","/",input$lookup4CsvFile$name)
      #print("Lookup4CsvFile changed")
      #print(input$lookup4CsvFile$datapath)
      file.copy(input$lookup4CsvFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup4CsvFile$name)), overwrite = TRUE)
      save_user_config("lookup4CsvFile")
    }) %>% bindEvent(input$lookup4CsvFile)

    observe({
      req(r$config)
      r$config["lookup4HelpFile"] <- file.path(paste0(app_sys("./extdata"),"/",input$lookup4HelpFile$name))
      #print("Lookup4HelpFile changed")
      #print(input$lookup4HelpFile$datapath)
      file.copy(input$lookup4HelpFile$datapath,file.path(paste0(app_sys("./extdata"),"/",input$lookup4HelpFile$name)), overwrite = TRUE)
      save_user_config("lookup4HelpFile")
    }) %>% bindEvent(input$lookup4HelpFile)

    #export annotations button
    observe({
      req(r$user_annotations_file_name,  r$user_annotations_data)
      volumes <- c(Documents = fs::path_home(), "R Installation" = R.home(),shinyFiles::getVolumes()())

      if (is.integer(input$export_annotations)) {
        cat("No directory has been selected (shinyDirChoose)")
        shinyFiles::shinyDirChoose(input,"export_annotations",roots = volumes, session = session, restrictions = system.file(package = "base"))
      } else {
        annotations_export_dir <- shinyFiles::parseDirPath(volumes, input$export_annotations)
        annotations_export_full_path_rds <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.rds")
        annotations_export_full_path_csv <- paste0(annotations_export_dir,"/", r$user_name, "s_annotations.csv")

        save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

        if(the$config$exportFileFormat == "rds"){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user","timestamp", "leaflet_id", "imagefile", "feature_type", "radius", "geometry", paste0(the$config$lookup1Label), paste0(the$config$lookup2Label), paste0(the$config$lookup3Label), paste0(the$config$lookup4Label))
          saveRDS(temp_df, file=annotations_export_full_path_rds)
          rm(temp_df)
          #file.copy(r$user_annotations_file_name, annotations_export_full_path_rds, overwrite = TRUE)
          shinyWidgets::show_alert(
            title = "Export Successful!",
            text = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_rds )),
            html = TRUE,
            type = "success"
          )
        }
        else if (the$config$exportFileFormat == "csv"){
          temp_df <- r$user_annotations_data
          colnames(temp_df) <- c("user","timestamp", "leaflet_id", "imagefile", "feature_type", "radius", "geometry", paste0(the$config$lookup1Label), paste0(the$config$lookup2Label), paste0(the$config$lookup3Label), paste0(the$config$lookup4Label))
          utils::write.csv(temp_df, annotations_export_full_path_csv, fileEncoding = "UTF-8", row.names=FALSE)
          rm(temp_df)
          shinyWidgets::show_alert(
            title = "Export Successful!",
            text = HTML(paste0("You exported the annotations to:<br>", annotations_export_full_path_csv )),
            html = TRUE,
            type = "success"
          )
        }

      }

    }) %>% bindEvent(input$export_annotations)

    output$infoText <- renderUI({
      if(nchar(r$user_name)>0){
        if(nchar(r$current_image)>0){
          #if(length(r$active_modules())>0){
          shinyjs::enable("save_annotation")
          shinyjs::enable("export_annotations")
          shinyjs::enable("add_module")
          shinyjs::enable("remove_module")
          str1 <- paste0("<b>Annotation File:</b> ", r$user_name, "s_annotations.rds")
          str2 <- paste0("<hr><b>Image File:</b> <small>", r$current_image, "</small><hr>")
          HTML(paste(str1, str2, sep = '<br/>'))
        }
        else {
          shinyjs::disable("save_annotation")
          shinyjs::disable("export_annotations")
          shinyjs::disable("add_module")
          shinyjs::disable("remove_module")
          #}
        }
      }
    })

    #this is to add modules dynamically
    #track active module names so we can remove them
    r$active_modules <- reactiveVal(value = NULL)
    #s <- reactiveValues()

    #add annotation record button
    observe({

      req(r$current_image_metadata)
      lat <- r$current_image_metadata$Latitude
      long <- r$current_image_metadata$Longitude
      # update the list of currently shown modules
      add_annotations_form(myActiveModules=r$active_modules, myModuleNumber=paste0("id_",input$add_module), myFeatureType="image_marker", myGeometry=paste0("POINT (",long," ",lat,")"),myDD1=NA,myDD2=NA,myDD3=NA, myDD4=NA)
    }) %>% bindEvent(input$add_module)

    #this is to remove form on button click
    observe({
      req(r)
      #first remove the last annotation delete the records and then save the remaining ones
      #this is done now on remove to load the pins back in on the map and keep everything synced
      #TODO should break this out into function as the code is repeated for the 'save_module' lower in this page
      remove_last_annotation(r)
      remove_current_image_data <- delete_annotations(r$user_annotations_data, r$current_image)
      #View(remove_current_image_data)
      r$user_annotations_data <- remove_current_image_data
      #View(r$user_annotations_data)

      if(length(r$active_modules())>0){
        #then add the new ones
        for(i in 1:length(r$active_modules())){
          val1 <- paste0("print(input$",r$active_modules()[i],"_dropdown1)")
          val2 <- paste0("print(input$",r$active_modules()[i],"_dropdown2)")
          ifelse(!is.null(eval(parse(text=val2))), print("dd2 exists"), val2 <- paste0("NA"))
          val3 <- paste0("print(input$",r$active_modules()[i],"_dropdown3)")
          ifelse(!is.null(eval(parse(text=val3))), print("dd3 exists"), val3 <- paste0("NA"))
          val4 <- paste0("print(input$",r$active_modules()[i],"_dropdown4)")
          ifelse(!is.null(eval(parse(text=val4))), print("dd4 exists"), val4 <- paste0("NA"))
          val5 <- paste0("print(input$",r$active_modules()[i],"_feature_type)")
          val6 <- paste0("print(input$",r$active_modules()[i],"_geometry)")
          val7 <- paste0("print(input$",r$active_modules()[i],"_leaflet_id)")

          r$user_annotations_data <- add_annotation_data(myUserAnnotationsData = r$user_annotations_data, myUser = r$user_name, myTimeStamp = timestamp(), myImage=r$current_image, myLeafletId=eval(parse(text=val7)), myFeatureType=eval(parse(text=val5)), myGeometry=eval(parse(text=val6)), myDD1 = eval(parse(text=val1)), myDD2 = eval(parse(text=val2)), myDD3 = eval(parse(text=val3)), myDD4 = eval(parse(text=val4)))
        }
      }

      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)
      #addMapLayer(r=r)

    }) %>% bindEvent(input$remove_module)

    observe({
      #print(app_sys("/app/www/",r$user_name, "s_annotations.rds"))
      req(r$user_annotations_file_name,  r$user_annotations_data, r$current_image_metadata, r$current_image)
      #View(r$user_annotations_data)

      #first delete any annotations made for this image
      remove_current_image_data <- delete_annotations(r$user_annotations_data, r$current_image)
      #View(remove_current_image_data)
      r$user_annotations_data <- remove_current_image_data
      #View(r$user_annotations_data)

      if(length(r$active_modules())>0){
        #then add the new ones
        for(i in 1:length(r$active_modules())){
          #print(r$active_modules()[i])
          #eval(parse(text=val))
          val1 <- paste0("print(input$",r$active_modules()[i],"_dropdown1)")
          val2 <- paste0("print(input$",r$active_modules()[i],"_dropdown2)")
          ifelse(!is.null(eval(parse(text=val2))), print("dd2 exists"), val2 <- paste0("NA"))
          val3 <- paste0("print(input$",r$active_modules()[i],"_dropdown3)")
          ifelse(!is.null(eval(parse(text=val3))), print("dd3 exists"), val3 <- paste0("NA"))
          val4 <- paste0("print(input$",r$active_modules()[i],"_dropdown4)")
          ifelse(!is.null(eval(parse(text=val4))), print("dd4 exists"), val4 <- paste0("NA"))
          val5 <- paste0("print(input$",r$active_modules()[i],"_feature_type)")
          val6 <- paste0("print(input$",r$active_modules()[i],"_geometry)")
          val7 <- paste0("print(input$",r$active_modules()[i],"_leaflet_id)")

          r$user_annotations_data <- add_annotation_data(myUserAnnotationsData = r$user_annotations_data, myUser = r$user_name, myTimeStamp = timestamp(), myImage=r$current_image, myLeafletId=eval(parse(text=val7)), myFeatureType= eval(parse(text=val5)), myGeometry=eval(parse(text=val6)), myDD1 = eval(parse(text=val1)), myDD2 = eval(parse(text=val2)), myDD3 = eval(parse(text=val3)), myDD4 = eval(parse(text=val4)))
        }

        #str(r$user_annotations_data)
        #print(length(r$active_modules()))
        #list_of_inputs <<- reactiveValuesToList(r)
        #print(list_of_inputs)

        #remove_annotations_form(r$active_modules)
      }
      #View(r$user_annotations_data)
      #print(paste0("Saving annotations in: ", r$user_annotations_file_name))
      save_annotations(myAnnotations=r$user_annotations_data, myAnnotationFileName = r$user_annotations_file_name)

      shinyWidgets::show_alert(
        title = "Annotation Saved!",
        text = "Awesome, saved the annotation, select another image and annotate it.",
        type = "success"
      )

    }) %>% bindEvent(input$save_annotation)

  })

  #check if there are any annotations for a selected image already
  observe({
    #print("current image changed")
    req(r$current_image,  r$current_image_metadata)
    remove_annotations_form(r)
    r$active_modules <- reactiveVal(value = NULL)
    previous_annotations <- check_for_annotations(r$user_annotations_data, r$current_image)

    if(nrow(previous_annotations > 1)){
      #print("annotations already exist")
      for(i in 1:nrow(previous_annotations)){
        #previous_annotations[i, "dropdown1"]
        add_annotations_form(myActiveModules=r$active_modules, myModuleNumber=previous_annotations[i, "leaflet_id"], myFeatureType=previous_annotations[i, "feature_type"] , myGeometry= previous_annotations[i, "geometry"], myDD1= previous_annotations[i, "dd1"],myDD2= previous_annotations[i, "dd2"], myDD3=previous_annotations[i, "dd3"], myDD4=previous_annotations[i, "dd4"])
      }
      #tell the user annotations already exist
      shinyWidgets::show_alert(
        title = "Annotations Already Exist!",
        text = "It looks like you've already done this one :) I've loaded that data.... If you want to edit it, just make the changes & click the 'Save Record' button.",
        type = "info"
      )
    }
  })%>% bindEvent(r$current_image)

}

## To be copied in the UI
# mod_control_form_ui("control_form_1")

## To be copied in the server
# mod_control_form_server("control_form_1")

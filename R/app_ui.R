#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  runtime_context <- current_runtime_context(initialize = FALSE)
  config <- runtime_context$config

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(runtime_context = runtime_context),
    shinyjs::useShinyjs(),
    if (requireNamespace("rintrojs", quietly = TRUE)) {
      rintrojs::introjsUI()
    },
    tags$head(
      tags$style(HTML("
        .pannotator-settings-shell {
          margin-bottom: 16px;
          overflow-x: hidden;
        }
        .pannotator-settings-shell-drawer {
          margin-bottom: 0;
        }
        .pannotator-settings-shell-drawer .card {
          margin-bottom: 0;
          box-shadow: none;
        }
        .pannotator-settings-shell .card-body,
        .pannotator-settings-shell .tab-content,
        .pannotator-settings-shell .tab-pane,
        .pannotator-settings-shell .pannotator-settings-body {
          overflow-x: hidden;
          max-width: 100%;
        }
        .pannotator-settings-shell .nav-tabs {
          display: flex;
          flex-wrap: wrap;
          row-gap: 4px;
        }
        .pannotator-settings-shell .nav-tabs > li > a,
        .pannotator-settings-shell .nav-tabs .nav-link {
          white-space: normal;
        }
        .pannotator-settings-shell .row {
          margin-left: 0;
          margin-right: 0;
        }
        .pannotator-settings-shell .row > [class*='col-'] {
          padding-left: 8px;
          padding-right: 8px;
        }
        .pannotator-settings-shell .shiny-input-container,
        .pannotator-settings-shell .form-control,
        .pannotator-settings-shell .input-group,
        .pannotator-settings-shell .bootstrap-select,
        .pannotator-settings-shell .selectize-control {
          max-width: 100%;
        }
        .pannotator-settings-global {
          margin-bottom: 16px;
        }
        .pannotator-settings-global-actions {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
          flex-wrap: wrap;
          padding: 12px 14px;
          margin-bottom: 12px;
          border: 1px solid #d0d7de;
          border-radius: 10px;
          background: var(--bs-tertiary-bg, rgba(248, 249, 250, 0.8));
        }
        .pannotator-settings-global-toggle {
          flex: 1 1 280px;
          min-width: 240px;
        }
        .pannotator-settings-global-toggle .shiny-input-container {
          margin-bottom: 0;
        }
        .pannotator-settings-global-toggle .checkbox {
          margin-top: 0;
          margin-bottom: 0;
        }
        .pannotator-settings-global-buttons {
          display: flex;
          align-items: center;
          justify-content: flex-end;
          gap: 8px;
          flex-wrap: wrap;
        }
        .pannotator-settings-csv-editor {
          margin-top: 8px;
          margin-bottom: 16px;
        }
        .pannotator-settings-csv-editor .text-muted {
          display: block;
          margin-bottom: 6px;
        }
        .pannotator-drawer-shell {
          position: relative;
        }
        .pannotator-drawer-shell > .bslib-sidebar-layout {
          min-height: calc(100vh - 2.5rem);
        }
        .pannotator-drawer-shell > .bslib-sidebar-layout > .main,
        .pannotator-drawer-shell > .bslib-sidebar-layout > .sidebar {
          min-height: calc(100vh - 2.5rem);
        }
        .pannotator-drawer-shell .sidebar-content {
          height: 100%;
          overflow-y: auto;
          overflow-x: hidden;
        }
        .pannotator-drawer-shell .pannotator-settings-shell-drawer,
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .pannotator-settings-body,
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .tab-content,
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .tab-pane.active,
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .tabbable {
          height: auto;
          min-height: 0;
          overflow: visible;
        }
        .pannotator-drawer-shell .pannotator-settings-shell-drawer > .card {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        .pannotator-drawer-shell .pannotator-settings-shell-drawer > .card > .card-body,
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .pannotator-settings-body > .card-body {
          flex: 1 1 auto;
          height: 100%;
        }
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .tabbable {
          display: flex;
          flex-direction: column;
        }
        .pannotator-drawer-shell .pannotator-settings-shell-drawer .tab-content {
          flex: 1 1 auto;
        }
        .pannotator-drawer-shell .bslib-sidebar-layout > .collapse-toggle {
          position: absolute;
          top: 8px;
          z-index: 1001;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          gap: 8px;
          width: auto;
          height: auto;
          padding: 8px 10px;
          border: none;
          background: var(--bs-primary, #0d6efd);
          color: #fff;
          box-shadow: 0 8px 20px rgba(0, 0, 0, 0.18);
        }
        .pannotator-drawer-shell .bslib-sidebar-layout > .collapse-toggle::before {
          content: '\\2699';
          font-size: 16px;
          line-height: 1;
        }
        .pannotator-drawer-shell .bslib-sidebar-layout > .collapse-toggle:hover,
        .pannotator-drawer-shell .bslib-sidebar-layout > .collapse-toggle:focus {
          background: #0b5ed7;
          color: #fff;
        }
        .pannotator-drawer-shell .bslib-sidebar-layout > .collapse-toggle > .collapse-icon {
          width: 16px;
          height: 16px;
          opacity: 1;
        }
        .pannotator-drawer-shell-left .bslib-sidebar-layout > .collapse-toggle {
          border-radius: 0 10px 10px 0;
        }
        .pannotator-drawer-shell-right .bslib-sidebar-layout > .collapse-toggle {
          border-radius: 10px 0 0 10px;
        }
        .pannotator-layout-choice-grid {
          display: grid;
          gap: 12px;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          margin-bottom: 16px;
        }
        .pannotator-layout-choice {
          border: 1px solid #d0d7de;
          border-radius: 10px;
          padding: 12px;
          background: var(--bs-body-bg, #fff);
          color: var(--bs-body-color, #212529);
        }
        .pannotator-layout-choice-clickable {
          cursor: pointer;
          transition: border-color 0.15s ease, box-shadow 0.15s ease, transform 0.15s ease;
        }
        .pannotator-layout-choice-clickable:hover,
        .pannotator-layout-choice-clickable:focus {
          border-color: var(--bs-primary, #0d6efd);
          box-shadow: 0 0 0 2px rgba(13, 110, 253, 0.18);
          outline: none;
          transform: translateY(-1px);
        }
        .pannotator-layout-choice-active {
          border-color: var(--bs-primary, #0d6efd);
          box-shadow: 0 0 0 1px rgba(13, 110, 253, 0.25);
        }
        .pannotator-layout-swatch {
          min-height: 120px;
        }
        .pannotator-layout-swatch-bottom,
        .pannotator-layout-swatch-main {
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .pannotator-layout-swatch-workspace {
          display: grid;
          grid-template-columns: minmax(0, 2fr) minmax(72px, 0.9fr);
          gap: 6px;
          align-items: stretch;
        }
        .pannotator-layout-swatch-workspace-main {
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .pannotator-layout-swatch-drawer {
          display: grid;
          gap: 8px;
          align-items: stretch;
        }
        .pannotator-layout-swatch-drawer-left {
          grid-template-columns: 84px minmax(0, 3fr);
        }
        .pannotator-layout-swatch-drawer-right {
          grid-template-columns: minmax(0, 3fr) 84px;
        }
        .pannotator-layout-swatch-row {
          display: grid;
          gap: 6px;
          grid-template-columns: repeat(3, minmax(0, 1fr));
        }
        .pannotator-layout-swatch-workspace-main .pannotator-layout-swatch-row {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .pannotator-layout-swatch-row .pannotator-layout-box-wide {
          grid-column: 1 / -1;
        }
        .pannotator-layout-swatch-side {
          display: flex;
          min-width: 84px;
        }
        .pannotator-layout-box {
          display: flex;
          align-items: center;
          justify-content: center;
          min-height: 34px;
          border-radius: 8px;
          background: var(--bs-secondary-bg, #eef3f8);
          color: var(--bs-secondary-color, #4b5a6a);
          font-size: 12px;
          font-weight: 600;
          text-align: center;
          padding: 8px;
        }
        .pannotator-layout-box-accent {
          background: rgba(13, 110, 253, 0.12);
          color: var(--bs-primary, #0d6efd);
        }
        .pannotator-layout-box-tall {
          width: 100%;
          height: 100%;
          min-height: 78px;
        }
        #shiny-notification-panel {
          position: fixed;
          top: 50% !important;
          left: 50% !important;
          right: auto !important;
          bottom: auto !important;
          transform: translate(-50%, -50%);
          width: min(420px, calc(100vw - 32px));
          max-width: calc(100vw - 32px);
          z-index: 99999;
        }
        #shiny-notification-panel .shiny-notification {
          width: 100%;
          max-width: 100%;
        }
        #shiny-notification-panel .shiny-progress-notification .progress {
          margin-bottom: 8px;
        }
        #shiny-notification-panel .shiny-progress-notification {
          min-height: 72px;
        }
        #shiny-notification-panel .shiny-progress-notification .progress-text {
          display: block;
          min-height: 44px;
          white-space: normal;
        }
        #shiny-notification-panel .shiny-progress-notification .progress-message,
        #shiny-notification-panel .shiny-progress-notification .progress-detail {
          display: block;
          white-space: normal;
          line-height: 1.35;
        }
        #shiny-notification-panel .shiny-progress-notification .progress-detail {
          margin-top: 4px;
        }
        .pannotator-panel-status {
          margin-top: 10px;
          margin-bottom: 10px;
        }
        .pannotator-panel-column {
          margin-bottom: 20px;
        }
        .pannotator-workspace-layout {
          display: grid;
          gap: 20px;
          align-items: start;
          width: 100%;
          max-width: 100%;
        }
        .pannotator-workspace-main,
        .pannotator-workspace-annotation,
        .pannotator-workspace-extra-panels {
          min-width: 0;
        }
        .pannotator-workspace-main,
        .pannotator-workspace-extra-panels {
          display: flex;
          flex-direction: column;
          gap: 20px;
        }
        .pannotator-workspace-top {
          display: grid;
          gap: 20px;
          align-items: stretch;
          min-width: 0;
        }
        .pannotator-panel-slot {
          width: 100%;
          max-width: 100%;
          overflow-x: hidden;
        }
        .pannotator-panel-slot > .well {
          max-width: 100%;
          overflow-x: hidden;
        }
        .pannotator-workspace-layout .pannotator-panel-slot > .well {
          margin-bottom: 0;
        }
        .pannotator-workspace-main .pannotator-panel-slot > .well {
          height: 100%;
        }
        .pannotator-workspace-annotation .pannotator-panel-slot > .well {
          height: auto;
          margin-bottom: 0;
        }
        .pannotator-workspace-settings .pannotator-settings-shell {
          margin-bottom: 0;
        }
        .pannotator-annotation-table-shell {
          width: 100%;
          max-width: 100%;
        }
        .pannotator-annotation-table-widget {
          width: 100%;
          max-width: 100%;
          overflow-x: auto;
        }
        .pannotator-annotation-table-widget .html-widget,
        .pannotator-annotation-table-widget .rhandsontable {
          max-width: 100%;
        }
        .pannotator-annotation-table-widget .handsontable tbody tr:hover td {
          background-color: rgba(var(--bs-primary-rgb, 13, 110, 253), 0.08) !important;
        }
        .pannotator-annotation-table-widget .handsontable td.currentRow,
        .pannotator-annotation-table-widget .handsontable th.currentRow {
          background-color: rgba(var(--bs-primary-rgb, 13, 110, 253), 0.14) !important;
        }
        .pannotator-annotation-table-widget .handsontable td.currentCol,
        .pannotator-annotation-table-widget .handsontable th.currentCol {
          background-color: rgba(var(--bs-primary-rgb, 13, 110, 253), 0.06) !important;
        }
        @media (min-width: 768px) {
          .pannotator-panel-row {
            display: flex;
            flex-wrap: wrap;
            align-items: stretch;
          }
          .pannotator-panel-row > .pannotator-panel-column {
            float: none;
            display: flex;
          }
          .pannotator-panel-row > .pannotator-panel-column > .pannotator-panel-slot {
            display: flex;
            flex: 1 1 auto;
          }
          .pannotator-panel-row > .pannotator-panel-column > .pannotator-panel-slot > .well {
            display: flex;
            flex-direction: column;
            width: 100%;
            height: 100%;
          }
        }
        @media (max-width: 991.98px) {
          .pannotator-workspace-layout,
          .pannotator-workspace-top {
            display: flex;
            flex-direction: column;
          }
        }
        .pannotator-inline-notice {
          margin-top: 10px;
          margin-bottom: 10px;
        }
        .pannotator-empty-state {
          min-height: 750px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          padding: 24px;
          text-align: center;
          border: 1px dashed #b8c2cc;
          border-radius: 8px;
          background: var(--bs-tertiary-bg, rgba(248, 249, 250, 0.9));
          color: var(--bs-body-color, #46505a);
        }
        .pannotator-annotation-card {
          margin-bottom: 20px;
          border: 1px solid #ccc;
          padding: 10px;
          box-shadow: 0px 2px 2px #eee;
          border-radius: 10px;
        }
        .pannotator-card-actions {
          display: flex;
          flex-direction: row;
          flex-wrap: nowrap;
          align-items: center;
          justify-content: flex-end;
          gap: 8px;
          margin-left: auto;
        }
        .pannotator-card-actions .pannotator-card-action {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 34px;
          min-width: 34px;
          height: 34px;
          padding: 0;
          line-height: 1;
          white-space: nowrap;
          text-align: center;
        }
        .pannotator-card-actions .pannotator-card-action .action-icon {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 100%;
          height: 100%;
          margin: 0;
          line-height: 1;
        }
        .pannotator-card-actions .pannotator-card-action .action-icon i {
          margin: 0;
          line-height: 1;
        }
        .pannotator-card-actions .pannotator-card-action .action-label {
          display: none;
        }
        .pannotator-drawer-layout .main-bslib-sidebar {
          padding-left: 0;
          padding-right: 0;
        }
        .pannotator-drawer-layout-right .sidebar {
          border-left: 1px solid #d0d7de;
        }
        .pannotator-drawer-layout-left .sidebar {
          border-right: 1px solid #d0d7de;
        }
      "))
    ),
    # Your application UI logic
    fluidPage(
      theme = get_app_theme(config = config),
      br(),
      build_app_shell_ui(config = config)
    )
  )

}

settings_drawer_width <- function() {
  "920px"
}


build_app_shell_ui <- function(config = myEnv$config) {
  settings_placement <- settings_placement_value(config)
  settings_drawer_position <- settings_drawer_position_value(config)

  if (identical(settings_placement, "drawer")) {
    return(
      div(
        class = paste(
          "pannotator-drawer-shell",
          paste0("pannotator-drawer-shell-", settings_drawer_position)
        ),
        style = paste0("--pannotator-drawer-width: ", settings_drawer_width(), ";"),
        bslib::layout_sidebar(
          class = paste(
            "pannotator-drawer-layout",
            paste0("pannotator-drawer-layout-", settings_drawer_position)
          ),
          sidebar = bslib::sidebar(
            id = "app_settings_drawer",
            position = settings_drawer_position,
            open = "closed",
            width = settings_drawer_width(),
            class = "pannotator-settings-drawer",
            mod_settings_ui(
              "settings",
              config = config,
              display_mode = "drawer"
            )
          ),
          mod_panel_host_ui("panel_host"),
          fillable = FALSE
        )
      )
    )
  }

  mod_panel_host_ui("panel_host")
}

get_app_theme <- function(config = NULL) {
  if (is.null(config)) {
    config <- current_runtime_context(initialize = FALSE)$config
  }

  theme_name <- config$appTheme
  theme_mode <- normalize_app_theme_mode(config$appThemeMode)

  available_themes <- tryCatch(
    bslib::bootswatch_themes(version = 5),
    error = function(e) character()
  )

  if (is.null(theme_name) || length(theme_name) == 0 || !nzchar(theme_name[[1]])) {
    theme_name <- "cerulean"
  }
  theme_name <- as.character(theme_name[[1]])

  if (!theme_name %in% available_themes) {
    theme_name <- "cerulean"
  }

  theme_args <- list(
    version = 5,
    bootswatch = theme_name
  )

  if (identical(theme_mode, "dark")) {
    theme_args$bg <- "#111827"
    theme_args$fg <- "#f8fafc"
    theme_args$primary <- "#4dabf7"
  }

  do.call(bslib::bs_theme, theme_args)
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(runtime_context = current_runtime_context(initialize = FALSE)) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  add_resource_path(
    "temp_dir",
    tempdir() #tools::R_user_dir("pannotator")
  )

  add_resource_path(
    "app_data",
    runtime_context$data_dir
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pannotator"
    )
  )
}

#' Run the Shiny Application
#'
#' `run_app()` starts the Shiny application contained in this package, with an option to load settings from a YAML file using `projectSettingsFile = "pathToYamlFile"`.
#'
#' @details This function initializes and runs the Shiny app developed with the golem framework.
#' It optionally loads application-specific settings from a YAML file, which can be useful for project-specific configurations.
#' Project YAML files can be exported from the Settings panel inside the app.
#'
#' @param projectSettingsFile Optional path to a project YAML file to load at startup.
#' @param ... Additional arguments to pass to `golem_opts`. See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @return No return value, called for side effects. Launches the Shiny app.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#' @examples
#' if (interactive()) {
#'   # Example: Run the application with default settings
#'
#'   options(shiny.port=httpuv::randomPort(),
#'           shiny.launch.browser = .rs.invokeShinyWindowExternal,
#'           shiny.maxRequestSize=9000*1024^2)
#'
#'   run_app()
#'
#'   # Example: Run the application with a project YAML exported from Settings
#'   run_app(projectSettingsFile = "C:/test-project.yml")
#' }
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  projectSettingsFile = NULL,
  ...
) {
  runtime_context <- build_runtime_context(
    projectSettingsFile = projectSettingsFile,
    initialize = TRUE
  )
  golem_opts <- list(...)
  golem_opts$projectSettingsFile <- NULL
  golem_opts$runtime_context <- NULL
  golem_opts <- c(
    golem_opts,
    list(
      projectSettingsFile = projectSettingsFile,
      runtime_context = runtime_context
    )
  )

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = golem_opts
  )
}

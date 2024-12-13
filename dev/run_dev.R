# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort(), shiny.launch.browser = .rs.invokeShinyWindowExternal, shiny.maxRequestSize = (5000 * 1024^2))

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload(pkg = ".")

# Run the application
run_app()
#run_app(options=list(launch.browser = TRUE))

#run_app(projectSettingsFile = "C:/E/test-project.yml")


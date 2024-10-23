# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "pannotator", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "Visualisation and Annotation of 360 Degree Imagery", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "Provides a customisable R 'shiny' app for immersively visualising, mapping and annotating panospheric (360 degree) imagery. The flexible interface allows annotation of any geocoded images using up to 4 user specified dropdown menus. The app uses 'leaflet' to render maps that display the geo-locations of images and panellum <https://pannellum.org/>, a lightweight panorama viewer for the web, to render images in virtual 360 degree viewing mode. Key functions include the ability to draw on & export parts of 360 images for downstream applications. Users can also draw polygons and points on map imagery related to the panoramic images and export them for further analysis. Downstream applications include using annotations to train AI/ML models and geospatial modelling and analysis of camera based survey data.", # What the package does (one paragraph).
  authors = c(
    person(
      given = "Nunzio", # Your First Name
      family = "Knerr", # Your Last Name
      email = "Nunzio.Knerr@csiro.au", # Your email
      role = c("aut", "cre"), # Your role (here author/creator)
      comment = c(ORCID = "0000-0002-0562-9479")
    ),
    person(
      given = "Robert", # Your First Name
      family = "Godfree", # Your Last Name
      email = "Robert.Godfree@csiro.au", # Your email
      role = c("aut"), # Your role (here author/creator)
      comment = c(ORCID = "0000-0002-4263-2917")
    ),
    person(
      given = "Matthew", # Your First Name
      family = "Petroff", # Your Last Name
      email = "contact@mpetroff.net",
      role = c("ctb")
    ),
    person(given = "CSIRO",
           role = "cph")
  ),
  repo_url = "https://github.com/NunzioKnerr/pannotator.git", # The URL of the GitHub repo (optional),
  pkg_version = "0.0.1.0001", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl_license(version = 3, include_future = TRUE) # You can set another license here
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
#usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_cran_comments()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
#golem::use_utils_ui(with_test = TRUE)
#golem::use_utils_server(with_test = TRUE)

## Use git ----
#usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "package",
  url = "https://github.com/NunzioKnerr/pannotator.git",
  overwrite = TRUE
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")

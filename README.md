
<!-- README.md is generated from README.Rmd. Please edit that file -->

## pannotator Source Code

The Panospheric Image Annotator in R (pannotator) software package
provides an easy-to-use interface for visualising 360 degree camera
images on satellite imagery and annotating the images with data selected
from user-defined drop-down menus. It’s designed for use in ecological
and biogeographical research but can be used to extract data from any
spatially explicit 360 degree camera imagery.

The package was built using Golem and is web application that can run
locally on any current computer system.

See the instructions below to get everything installed an running.

This vignette provides an overview of the functionality of the package,
including setup and configuration, interface layout, image selection,
drop-down menu specification, annotation of image files, and exporting
data.

## Installation

The pannotator package makes extensive use of ExifTool by Phil Harvey
([Exiftool.org](https://exiftool.org/)). To make installation of
ExifTool accessible in R there is a package exiftoolr that you must
install by running the code below.

``` r
# First check if you have exiftoolr installed
check_for_package <- system.file(package = "exiftoolr")
print(check_for_package)

# If not run the following code
if (check_for_package == "") {
  print("exiftoolr package not found .....installing now")
  install.packages("exiftoolr")
} else {
  print("exiftoolr package is already installed")
}
```

Now that you have installed exiftoolr we can check to make sure that
ExifTool is on your system.

``` r
library(exiftoolr)  
check_for_ExifTool <- exiftoolr::exif_version(quiet = TRUE)

# Install ExifTool if not found
if (exists("check_for_ExifTool")) {
  print("ExifTool found on system")
} else {
  print("ExifTool not found ... installing now")   
  exiftoolr::install_exiftool()
}
```

You must also install the ‘remotes’ package which we will use to install
the pannotator package.

``` r
check_for_package <-  system.file(package = "remotes")

print(check_for_package)
# If not run the following code
if (check_for_package == "") {
  print("remotes package not found .....installing now")
  install.packages("remotes")
} else {
  print("remotes package is already installed")
}
```

One last package not currently on CRAN is called ‘gridlayout’. The code
below checks for that and if need be installs it using the remotes
package.

``` r
check_for_package <-  system.file(package = "gridlayout")

print(check_for_package)
# If not run the following code
if (check_for_package == "") {
  print("gridlayout package not found .....installing now")   
  remotes::install_github("rstudio/gridlayout")
} else {
  print("gridlayout package is already installed")
}
```

You can now install the development version of the pannotator software.

``` r
library(remotes)

# to install a local version use this code: 
# edit the path to point to your loacal version of the package.
remotes::install_local(path = "pannotator_0.0.0.9001.tar.gz", dependencies = TRUE) 
```

Finally, to run the application use the following code.

``` r
library(pannotator) 
run_app()
```

If you want help you can find it using the following code:

``` r
vignette('pannotator', package = 'pannotator')
```

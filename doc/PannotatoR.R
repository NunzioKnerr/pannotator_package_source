## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library(PannotatoR)
#  run_app()

## ----eval=F, echo=T-----------------------------------------------------------
#  #library(dplyr)
#  #library(mapview)
#  #library(RColorBrewer)
#  #library(sf)
#  df_annotation <- readRDS("C:/user_1_annotations.rds")
#  # read in the .rds file
#  df_annotation <- st_as_sf(df_annotation, wkt = "geometry",crs = 4326)
#  #define
#  #the geometry
#  df_annotation$dd2 <- as.numeric(df_annotation$dd2)
#  # dd2 = -999 where crowns
#  # have not been assessed for health (NA);
#  # range = 0 for no live leaves (entirely # dead) to 100 for entire crown healthy
#  df_annotation <- subset(df_annotation, dd2 > -1 )
#  # select only records where
#  # Allocasuarina crowns have been assessed for health; that is, excluding NA records
#  mapviewOptions(basemaps = c("Esri.WorldImagery"),
#                 vector.palette = colorRampPalette(c("red","orange", "yellow", "green")),
#                 layers.control.pos = "topright")   mapview(df_annotation, zcol = "dd2 ", na.rm = TRUE)


library(scales)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(sf)
library(raster)
library(tidyr)
library(geotidy)
library(tidygeocoder)
library(knitr)
library(leaflet)
library(leaflegend)
library(leaflet.extras)

options(shiny.maxRequestSize=30*1024^2)

# Define two projection strings
crs_proj <- "+proj=utm +zone=18 +datum=NAD83" # Projection for intersecting process
crs_latlng <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' # Latitudes and longitudes for visualizing in leaflet map

# Define regex pattern
rx_pattern <- "(?<=\\().+(?=\\))"

# Load all embedded datasets
source("Load_datasets.R")

# Create markers reported drinking locations
alcohol_marker <- awesomeIcons(
  icon = "beer",
  iconColor = "White",
  markerColor = "brown",
  library = "fa"
)

# Markers for reported negative emotion
neg_marker <- makeSymbolsSize(
  values = c(0,1,2,3,4,5,6,7,8,9,10),
  shape = 'circle',
  color = 'red',
  fillColor = 'red',
  opacity = .5,
  baseSize = 10
)

# Markers for reported positive emotion
pos_marker <- makeSymbolsSize(
  values = c(0,1,2,3,4,5,6,7,8,9,10),
  shape = 'circle',
  color = 'purple',
  fillColor = 'purple',
  opacity = .5,
  baseSize = 10
)

# Palette for the alcohol KDE layer
pal <- colorNumeric(c("#FFF7E0", "#FF5733", "#581845"),
                    values(alcohol_dens_ras), 
                    na.color = "transparent")
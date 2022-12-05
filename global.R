library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(sf)
library(tidyr)
library(geotidy)
library(tidygeocoder)
library(knitr)
library(leaflet)
library(leaflet.extras)

options(shiny.maxRequestSize=30*1024^2)

# Define two projection strings
crs_proj <- "+proj=utm +zone=18 +datum=NAD83" # Projection for intersecting process
crs_latlng <- '+proj=longlat +datum=WGS84' # Latitudes and longitudes for visualizing in leaflet map
# Define regex pattern
rx_pattern <- "(?<=\\().+(?=\\))"

source("load_alcohol_dataset.R")
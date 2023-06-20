library(stringr)
library(stringi)
library(scales)
library(lubridate)
library(xts)
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(tidyr)
library(geotidy)
library(tidygeocoder)
library(spatstat)
library(knitr)
library(dygraphs)
library(plotly)
library(leaflet)
library(leaflegend)
library(leaflet.extras)
library(shiny)
library(shinythemes)
library(shinycssloaders)

options(shiny.reactlog=TRUE) 
options(shiny.maxRequestSize = 50*1024^2) # Max file size to upload to the tool
options(spinner.color = "#abe6dc", spinner.color.background = "#ffffff", spinner.size = 0.5)


# Define two projection strings
crs_proj <- "+proj=utm +zone=18 +datum=NAD83" # Projection for KDE
crs_latlng <- '+proj=longlat +datum=WGS84' # Latitudes and longitudes for visualizing sf objects in the leaflet map.

#-----------------------------------------------------------------------

# Load all datasets

# Load the geocoded alcohol dataset (This dataset is created by overlaying the geocoded alcohol shp with NYS counties shp.)
alcohol_loc_by_county <- read_rds("./data/Alcohol_Locations_exclude_NYC_LongIsland_sf.rds")

# Load the KDE output raster
alcohol_dens_ras <- raster("./data/Alcohol_Kernel_Density_raster_300m_150m_EPSG3857.tif")

#-----------------------------------------------------------------------

# Create the dictionary of drinking cues
cues_dict <- c(
  "1" = "alcohol advertisements",
  "2" = "drinking mentioned in the media (television, movies, radio)",
  "3" = "glassware",
  "4" = "dartboard",
  "5" = "pool table",
  "6" = "bar",
  "7" = "liquor/wine bottles",
  "8" = "other",
  "9" = "nothing"
)

# Create markers for drinking locations
alcohol_marker <- awesomeIcons(
  icon = "beer",
  iconColor = "White",
  markerColor = "brown",
  library = "fa"
)

# Create markers for where they report cues
cue_marker <- awesomeIcons(
  icon = "lightbulb",
  iconColor = "White",
  markerColor = "purple",
  library = "fa"
)

# Palette for the alcohol KDE layer
kde_pal <- colorNumeric(c("#FFF7E0", "#FF5733", "#581845"),
                        values(alcohol_dens_ras), 
                        na.color = "transparent")

# Palette for the reported addresses layer
loc_type_pal <- colorFactor(palette = c("#5d95fc", "#c57cfc", "#fca7c5", "#fac673"),
                            level = c("home", "drink", "buy", "work"))

# Make a list of icons for negative emotions
neg_icons <- iconList(
  emo_0 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_0.png",
    iconWidth = 20, iconHeight = 20),
  emo_neg_1 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_1.png",
    iconWidth = 20, iconHeight = 20),
  emo_neg_2 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_2.png",
    iconWidth = 20, iconHeight = 20),
  emo_neg_3 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_3.png",
    iconWidth = 20, iconHeight = 20),
  emo_neg_4 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_4.png",
    iconWidth = 20, iconHeight = 20),
  emo_neg_5 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_5.png",
    iconWidth = 20, iconHeight = 20)
)

# The html for the legend of the negative emotions
neg_html_legend <- "<b>Negative Emotion</b><br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_0.png' style='width:20px;height:20px;'>&nbsp0<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_1.png' style='width:20px;height:20px;'>&nbsp1-2<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_2.png' style='width:20px;height:20px;'>&nbsp3-4<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_3.png' style='width:20px;height:20px;'>&nbsp5-6<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_4.png' style='width:20px;height:20px;'>&nbsp7-8<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_neg_5.png' style='width:20px;height:20px;'>&nbsp9-10"

# Make a list of icons for positive emotions
pos_icons <- iconList(
  emo_0 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_0.png",
    iconWidth = 20, iconHeight = 20),
  emo_pos_1 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_1.png",
    iconWidth = 20, iconHeight = 20),
  emo_pos_2 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_2.png",
    iconWidth = 20, iconHeight = 20),
  emo_pos_3 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_3.png",
    iconWidth = 20, iconHeight = 20),
  emo_pos_4 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_4.png",
    iconWidth = 20, iconHeight = 20),
  emo_pos_5 = makeIcon(
    iconUrl = "https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_5.png",
    iconWidth = 20, iconHeight = 20)
)

# The html for the legend of the positive emotions
pos_html_legend <- "<b>Positive Emotion</b><br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_0.png' style='width:20px;height:20px;'>&nbsp0<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_1.png' style='width:20px;height:20px;'>&nbsp1-2<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_2.png' style='width:20px;height:20px;'>&nbsp3-4<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_3.png' style='width:20px;height:20px;'>&nbsp5-6<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_4.png' style='width:20px;height:20px;'>&nbsp7-8<br/>
<img src='https://raw.githubusercontent.com/weiliu-gis/SPACES_Dashboard/main/emo_icons/emo_pos_5.png' style='width:20px;height:20px;'>&nbsp9-10"
# Load the geocoded alcohol dataset
alcohol_outlets_loc <- read.csv("./data/NY_Alcohol_Outlets_Locations.csv", header=TRUE)
# load the New York State shape file
nys_shp <- st_read("./data/NYS_Boundary_shp/State.shp")
# Convert to an sf object
alcohol_outlets_loc <- st_as_sf(alcohol_outlets_loc, coords = c("long", "lat"), crs = crs_latlng)
# Re-project
nys_shp <- st_transform(nys_shp, crs = crs_latlng)
# Find points within polygon
alcohol_outlets_loc_within <- st_intersection(alcohol_outlets_loc, nys_shp)
# Project the data for using them with the leaflet package
alcohol_outlets_loc_within <- st_transform(alcohol_outlets_loc_within, crs = crs_latlng)
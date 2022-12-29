# Load the New York State shape file
nys_shp <- st_read("./data/NYS_Boundary_shp/State.shp")
nys_shp <- st_transform(nys_shp, crs = crs_latlng)

# Load the NYS counties shp
nys_counties_shp <- st_read("./data/NYS_Counties_shp/counties.shp")
nys_counties_shp <- st_transform(nys_counties_shp, crs = crs_latlng)

# Load the geocoded alcohol dataset
alcohol_loc <- read.csv("./data/NYS_Alcohol_Outlets_Locations.csv", header=TRUE)
alcohol_loc <- st_as_sf(alcohol_loc, coords = c("long", "lat"), crs = crs_latlng)
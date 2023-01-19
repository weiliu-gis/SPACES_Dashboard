# Load the New York State shape file
nys_shp <- st_read("./data/NYS_State_shp/State_Shoreline.shp")
nys_shp <- st_transform(nys_shp, crs = crs_latlng)

# Load the NYS counties shp
nys_counties_shp <- st_read("./data/NYS_Counties_shp/Counties_Shoreline.shp")
nys_counties_shp <- st_transform(nys_counties_shp, crs = crs_latlng)

# Load the geocoded alcohol dataset (This dataset is created by overlaying the geocoded alcohol shp with NYS counties shp.)
alcohol_loc_by_county <- read_rds("./data/Alcohol_Locations_By_County_sf.rds")

# Load the KDE output raster
alcohol_dens_ras <- readRDS(file = "./data/Alcohol_Kernel_Density_raster_250m_50m.rds")

library(tidygeocoder)
library(dplyr)
library(sf)
library(leaflet)

# Load alcohol outlet dataset (Got this dataset from)
alcohol_outlets_df = read.csv("Liquor_Authority_Daily_List_of_Active_Licenses_API.csv", header=TRUE) # Alcohol Outlets

# Gain complete addresses
for (i in 1:nrow(alcohol_outlets_df)) {
  alcohol_outlets_df$complete_address[i] <- paste0(alcohol_outlets_df$Premise.Address[i], ", ", alcohol_outlets_df$Premise.Address.2[i], ", ",alcohol_outlets_df$Premise.City[i], ", New York")
}

# Geocoding through combined methods: Google Geocoding API, ArcGIS Single Address Geocoding API, and Nominatim API (This step took 2 hrs.)
alcohol_outlets_loc_combining <- alcohol_outlets_df %>%
  geocode_combine(
    queries = list(list(method = "google"),
                   list(method = "arcgis"),
                   list(method = "osm")
    ),
    global_params = list(address = "complete_address"),
    cascade = TRUE
  )

# Save the results to a new .csv file
write.csv(alcohol_outlets_loc_combining,"Liquor_Authority_coordinates_combine.csv", row.names = FALSE)

# Load the geocoded dataset
alcohol_outlets_loc_combining = read.csv("Liquor_Authority_coordinates_combine.csv", header=TRUE)

# Check whether all addresses have been coded
sum(is.na(alcohol_outlets_loc_combining$long))

# Convert data frames to objects with geographic information
crs_latlng <- '+proj=longlat +datum=WGS84'
alcohol_outlets_loc_combining <- st_as_sf(alcohol_outlets_loc_combining, coords = c("long", "lat"), crs = crs_latlng)

# Mapping
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    data = alcohol_outlets_loc_combining,
    radius = 3,
    fillColor = "alcohol_outlets_loc_combining",
    fillOpacity = 1,
    stroke = FALSE,
    popup = paste0("Name: ", alcohol_outlets_loc_combining$Premise.Name,
                   "<br>Type: ", alcohol_outlets_loc_combining$Method.of.Operation,
                   "<br>Address: ", alcohol_outlets_loc_combining$complete_address),
    popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
  )
map

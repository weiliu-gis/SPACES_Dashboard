function(input, output, session){
  
  # Data Explorer
  # ----------------------------------------------
  
  # Display GPS data frame
  output$gps_table <- renderTable({
    req(input$gps_csv)
    df <- read.csv(input$gps_csv$datapath)
    if(input$display == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })
  
  # Display Baseline data frame
  output$base_table <- renderTable({
    req(input$base_csv)
    df <- read.csv(input$base_csv$datapath)
    if(input$display == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })
  
  # Display EMA data frame
  output$ema_table <- renderTable({
    req(input$ema_csv)
    df <- read.csv(input$ema_csv$datapath)
    if(input$display == "head"){
      return(head(df))
    }
    else{
      return(df)
    }
  })
  
  # Processing data
  # ----------------------------------------------
  
  # GPS data
  #
  gps_df <- reactive({
    req(input$gps_csv)
    df <- read.csv(input$gps_csv$datapath)
    # Filter out those records with accuracy values larger than 1000
    df <- filter(df, accuracy < 1000) %>%
      # Parse to POSIX
      mutate(time = ymd_hms(time)) %>%
      # Sort the data frame by time (ascending)
      arrange(time)
    return(df)
  })
  # Convert data frames to objects with geographic information
  gps_point <- reactive({
    st_as_sf(gps_df(), coords = c("longitude", "latitude"), crs = crs_latlng)
  })
  # Convert GPS points into a continued polyline (One line-string for one person)
  gps_polyline <- reactive({
    summarize(gps_point(), geometry = st_makeline(geometry), uid = first(uid))
  })
  
  # Baseline Survey
  #
  # Reported drinking locations
  drink_location <- reactive({
    req(input$base_csv)
    df <- read.csv(input$base_csv$datapath)
    # Retrieve and format all drinking locations
    base_drink_loc1 <- df$pDrinkQ1aLoc1
    base_drink_loc2 <- df$pDrinkQ1aLoc2
    base_drink_loc3 <- df$pDrinkQ1aLoc3
    base_drink_loc <- data.frame(address = c(base_drink_loc1, base_drink_loc2, base_drink_loc3))
    for (i in 1:nrow(base_drink_loc)) {
      if (is.na(base_drink_loc$address[i]) != TRUE & nchar(base_drink_loc$address[i]) > 0) {
        base_drink_loc$address[i] <- str_extract(base_drink_loc$address[i], rx_pattern)
      }
      else {
        base_drink_loc$address[i] <- NA
      }
    }
    # Drop NAs from the data frame
    base_drink_loc <- drop_na(base_drink_loc)
    # Obtain coordinates by ArcGIS Single Address geocoding (We are using this because it is free, but, yes, it could be inaccurate.)
    base_drink_loc <- tidygeocoder::geocode(base_drink_loc,
                                            address = address,
                                            method = "arcgis")
    # Convert the data frame to an sf object
    base_drink_loc <- st_as_sf(base_drink_loc, coords = c("long", "lat"), crs = crs_latlng)
    return(base_drink_loc)
  })
  
  # EMA survey
  #
  # Emotion
  ema_emotion <- reactive({
    req(input$ema_csv)
    df <- read.csv(input$ema_csv$datapath)
    # Clean the EMA data
    for (i in 1:nrow(df)) {
      # Format the start time of the positive/negative emotion
      df$start_time[i] <- as.character(ymd_hms(paste0(substr(df$startTime[i], 1, 11), df$expQ7[i], ":00")))
      # Format the end time of the positive/negative emotion
      if (df$expQ8[i] == "No") { # The person is not in the emotion at the moment, use the reported end time as the end time.
        df$end_time[i] <- as.character(ymd_hms(paste0(substr(df$startTime[i], 1, 11), df$expQ8a[i], ":00")))
      }
      else { # The person is still in the emotion at the moment, use the start time of the survey as the end time
        df$end_time[i] <- df$startTime[i]
      }
      # Whether the assessment is self-intitiated? 1 as "Yes"; 0 as "No (random prompt)"
      if (df$expQ1[i] == "a") {
        df$self[i] <- 1
      }
      else {
        df$self[i] <- 0
      }
    }
    df <- mutate(df,
                 emo_unpleasant = expQ2,
                 emo_pleasant = expQ3)
    # Parse time columns to POSIX
    df$start_time = ymd_hms(df$start_time)
    df$end_time = ymd_hms(df$end_time)
    # Join GPS records to EMA data set by matching the time stamps to each of the time periods.
    df_point <- gps_df() %>%
      left_join(df, by = "uid") %>%
      mutate(inRange = (time >= start_time-5*60 & time <= end_time+5*60)) %>% # Allow a 5-min deviation
      filter(inRange) %>%
      dplyr::select(c("uid", "time", "self", "emo_unpleasant", "emo_pleasant", "longitude", "latitude"))
    # Convert the data frame to an sf object
    df_point <- st_as_sf(df_point, coords = c("longitude", "latitude"), crs = crs_latlng)
    return(df_point)
  })
  
  # Map View
  # ----------------------------------------------
  
  # Map 1: emotion & urge
  
  output$map1 <- renderLeaflet({
    
    leaflet() %>%
      
      # Base groups
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      
      # Overlay groups
      ## GPS points
      addCircleMarkers(
        data = gps_point(),
        group = "GPS Points",
        radius = 3,
        fillColor = "darkblue",
        fillOpacity = 1,
        stroke = FALSE,
        popup = paste0("Date & Time: ", gps_df()$time),
        popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
      ) %>%
      ## GPS traces
      addPolylines(
        data = gps_polyline(),
        group = "GPS Polylines",
        opacity = 0.5,
        color = "darkblue",
        weight = 3) %>%
      ## Drinking locations
      addAwesomeMarkers(
        data = drink_location(),
        group = "Drinking Locations",
        icon = alcohol_marker,
        popup = drink_location()$address
      ) %>%
      ## Negative Emotion
      addCircleMarkers(
        data = ema_emotion(),
        group = "Negative Emotion",
        radius = ~rescale(emo_unpleasant, c(0,15)),
        fillOpacity = 0.6,
        stroke = FALSE,
        color = "blue",
        popup = paste0("Being unpleasant: ", ema_emotion()$emo_unpleasant)
      ) %>%
      addLegendSize(
        group = "Negative Emotion",
        values = c(0,1,2,3,4,5,6,7,8,9,10),
        color = 'blue',
        fillColor = 'blue',
        opacity = .5,
        title = 'Being Unpleasant',
        shape = 'circle',
        orientation = 'vertical',
        position = 'bottomright',
        breaks = 10
      ) %>%
      ## Positive Emotion
      addCircleMarkers(
        data = ema_emotion(),
        group = "Positive Emotion",
        radius = ~rescale(emo_pleasant, c(5,13)),
        fillOpacity = 0.6,
        stroke = FALSE,
        color = "purple",
      ) %>%
      
      # Layer control
      addLayersControl(
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Negative Emotion", "Positive Emotion"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("GPS Points", "Positive Emotion"))
  })
  
  # Map 2: environment
  
  output$map2 <- renderLeaflet({
    
    leaflet() %>%
      
      # Base groups
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      
      # Overlay groups
      ## Geocoded NYS alcohol outlets
      addCircleMarkers(
        data = alcohol_loc_by_county,
        group = "Alcohol Outlets",
        radius = 3,
        fillColor = "alcohol_loc_by_county",
        fillOpacity = 1,
        stroke = FALSE,
        popup = paste0("Name: ", alcohol_loc_by_county$Premise.Name,
                       "<br>Type: ", alcohol_loc_by_county$Method.of.Operation,
                       "<br>Address: ", alcohol_loc_by_county$complete_address),
        popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
      ) %>%
      ## KDE
      addRasterImage(alcohol_dens_ras,
                     group = "Alcohol Kernel Density",
                     colors = pal,
                     opacity = 0.6
      ) %>%
      addLegend(group = "Alcohol Kernel Density",
                pal = pal,
                values = values(alcohol_dens_ras),
                title = "Alcohol Kernel Density (/sq.km)"
      ) %>%
      
      # Set map view to initial extent
      addResetMapButton() %>%
      
      # Layer control
      addLayersControl(
        position = c("topleft"),
        overlayGroups = c("Alcohol Outlets", "Alcohol Kernel Density"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Alcohol Outlets"))
  })
}
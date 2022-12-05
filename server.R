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
  # Convert GPS points into continued polylines (One line-string for one person)
  gps_polyline <- reactive({
    summarize(gps_point(), geometry = st_makeline(geometry), uid = first(uid))
  })
  
  # Map View
  # ----------------------------------------------
  
  # Map 1: emotion
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
      
      # Layer control
      addLayersControl(
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("GPS Points"))
  })
  
  # Map 2: environment
  output$map2 <- renderLeaflet({
    
    leaflet() %>%
      
      # Base groups
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      
      # Overlay groups
      ## Geocoded NYS alcohol outlets
      addCircleMarkers(
        data = alcohol_outlets_loc_within,
        group = "Alcohol Outlets",
        radius = 3,
        fillColor = "alcohol_outlets_loc_within",
        fillOpacity = 1,
        stroke = FALSE,
        popup = paste0("Name: ", alcohol_outlets_loc_within$Premise.Name,
                       "<br>Type: ", alcohol_outlets_loc_within$Method.of.Operation,
                       "<br>Address: ", alcohol_outlets_loc_within$complete_address),
        popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
        )%>%
      ## Heatmap
      addHeatmap(
        data = alcohol_outlets_loc_within,
        group = "Heatmap",
        blur = 10,
        max = 0.05,
        radius = 10
      ) %>%
      
      # Layer control
      addLayersControl(
        overlayGroups = c("Alcohol Outlets", "Heatmap"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Alcohol Outlets"))
  })
}
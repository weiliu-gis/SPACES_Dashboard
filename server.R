function(input, output, session) {
  
  # 1 Data Explorer
  # ============================================
  # This part is for reading the selected data files and choosing which participant to analyze by inputting their UID.
  
  # 1.1 Display GPS dataframe
  # --------------------------------------------
  
  gps_df <- reactive({
    req(input$gps_csv, input$uid)
    df <- read.csv(input$gps_csv$datapath)
    return(df %>% filter(uid == input$uid))
  })
  
  output$gps_table <- renderTable({
    req(gps_df())
    df <- gps_df()
    if (input$display == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })
  
  #  1.2 Display Baseline data frame
  # --------------------------------------------
  
  # Get input
  base_df <- reactive({
    req(input$base_csv, input$uid)
    df <- read.csv(input$base_csv$datapath)
    return(df %>% filter(uid == input$uid))
  })
  
  # Render table
  output$base_table <- renderTable({
    req(base_df())
    df <- base_df()
    if (input$display == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })
  
  # 1.3 Display EMA data frame
  # --------------------------------------------
  
  # Get input
  ema_df <- reactive({
    req(input$ema_csv, input$uid)
    df <- read.csv(input$ema_csv$datapath)
    return(df %>% filter(uid == input$uid))
  })
  
  # Render table
  output$ema_table <- renderTable({
    req(ema_df())
    df <- ema_df()
    if (input$display == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })
  
  # 1.4 Display Addresses data frame
  # --------------------------------------------
  
  # Get input
  addr_df <- reactive({
    req(input$addr_csv)
    df <- read.csv(input$addr_csv$datapath)
    return(df %>% filter(uid == input$uid))
  })
  
  # Render table
  output$addr_table <- renderTable({
    req(addr_df())
    df <- addr_df()
    return(df)
  })
  
  
  # 2 Process data
  # ============================================
  # This part is for processing all data to extract information including
  # drinking locations, drinking cues, drinking urge, emotions...
  
  # Preprocess GPS, Daily Baseline and EMA data
  data_processed <- reactive({
    req(gps_df(), base_df(), ema_df())
    result_list <- preprocessData(gps_df(), base_df(), ema_df())
    return(result_list)
  })
  
  # Identify drinking urge clusters
  urge_cluster <- reactive({
    result_list <- findUrgeCluster(data_processed()$urge)
    return(result_list)
  })
  
  # 2.4 Self-Reported Addresses
  # ------------------------------------------------------------------
  
  addr_geocoded_df <- reactive({
    df <- tidygeocoder::geocode(addr_df(), address = address, method = "arcgis")
    return(df)
  })
  
  # 3 Visualization
  # ------------------------------------------------------------------
  
  # 3.1 Map - traces, emotion & urge
  # ------------------------------------------------------------------
  
  # 3.1.1 Graph for temporal view of emotion status
  
  # Create line graph
  output$dygraph <- renderDygraph({
    dygraph(data_processed()$emotion_xts) %>%
      dyAxis("y", valueRange = c(-10, 11)) %>%
      dySeries('positive', fillGraph = TRUE, label = "Positive", drawPoints = FALSE, pointSize = 0, color = "rgba(127, 206, 89, 1)") %>% # Green
      dySeries('negative', fillGraph = TRUE, label = "Negative", drawPoints = FALSE, pointSize = 0, color = "rgba(65, 213, 255, 1)") %>% # Blue
      dySeries('rolling_average', label = "Rolling Average", drawPoints = TRUE, pointSize = 1.5, strokeWidth = 1.5, color = 'rgba(62, 8, 165, 1)') %>% # Purple
      dyLegend(width = 500) %>%
      dyRangeSelector(height = 30,
                      fillColor = "lightgray",
                      strokeColor = "") %>%
      # Ribbon
      dyRibbon(data = data_processed()$ribbon_data, top = 1, bottom = 0,
               palette = c("#FFFFFF", "#F6CECE"))
  })
  
  # 3.1.2 Text outputs
  
  # Extract the from/to time values from the date window on the range selector
  from_time <- reactive({as.POSIXct(input$dygraph_date_window[1], format = "%Y-%m-%dT%H:%M:%OSZ")})
  to_time <- reactive({as.POSIXct(input$dygraph_date_window[2], format = "%Y-%m-%dT%H:%M:%OSZ")})
  
  # Print the from/to time values
  output$from <- renderText({as.character(from_time())})
  output$to <- renderText({as.character(to_time())})
  
  # 3.1.3 Map for spatial view
  
  gps_sf_filtered <- reactive({
    dplyr::filter(data_processed()$gps_sf,
                  time >= from_time() & time <= to_time())
  })
  # Convert GPS points into a continued polyline (One line-string for one person)
  gps_sf_line_filtered <- reactive({
    summarize(gps_sf_filtered(),
              geometry = st_makeline(geometry),
              uid = first(uid))
  })
  
  emotion_filtered <- reactive({
    dplyr::filter(data_processed()$ema_emotion_sf,
                  time >= from_time() & time <= to_time())
  })
  
  drinking_filtered <- reactive({
    dplyr::filter(data_processed()$drinking_time_sf,
                  time >= from_time() & time <= to_time())
  })
  
  cue_filtered <- reactive({
    data_processed()$cue_sf %>%
      dplyr::filter(time >= from_time() & time <= to_time()) %>%
      dplyr::filter(cue_exist == "Yes")
  })
  
  # Create the map view
  output$map <- renderLeaflet({
    
    leaflet() %>%
      
      # Base groups
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      
      # Overlay groups
      ## GPS points
      addCircleMarkers(
        data = gps_sf_filtered(),
        group = "GPS Points",
        radius = 3,
        fillColor = "black",
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = paste0("Date & Time: ", gps_sf_filtered()$time),
        popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
      ) %>%
      ## GPS traces
      addPolylines(
        data = gps_sf_line_filtered(),
        group = "GPS Polylines",
        opacity = 0.5,
        color = "black",
        weight = 3
      ) %>%
      ## Urge to Drink
      addPolygons(
        data = urge_cluster()$urge_buffer_sf,
        group = "Drinking Urge",
        color = ~urge_cat_pal(cat_knn),
        weight = 1,
        opacity = 0.3,
        fillOpacity = 0.05,
        fillColor = ~urge_cat_pal(cat_knn)
      ) %>%
      addMarkers(
        data = urge_cluster()$urge_spot_sf,
        group = "Drinking Urge",
        icon = ~urge_icons[cat_knn],
        label = paste0("Gi*: ", urge_cluster()$urge_spot_sf$gi_star_knn, "  Urge: ", urge_cluster()$urge_spot_sf$urge_alcohol)
      ) %>%
      ## Drinking locations
      addAwesomeMarkers(
        data = drinking_filtered(),
        group = "Drinking Locations",
        icon = alcohol_marker,
      ) %>%
      ## Cues
      addAwesomeMarkers(
        data = cue_filtered(),
        group = "Drinking Cues",
        icon = cue_marker,
        label = paste0(cue_filtered()$cue_char),
        popup = paste0("Date & Time: ", cue_filtered()$time,
                       "<br>", "Drinking cues: ", cue_filtered()$cue_char)
      ) %>%
      ## Negative Emotion
      addMarkers(
        data = emotion_filtered(),
        group = "Negative Emotion",
        icon = ~neg_icons[neg_icon],
        label = paste0(abs(emotion_filtered()$emo_unpleasant)),
        popup = paste0("Date & Time: ", emotion_filtered()$time,
                       "<br>", "Being unpleasant: ", abs(emotion_filtered()$emo_unpleasant))
      ) %>%
      ## Positive Emotion
      addMarkers(
        data = emotion_filtered(),
        group = "Positive Emotion",
        icon = ~pos_icons[pos_icon],
        label = paste0(emotion_filtered()$emo_pleasant),
        popup = paste0("Date & Time: ", emotion_filtered()$time,
                       "<br>", "Being pleasant: ", emotion_filtered()$emo_pleasant)
      ) %>%
      ## Reported Addresses
      addCircleMarkers(
        data = addr_geocoded_df(),
        group = "Reported Addresses",
        lat = ~lat,
        lng = ~long,
        color = ~loc_type_pal(locationType),
        radius = 12,
        stroke = TRUE,
        fillOpacity = 0.5,
        label = paste0(addr_geocoded_df()$locationType),
        popup = paste0("Location type: ", addr_geocoded_df()$locationType,
                       "<br>", "Description: ", addr_geocoded_df()$description,
                       "<br>", "Address: ", addr_geocoded_df()$address)
      ) %>%
      
      # Set map view to initial extent
      addResetMapButton() %>%
      
      # Layer control
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Urge", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Drinking Urge", "Reported Addresses", "Drinking Cues", "Drinking Urge"))
  })
  
  # This observer is responsible for showing and hiding legends for emotion, urge, address and density layers
  observe({
    proxy <- leafletProxy("map") %>% clearControls()
    if (any(input$map_groups %in% "Negative Emotion")) {
      proxy <- proxy %>%
        addControl(
          html = neg_html_legend, 
          position = "bottomright"
        )
    }
    if (any(input$map_groups %in% "Positive Emotion")) {
      proxy <- proxy %>%
        addControl(
          html = pos_html_legend, 
          position = "bottomright"
        )
    }
    if (any(input$map_groups %in% "Drinking Urge")) {
      proxy <- proxy %>%
        addControl(
          html = urge_html_legend, 
          position = "bottomright"
        )
    }
    if (any(input$map_groups %in% "Reported Addresses")) {
      proxy <- proxy %>%
        addLegendFactor(
          data = addr_geocoded_df(),
          group = "Reported Addresses",
          shape = 'circle',
          pal = loc_type_pal,
          values = addr_geocoded_df()$locationType,
          title = "Location Type",
          opacity = 0.7,
          position = 'bottomright'
        )
    }
  })
  
  # This observer is responsible for showing and hiding alcohol density legend
  observe({
    proxy <- leafletProxy("map") %>% removeControl(layerId = 'density_legend')
    if (any(input$map_groups %in% "Alcohol Kernel Density")) {
      proxy <- proxy %>%
        addLegend(
          layerId = 'density_legend',
          group = "Alcohol Kernel Density",
          pal = kde_pal,
          values = values(alcohol_dens_ras),
          title = "Alcohol Kernel Density",
          position = 'bottomleft'
        )
    }
  })
  
  # This observer is responsible for showing the alcohol exposure raster layer
  observeEvent(input$show, {
    leafletProxy("map") %>%
      addRasterImage(
        group = "Alcohol Kernel Density",
        alcohol_dens_ras,
        colors = kde_pal,
        opacity = 0.6,
        project = FALSE
      ) %>%
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Urge", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses", "Alcohol Kernel Density"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Drinking Urge", "Alcohol Outlets", "Reported Addresses", "Drinking Cues"))
  })
  
  # This observer is responsible for hiding the alcohol exposure
  observeEvent(input$hide, {
    leafletProxy("map") %>%
      clearGroup(c("Alcohol Kernel Density")) %>%
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Urge", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Reported Addresses", "Drinking Cues", "Drinking Urge"))
  })
  
  # 3.2 Timeline - emotion, urge, regulation trying
  # ------------------------------------------------------------------
  
  output$plotly <- renderPlotly({
    plot_ly(source = "plotly") %>%
      
      # Drinking time
      add_trace(data = data_processed()$drinking_time_graph,
                x = ~time, y = ~fake_value,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(255, 86, 70, 0)'),
                showlegend = FALSE,
                hoverinfo = 'none') %>%
      add_trace(data = data_processed()$drinking_time_graph, x = ~time, y = ~fake_value*(-1),
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(255, 86, 70, 0)'),
                fill = 'tonexty',
                fillcolor='rgba(255, 86, 70, 0.3)',
                showlegend = FALSE,
                hoverinfo = 'none') %>%
      
      # Regulation trying
      add_trace(data = data_processed()$reg_graph,
                x = ~time, y = ~reg_try,
                type = 'scatter', mode = 'lines',
                line = list(color = "#6E6E6E"), # Grey
                name = "Regulation Trying") %>%
      
      # Urge to drink
      add_trace(data = data_processed()$urge,
                x = ~time, y = ~urge_alcohol,
                type = 'scatter', mode = 'lines',
                line = list(color = "#FDAC14"), # Orange
                name = "Drinking Urge") %>%
      
      # Emotion
      add_trace(data = data_processed()$ema_emotion_graph,
                x = ~time, y = ~emo_rolling_average,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(62, 8, 165, 1)"), # Purple
                name = "Average Emotion") %>%
      add_trace(data = data_processed()$ema_emotion_graph,
                x = ~time, y = ~emo_pleasant,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(127, 206, 89, 1)"), # Green
                name = "Positive Emotion") %>%
      add_trace(data = data_processed()$ema_emotion_graph,
                x = ~time, y = ~emo_unpleasant,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(65, 213, 255, 1)"), # Blue
                name = "Negative Emotion") %>%
      
      # Range slider and range selector
      layout(xaxis = list(rangeslider = list(visible = T, type = "date", thickness = 0.2),
                          rangeselector = list(
                            buttons = list(
                              list(count = 6, label = "6h", step = "hour", stepmode = "backward"),
                              list(count = 1, label = "1d", step = "day", stepmode = "backward"),
                              list(count = 7, label = "1w", step = "day", stepmode = "todate"),
                              list(step="all"))
                          )
      )
      ) %>%
      
      # Other layout settings
      layout(xaxis = list(title = NA,
                          zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = '#ffff'),
             yaxis = list(title = NA,
                          zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = '#ffff'),
             plot_bgcolor='#f2f2f2')
  })
  
  
}
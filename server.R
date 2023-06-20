# This file has been modified on 02/21/2023

function(input, output, session) {
  # 1 Data Explorer
  # ------------------------------------------------------------------
  
  # 1.1 Display GPS data frame
  # ------------------------------------------------------------------
  
  # Get input
  gps_df <- reactive({
    req(input$gps_csv)
    df <- read.csv(input$gps_csv$datapath)
    return(df)
  })
  
  # Render table
  output$gps_table <- renderTable({
    req(gps_df())
    df <- gps_df()
    if (input$display == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })
  
  
  # 1.2 Display Baseline data frame
  # ------------------------------------------------------------------
  
  # Get input
  base_df <- reactive({
    req(input$base_csv)
    df <- read.csv(input$base_csv$datapath)
    return(df)
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
  # ------------------------------------------------------------------
  
  # Get input
  ema_df <- reactive({
    req(input$ema_csv)
    df <- read.csv(input$ema_csv$datapath)
    return(df)
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
  # ------------------------------------------------------------------
  
  # Get input
  addr_df <- reactive({
    req(input$addr_csv)
    df <- read.csv(input$addr_csv$datapath)
    return(df)
  })
  
  # Render table
  output$addr_table <- renderTable({
    req(addr_df())
    df <- addr_df()
    return(df)
  })
  
  # 2 Process data
  # ------------------------------------------------------------------
  
  data_processed <- reactive({
    
    req(gps_df())
    req(base_df())
    req(ema_df())
    
    # 2.1 GPS Traces
    # ------------------------------------------------------------------
    
    gps_df_processed <- gps_df()
    
    # Filter out those records with accuracy values larger than 1000
    gps_df_processed <-
      filter(gps_df_processed, accuracy < 1000) %>%
      # Parse to POSIX
      mutate(time = ymd_hms(time)) %>%
      # Sort the data frame by time (ascending)
      arrange(time) %>%
      dplyr::filter(!duplicated(time))
    
    gps_point <- st_as_sf(gps_df_processed,
                          coords = c("longitude", "latitude"), crs = crs_latlng)
    # GPS polylines will be created in the visualization part
    
    
    # 2.2 Drinking Outcomes
    # ------------------------------------------------------------------
    
    # 2.2.1 Drinking duration
    
    # (1) EMA (Are you drinking or about to drink right now?)

    ema_df <- ema_df()
    
    for (i in 1:nrow(ema_df)) {
      if (ema_df$drinkingQ3[i] == "Yes") {
        ema_df$start_time[i] <-
          as.character(ymd_hms(paste0(
            substr(ema_df$startTime[i], 1, 11),
            ema_df$drinkingQ3a[i],
            ":00")))
        ema_df$end_time[i] <- ema_df$startTime[i]
      }
      else {
        ema_df$start_time[i] <- NA
        ema_df$end_time[i] <- NA
      }
    }
    drinking_time_ema <- ema_df %>%
      dplyr::select(start_time, end_time)
    
    # (2) Baseline (Are you drinking or about to drink right now?)
    
    base_df_1 <- base_df()
    
    for (i in 1:nrow(base_df_1)) {
      if (base_df_1$outQ3[i] == "Yes") {
        base_df_1$start_time[i] <-
          as.character(ymd_hms(paste0(
            substr(base_df_1$startTime[i], 1, 11),
            base_df_1$outQ3a[i],
            ":00")))
        base_df_1$end_time[i] <- base_df_1$startTime[i]
      }
      else {
        base_df_1$start_time[i] <- NA
        base_df_1$end_time[i] <- NA
      }
    }
    drinking_time_base <- base_df_1 %>%
      dplyr::select(start_time, end_time)
    
    # (3) Baseline (Did you drink yesterday?)
    base_df_2 <- base_df()
    
    for (i in 1:nrow(base_df_2)) {
      if (base_df_2$pDrinkQ1[i] == "Yes") {
        t_start <- hm(base_df_2$pDrinkQ2dStart[i])
        t_stop <- hm(base_df_2$pDrinkQ2dStop[i])
        # Start time
        if (am(t_start)) {
          base_df_2$start_time[i] <-
            as.character(ymd_hms(paste0(
              substr(base_df_2$startTime[i], 1, 11),
              base_df_2$pDrinkQ2dStart[i],
              ":00"
            )))
        } else {
          base_df_2$start_time[i] <-
            as.character(ymd_hms(paste0(
              substr(base_df_2$startTime[i], 1, 11),
              base_df_2$pDrinkQ2dStart[i],
              ":00"
            )) - hours(24))
        }
        # Stop time
        if (am(t_stop)) {
          base_df_2$end_time[i] <-
            as.character(ymd_hms(paste0(
              substr(base_df_2$startTime[i], 1, 11),
              base_df_2$pDrinkQ2dStop[i],
              ":00"
            )))
        } else {
          base_df_2$end_time[i] <-
            as.character(ymd_hms(paste0(
              substr(base_df_2$startTime[i], 1, 11),
              base_df_2$pDrinkQ2dStop[i],
              ":00"
            )) - hours(24))
        }
      } else {
        base_df_2$start_time[i] <- NA
        base_df_2$end_time[i] <- NA
      }
    }
    drinking_time_yesterday <- base_df_2 %>%
      dplyr::select(start_time, end_time)
    
    # Combine the times
    drinking_time <- rbind(drinking_time_ema, drinking_time_base, drinking_time_yesterday)
    drinking_time$uid <- gps_df_processed$uid[1]
    drinking_time <- drop_na(drinking_time) %>%
      arrange(start_time)
    drinking_time$start_time <- ymd_hms(drinking_time$start_time)
    drinking_time$end_time <- ymd_hms(drinking_time$end_time)
    
    # For map view - Only preserve GPS points where drinking outcomes are reported
    drinking_time_point <- gps_df_processed %>%
      left_join(drinking_time, by = "uid") %>%
      mutate(inRange = (time >= start_time - 5 * 60 & time <= end_time + 5 * 60)) %>%
      filter(inRange) %>%
      dplyr::select(uid, time, longitude, latitude) %>%
      mutate(fake_value = 10)
    drinking_time_point <- st_as_sf(drinking_time_point, coords = c("longitude", "latitude"), crs = crs_latlng)
    
    # For line graph - Preserve all GPS points, fake_value being set as 10 where drinking outcomes are reported
    drinking_time_dgraphs <- gps_df_processed %>%
      left_join(drinking_time_point, by = "time") %>%
      dplyr::select(time, fake_value) %>%
      mutate_at(c('fake_value'), ~  replace_na(., 0)) %>%
      dplyr::filter(!duplicated(time)) %>%
      mutate(time = as.POSIXct(time))
    
    # 2.2.2 Drinking Urge
    
    gps_df_1 <- gps_df_processed
    
    # (1) Baseline (On a scale of 1-10, how strong is your urge to drink right now?)
    urge_base <- base_df()[c("uid", "startTime", "outQ1")]
    colnames(urge_base) <- c("uid", "startTime", "urge_alcohol")
    
    # (2) EMA (On a scale of 1-10, how strong is your urge to drink right now?)
    urge_ema <- ema_df()[c("uid", "startTime", "drinkingQ1")]
    colnames(urge_ema) <- c("uid", "startTime", "urge_alcohol")
    
    # (3) Combine
    urge_combined <- rbind(urge_base, urge_ema)
    urge_combined$startTime = ymd_hms(urge_combined$startTime)
    urge_combined <- urge_combined[order(urge_combined$startTime),]
    
    # Add the scores of urge to drink from EMA to the gps_df data frame by using the nearest time
    gps_df_1$time_diff <- NA
    gps_df_1$urge_alcohol <- NA
    for (i in 1:nrow(urge_combined)) {
      for (j in 1:nrow(gps_df_1)) {
        gps_df_1$time_diff[j] <- abs(as.numeric(urge_combined$startTime[i]) - as.numeric(gps_df_1$time[j]))
        if (j >= 2) {
          # Find the smallest time difference
          if (gps_df_1$time_diff[j] > gps_df_1$time_diff[j-1]) {
            gps_df_1$urge_alcohol[j-1] <- urge_combined$urge_alcohol[i]
            break
          }
        }
      }
    }
    
    urge <- gps_df_1 %>% 
      dplyr::select(uid, time, longitude, latitude, urge_alcohol) %>%
      drop_na(urge_alcohol) %>%
      distinct(time, .keep_all = TRUE)
    
    # Convert the data frame to an sf object
    urge_point <- st_as_sf(urge, coords = c("longitude", "latitude"), crs = crs_latlng)
    
    # 2.2.3 Drinking Cues

    gps_df_processed_2 <- gps_df_processed
    
    cue_ema <- ema_df()[c("uid", "startTime", "drinkingQ6", "drinkingQ6a")]
    
    cue_list <- list()
    for (i in 1:nrow(cue_ema)) {
      
      this_cue_list <- list()
      # Multiple cues are separated by semicolons
      this_cue_code_list <- strsplit((cue_ema$drinkingQ6[i]),split = ';')[[1]]
      
      # Whether there is cue (whether they select "9")
      if (length(this_cue_code_list) == 1 && this_cue_code_list[1] == "9") {
        cue_ema$cue_exist[i] <- "No"
      } else {
        cue_ema$cue_exist[i] <- "Yes"
      }
      
      # Convert cue codes into readable texts
      for (cue_code in this_cue_code_list) {
        # If they select "other", also provide the description of the drinking cue
        if (cue_code == "8") {
          cue <- paste0("other (", cue_ema$drinkingQ6a[i], ")")
          # Use the dictionary to decode the drinking cues
        } else {
          cue <- as.character(cues_dict[cue_code])
        }
        this_cue_list[[length(this_cue_list) + 1]] <- cue
      }
      cue_list[[length(cue_list) + 1]] <- this_cue_list
    }
    
    # Append the list back to the drinking cue data frame
    cue_ema$cue_list <- cue_list
    
    # Save the cues as strings in another column
    cue_ema$cue_char <- NA
    for (i in 1:nrow(cue_ema)) {
      cue_ema$cue_char[i] <- stri_paste(unlist(cue_ema$cue_list[i]), collapse='; ')
    }
    
    cue_ema$startTime = ymd_hms(cue_ema$startTime)
    cue_ema <- cue_ema[order(cue_ema$startTime),]
    
    # Assign the scores of urge to drink from EMA to the nearest GPS ponits in time
    gps_df_processed_2$time_diff <- NA
    gps_df_processed_2$cue_exist <- NA
    gps_df_processed_2$cue_char <- NA
    for (i in 1:nrow(cue_ema)) {
      for (j in 1:nrow(gps_df_processed_2)) {
        # Calculate the time difference
        gps_df_processed_2$time_diff[j] <- abs(as.numeric(cue_ema$startTime[i]) - as.numeric(gps_df_processed_2$time[j]))
        if (j >= 2) {
          # Find the smallest time difference
          if (gps_df_processed_2$time_diff[j] > gps_df_processed_2$time_diff[j-1]) {
            gps_df_processed_2$cue_exist[j-1] <- cue_ema$cue_exist[i]
            gps_df_processed_2$cue_char[j-1] <- cue_ema$cue_char[i]
            break
          }
        }
      }
    }
    
    cue <- gps_df_processed_2 %>% 
      dplyr::select(uid, time, longitude, latitude, cue_exist, cue_char) %>%
      drop_na(cue_exist) %>%
      distinct(time, .keep_all = TRUE)
    
    # Convert the data frame to an sf object
    cue_point <- st_as_sf(cue, coords = c("longitude", "latitude"), crs = crs_latlng)
    
    # 2.3 Emotions
    # ------------------------------------------------------------------
    
    ema_df <- ema_df()
    # Clean the EMA data
    for (i in 1:nrow(ema_df)) {
      # Format the start time of the positive/negative emotion
      ema_df$start_time[i] <- as.character(ymd_hms(paste0(substr(ema_df$startTime[i], 1, 11), ema_df$expQ7[i], ":00")))
      # Format the end time of the positive/negative emotion
      if (ema_df$expQ8[i] == "No") { # The person is not in the emotion at the moment, use the reported end time as the end time.
        ema_df$end_time[i] <- as.character(ymd_hms(paste0(substr(ema_df$startTime[i], 1, 11), ema_df$expQ8a[i], ":00")))
      }
      else { # The person is still in the emotion at the moment, use the start time of the survey as the end time
        ema_df$end_time[i] <- ema_df$startTime[i]
      }
      # Whether the assessment is self-intitiated? 1 as "Yes"; 0 as "No (random prompt)"
      if (ema_df$expQ1[i] == "a") {
        ema_df$self[i] <- 1
      }
      else {
        ema_df$self[i] <- 0
      }
    }
    ema_emotion <- mutate(ema_df,
                          emo_unpleasant = -expQ2,
                          emo_pleasant = expQ3,
                          emo_average = expQ3 - expQ2) %>%
      arrange(start_time)
    # Parse time columns to POSIX
    ema_emotion$start_time = ymd_hms(ema_emotion$start_time)
    ema_emotion$end_time = ymd_hms(ema_emotion$end_time)
    
    # For map view - Only preserve GPS points where emotion events are reported
    ema_emotion_point <- gps_df_processed %>%
      left_join(ema_emotion, by = "uid") %>% # Join GPS records to EMA data set by matching the time stamps to each of the time periods.
      mutate(inRange = (time >= start_time - 5 * 60 & time <= end_time + 5 * 60)) %>% # Allow a 5-min deviation
      filter(inRange) %>%
      dplyr::select(uid, time, self, emo_unpleasant, emo_pleasant, emo_average, longitude, latitude)
    
    # Prepare the emotion factor column
    ema_emotion_point <- ema_emotion_point %>% 
      mutate(pos_icon = case_when((emo_pleasant == 0) ~ 'emo_0',
                                  (emo_pleasant > 0 & emo_pleasant <= 2) ~ 'emo_pos_1',
                                  (emo_pleasant > 2 & emo_pleasant <= 4) ~ 'emo_pos_2',
                                  (emo_pleasant > 4 & emo_pleasant <= 6) ~ 'emo_pos_3',
                                  (emo_pleasant > 6 & emo_pleasant <= 8) ~ 'emo_pos_4',
                                  (emo_pleasant > 8 & emo_pleasant <= 10) ~ 'emo_pos_5')) %>%
      mutate(neg_icon = case_when((emo_unpleasant == 0) ~ 'emo_0',
                                  (-emo_unpleasant > 0 & -emo_unpleasant <= 2) ~ 'emo_neg_1',
                                  (-emo_unpleasant > 2 & -emo_unpleasant <= 4) ~ 'emo_neg_2',
                                  (-emo_unpleasant > 4 & -emo_unpleasant <= 6) ~ 'emo_neg_3',
                                  (-emo_unpleasant > 6 & -emo_unpleasant <= 8) ~ 'emo_neg_4',
                                  (-emo_unpleasant > 8 & -emo_unpleasant <= 10) ~ 'emo_neg_5'))
    ema_emotion_point$pos_icon <- as.factor(ema_emotion_point$pos_icon)
    ema_emotion_point$neg_icon <- as.factor(ema_emotion_point$neg_icon)
    
    # Convert the data frame to an sf object
    ema_emotion_point <- st_as_sf(ema_emotion_point, coords = c("longitude", "latitude"), crs = crs_latlng)
    
    # For line graph - Preserve all GPS points, NAs are replaced by zeros
    ema_emotion_dgraphs <- gps_df_processed %>%
      left_join(ema_emotion_point, by = "time") %>%
      mutate_at(c('emo_unpleasant','emo_pleasant', 'emo_average'), ~replace_na(.,0)) %>%
      # Rolling average with the window size of 7 and central time stamp selected
      mutate(emo_rolling_average = rollmean(emo_average, k=7, fill=NA, align='center')) %>%
      dplyr::select(time, emo_unpleasant, emo_pleasant, emo_rolling_average) %>%
      distinct(time, .keep_all = TRUE)
    # Convert into xts format
    positive <- xts(x = ema_emotion_dgraphs$emo_pleasant, order.by = ema_emotion_dgraphs$time)
    negative <- xts(x = ema_emotion_dgraphs$emo_unpleasant, order.by = ema_emotion_dgraphs$time)
    rolling_average <- xts(x = ema_emotion_dgraphs$emo_rolling_average, order.by = ema_emotion_dgraphs$time)
    emotion_xts <- cbind(positive, negative, rolling_average)
    
    # Create ribbon data for the line graph
    drinking_or_not <- drinking_time_dgraphs$fake_value
    drinking <- which(drinking_or_not == 10)
    ribbon_data <- rep(0, nrow(emotion_xts))
    ribbon_data[drinking] <- 1
    
    
    # Processed data are returned
    # ------------------------------------------------------------------
    
    return(list(
      # GPS Traces
      gps_df_processed = gps_df_processed,
      gps_point = gps_point,
      # Drinking Outcomes
      drinking_time_point = drinking_time_point,
      drinking_time_dgraphs = drinking_time_dgraphs,
      # Drinking urge
      urge_point = urge_point,
      urge_dgraphs = urge,
      # Drinking cue
      cue_point = cue_point,
      cue_dgraphs = cue,
      # Emotion events
      ema_emotion_point = ema_emotion_point,
      ema_emotion_dgraphs = ema_emotion_dgraphs,
      emotion_xts = emotion_xts,
      ribbon_data = ribbon_data))
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
  
  gps_point_filtered <- reactive({
    dplyr::filter(data_processed()$gps_point,
                  time >= from_time() & time <= to_time())
  })
  # Convert GPS points into a continued polyline (One line-string for one person)
  gps_line_filtered <- reactive({
    summarize(gps_point_filtered(),
              geometry = st_makeline(geometry),
              uid = first(uid))
  })
  
  emotion_filtered <- reactive({
    dplyr::filter(data_processed()$ema_emotion_point,
                  time >= from_time() & time <= to_time())
  })
  
  drinking_filtered <- reactive({
    dplyr::filter(data_processed()$drinking_time_point,
                  time >= from_time() & time <= to_time())
  })
  
  cue_filtered <- reactive({
    data_processed()$cue_point %>%
      dplyr::filter(time >= from_time() & time <= to_time()) %>%
      dplyr::filter(cue_exist == "Yes")
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      
      # Base groups
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      
      # Overlay groups
      ## GPS points
      addCircleMarkers(
        data = gps_point_filtered(),
        group = "GPS Points",
        radius = 3,
        fillColor = "black",
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = paste0("Date & Time: ", gps_point_filtered()$time),
        popupOptions = popupOptions(closeButton=FALSE, closeOnClick=TRUE)
      ) %>%
      ## GPS traces
      addPolylines(
        data = gps_line_filtered(),
        group = "GPS Polylines",
        opacity = 0.5,
        color = "black",
        weight = 3) %>%
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
                       "<br>", "Address: ", addr_geocoded_df()$address)) %>%
      
      # Set map view to initial extent
      addResetMapButton() %>%
      
      # Layer control
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Reported Addresses", "Drinking Cues"))
  })
  
  # This observer is responsible for showing and hiding  the emotion legend
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
    if (any(input$map_groups %in% "Reported Addresses")) {
      proxy <- proxy %>%
        addLegendFactor(
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
  
  # This observer is responsible for showing the alcohol cue
  observeEvent(input$show, {
    leafletProxy("map") %>%
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
      addRasterImage(
        group = "Alcohol Kernel Density",
        alcohol_dens_ras,
        colors = kde_pal,
        opacity = 0.6,
        project = FALSE
      ) %>%
      addLegend(
        group = "Alcohol Kernel Density",
        layerId = 'density_legend',
        pal = kde_pal,
        values = values(alcohol_dens_ras),
        title = "Alcohol Kernel Density",
        position = 'bottomright'
      ) %>%
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses", "Alcohol Kernel Density", "Alcohol Outlets"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Alcohol Outlets", "Reported Addresses", "Drinking Cues"))
  })
  
  # This observer is responsible for hiding the alcohol cue
  observeEvent(input$hide, {
    leafletProxy("map") %>%
      clearGroup(c("Alcohol Kernel Density", "Alcohol Outlets")) %>%
      removeControl(layerId = 'density_legend') %>%
      addLayersControl(
        position = c("topleft"),
        baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"),
        overlayGroups = c("GPS Points", "GPS Polylines", "Drinking Locations", "Drinking Cues", "Negative Emotion", "Positive Emotion", "Reported Addresses"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("GPS Polylines", "Negative Emotion", "Positive Emotion", "Reported Addresses", "Drinking Cues"))
  })
  
  # 3.1 Timeline - emotion & urge...
  # ------------------------------------------------------------------
  
  # Original timeline
  output$plotly <- renderPlotly({
    plot_ly(source = "plotly") %>%
      
      # Drinking time
      add_trace(data = data_processed()$drinking_time_dgraphs,
                x = ~time, y = ~fake_value,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(255, 86, 70, 0)'),
                showlegend = FALSE,
                hoverinfo = 'none') %>%
      add_trace(data = data_processed()$drinking_time_dgraphs, x = ~time, y = ~fake_value*(-1),
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(255, 86, 70, 0)'),
                fill = 'tonexty',
                fillcolor='rgba(255, 86, 70, 0.3)',
                showlegend = FALSE,
                hoverinfo = 'none') %>%
      
      # Urge to drink
      add_trace(data = data_processed()$urge_dgraphs,
                x = ~time, y = ~urge_alcohol,
                type = 'scatter', mode = 'lines',
                line = list(color = "#FDAC14"), # Orange
                name = "Drinking Urge") %>%
      
      # Emotion
      add_trace(data = data_processed()$ema_emotion_dgraphs,
                x = ~time, y = ~emo_rolling_average,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(62, 8, 165, 1)"), # Purple
                name = "Average Emotion") %>%
      add_trace(data = data_processed()$ema_emotion_dgraphs,
                x = ~time, y = ~emo_pleasant,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(127, 206, 89, 1)"), # Green
                name = "Positive Emotion") %>%
      add_trace(data = data_processed()$ema_emotion_dgraphs,
                x = ~time, y = ~emo_unpleasant,
                type = 'scatter', mode = 'lines',
                line = list(color = "rgba(65, 213, 255, 1)"), # Blue
                name = "Negative Emotion") %>%
      
      # Range slider and range selector
      layout(xaxis = list(rangeslider = list(visible = T, type = "date", thickness = 0.35),
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
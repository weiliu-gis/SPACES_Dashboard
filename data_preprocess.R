preprocessData <- function(gps_df, base_df, ema_df){
  
  # 2.1 GPS Traces
  # --------------------------------------------
  
  gps_df_processed <-
    # Filter out those records with accuracy values larger than 1000
    filter(gps_df, accuracy < 500) %>%
    # Parse to POSIX
    mutate(time = ymd_hms(time, tz = "EST")) %>%
    # Sort the data frame by time (ascending)
    arrange(time) %>%
    dplyr::filter(!duplicated(time))
  gps_sf <- st_as_sf(gps_df_processed, coords = c("longitude", "latitude"), crs = crs_latlng)
  
  # 2.2 Drinking outcomes
  # --------------------------------------------
  
  # 2.2.1 Drinking duration
  
  # (1) EMA (Are you drinking or about to drink right now?)
  
  ema_df_1 <- ema_df
  
  for (i in 1:nrow(ema_df_1)) {
    if (ema_df_1$drinkingQ3[i] == "Yes") {
      ema_df_1$start_time[i] <-
        as.character(
          ymd_hms(
            paste0(substr(ema_df_1$startTime[i], 1, 11), ema_df_1$drinkingQ3a[i], ":00"),
            tz = "EST"))
      ema_df_1$end_time[i] <- ema_df_1$startTime[i]
    }
    else {
      ema_df_1$start_time[i] <- NA
      ema_df_1$end_time[i] <- NA
    }
  }
  drinking_time_ema <- ema_df_1 %>%
    dplyr::select(start_time, end_time)
  
  # (2) Baseline (Are you drinking or about to drink right now?)
  
  base_df_1 <- base_df
  
  for (i in 1:nrow(base_df_1)) {
    if (base_df_1$outQ3[i] == "Yes") {
      base_df_1$start_time[i] <-
        as.character(
          ymd_hms(
            paste0(substr(base_df_1$startTime[i], 1, 11), base_df_1$outQ3a[i],":00"),
            tz = "EST"))
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
  
  base_df_2 <- base_df
  
  for (i in 1:nrow(base_df_2)) {
    if (base_df_2$pDrinkQ1[i] == "Yes") {
      t_start <- hm(base_df_2$pDrinkQ2dStart[i])
      t_stop <- hm(base_df_2$pDrinkQ2dStop[i])
      # Start time
      if (am(t_start)) {
        base_df_2$start_time[i] <-
          as.character(
            ymd_hms(
              paste0(substr(base_df_2$startTime[i], 1, 11), base_df_2$pDrinkQ2dStart[i], ":00"),
              tz = "EST"))
      } else {
        base_df_2$start_time[i] <-
          as.character(
            ymd_hms(
              paste0(substr(base_df_2$startTime[i], 1, 11), base_df_2$pDrinkQ2dStart[i], ":00"),
              tz = "EST") - hours(24))
      }
      # Stop time
      if (am(t_stop)) {
        base_df_2$end_time[i] <-
          as.character(
            ymd_hms(
              paste0(substr(base_df_2$startTime[i], 1, 11), base_df_2$pDrinkQ2dStop[i], ":00"),
              tz = "EST"))
      } else {
        base_df_2$end_time[i] <-
          as.character(
            ymd_hms(
              paste0(substr(base_df_2$startTime[i], 1, 11), base_df_2$pDrinkQ2dStop[i], ":00"),
              tz = "EST") - hours(24))
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
  drinking_time$start_time <- ymd_hms(drinking_time$start_time, tz = "EST")
  drinking_time$end_time <- ymd_hms(drinking_time$end_time, tz = "EST")
  
  # For map view - Only preserve GPS points where drinking outcomes are reported
  drinking_time_temp <- gps_df_processed %>%
    left_join(drinking_time, by = "uid") %>%
    mutate(inRange = (time >= start_time - 5 * 60 & time <= end_time + 5 * 60)) %>%
    filter(inRange) %>%
    dplyr::select(uid, time, longitude, latitude) %>%
    mutate(fake_value = 10)
  drinking_time_sf <- st_as_sf(drinking_time_temp, coords = c("longitude", "latitude"), crs = crs_latlng)
  
  # For line graph - Preserve all GPS points, fake_value being set as 10 where drinking outcomes are reported
  drinking_time_graph <- gps_df_processed %>%
    left_join(drinking_time_temp, by = "time") %>%
    dplyr::select(time, fake_value) %>%
    mutate_at(c('fake_value'), ~  replace_na(., 0)) %>%
    dplyr::filter(!duplicated(time)) %>%
    mutate(time = as.POSIXct(time))
  
  # 2.2.2 Drinking Urge
  
  gps_df_processed_1 <- gps_df_processed
  
  # (1) Baseline (On a scale of 1-10, how strong is your urge to drink right now?)
  urge_base <- base_df[c("uid", "startTime", "outQ1")]
  colnames(urge_base) <- c("uid", "startTime", "urge_alcohol")
  
  # (2) EMA (On a scale of 1-10, how strong is your urge to drink right now?)
  urge_ema <- ema_df[c("uid", "startTime", "drinkingQ1")]
  colnames(urge_ema) <- c("uid", "startTime", "urge_alcohol")
  
  # (3) Combine
  urge_combined <- rbind(urge_base, urge_ema)
  urge_combined$startTime = ymd_hms(urge_combined$startTime, tz = "EST")
  urge_combined <- urge_combined[order(urge_combined$startTime),]
  
  # Add the scores of urge to drink from EMA to the gps_df data frame by using the nearest time
  gps_df_processed_1$time_diff <- NA
  gps_df_processed_1$urge_alcohol <- NA
  for (i in 1:nrow(urge_combined)) {
    for (j in 1:nrow(gps_df_processed_1)) {
      gps_df_processed_1$time_diff[j] <- abs(as.numeric(urge_combined$startTime[i]) - as.numeric(gps_df_processed_1$time[j]))
      if (j >= 2) {
        # Find the smallest time difference
        if (gps_df_processed_1$time_diff[j] > gps_df_processed_1$time_diff[j-1]) {
          gps_df_processed_1$urge_alcohol[j-1] <- urge_combined$urge_alcohol[i]
          break
        }
      }
    }
  }
  
  urge <- gps_df_processed_1 %>% 
    dplyr::select(uid, time, longitude, latitude, urge_alcohol) %>%
    drop_na(urge_alcohol) %>%
    distinct(time, .keep_all = TRUE) %>%
    arrange(time)
  

  # 2.2.3 Drinking Cues
  
  gps_df_processed_2 <- gps_df_processed
  
  cue_ema <- ema_df[c("uid", "startTime", "drinkingQ6", "drinkingQ6a")]
  
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
  
  cue_ema$startTime = ymd_hms(cue_ema$startTime, tz = "EST")
  cue_ema <- cue_ema[order(cue_ema$startTime),]
  
  # Assign the reported cues to the nearest GPS points in time
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
  cue_sf <- st_as_sf(cue, coords = c("longitude", "latitude"), crs = crs_latlng)
  
  # 2.3 Emotions
  # ------------------------------------------------------------------
  
  ema_df_2 <- ema_df
  
  # Clean the EMA data
  for (i in 1:nrow(ema_df_2)) {
    # Format the start time of the positive/negative emotion
    ema_df_2$start_time[i] <- as.character(ymd_hms(paste0(substr(ema_df_2$startTime[i], 1, 11), ema_df_2$expQ7[i], ":00"), tz = "EST"))
    # Format the end time of the positive/negative emotion
    if (ema_df_2$expQ8[i] == "No") { # The person is not in the emotion at the moment, use the reported end time as the end time.
      ema_df_2$end_time[i] <- as.character(ymd_hms(paste0(substr(ema_df_2$startTime[i], 1, 11), ema_df_2$expQ8a[i], ":00"), tz = "EST"))
    }
    else { # The person is still in the emotion at the moment, use the start time of the survey as the end time
      ema_df_2$end_time[i] <- ema_df_2$startTime[i]
    }
    # Whether the assessment is self-intitiated? 1 as "Yes"; 0 as "No (random prompt)"
    if (ema_df_2$expQ1[i] == "a") {
      ema_df_2$self[i] <- 1
    }
    else {
      ema_df_2$self[i] <- 0
    }
  }
  ema_emotion <- mutate(ema_df_2,
                        emo_unpleasant = -expQ2,
                        emo_pleasant = expQ3,
                        emo_average = expQ3 - expQ2) %>%
    arrange(start_time)
  # Parse time columns to POSIX
  ema_emotion$start_time = ymd_hms(ema_emotion$start_time, tz = "EST")
  ema_emotion$end_time = ymd_hms(ema_emotion$end_time, tz = "EST")
  
  # For map view - Only preserve GPS points where emotion events are reported
  ema_emotion_temp <- gps_df_processed %>%
    left_join(ema_emotion, by = "uid") %>% # Join GPS records to EMA data set by matching the time stamps to each of the time periods.
    mutate(inRange = (time >= start_time - 5 * 60 & time <= end_time + 5 * 60)) %>% # Allow a 5-min deviation
    filter(inRange) %>%
    dplyr::select(uid, time, self, emo_unpleasant, emo_pleasant, emo_average, longitude, latitude)
  
  # Prepare the emotion factor column
  ema_emotion_temp <- ema_emotion_temp %>% 
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
  ema_emotion_temp$pos_icon <- as.factor(ema_emotion_temp$pos_icon)
  ema_emotion_temp$neg_icon <- as.factor(ema_emotion_temp$neg_icon)
  
  # Convert the data frame to an sf object
  ema_emotion_sf <- st_as_sf(ema_emotion_temp, coords = c("longitude", "latitude"), crs = crs_latlng)
  
  # For line graph - Preserve all GPS points, NAs are replaced by zeros
  ema_emotion_graph <- gps_df_processed %>%
    left_join(ema_emotion_temp, by = "time") %>%
    mutate_at(c('emo_unpleasant','emo_pleasant', 'emo_average'), ~replace_na(.,0)) %>%
    # Rolling average with the window size of 7 and central time stamp selected
    mutate(emo_rolling_average = rollmean(emo_average, k=7, fill=NA, align='center')) %>%
    dplyr::select(time, emo_unpleasant, emo_pleasant, emo_rolling_average) %>%
    distinct(time, .keep_all = TRUE)
  # Convert into xts format
  positive <- xts(x = ema_emotion_graph$emo_pleasant, order.by = ema_emotion_graph$time)
  negative <- xts(x = ema_emotion_graph$emo_unpleasant, order.by = ema_emotion_graph$time)
  rolling_average <- xts(x = ema_emotion_graph$emo_rolling_average, order.by = ema_emotion_graph$time)
  emotion_xts <- cbind(positive, negative, rolling_average)
  
  # 2.4 Emotion Regulation Trying
  # ------------------------------------------------------------------
  
  reg_ema <- ema_df[c("uid", "startTime", "regQ18")]
  colnames(reg_ema) <- c("uid", "startTime", "reg_try")
  
  gps_df_processed_3 <- gps_df_processed
  
  reg_ema$startTime = ymd_hms(reg_ema$startTime, tz = "EST")
  reg_ema <- reg_ema[order(reg_ema$startTime),]
  
  # Assign the scores of regulation trying from EMA to the nearest GPS points in time
  gps_df_processed_3$time_diff <- NA
  gps_df_processed_3$reg_try <- NA
  for (i in 1:nrow(reg_ema)) {
    for (j in 1:nrow(gps_df_processed_3)) {
      # Calculate the time difference
      gps_df_processed_3$time_diff[j] <- abs(as.numeric(reg_ema$startTime[i]) - as.numeric(gps_df_processed_3$time[j]))
      if (j >= 2) {
        # Find the smallest time difference
        if (gps_df_processed_3$time_diff[j] > gps_df_processed_3$time_diff[j-1]) {
          gps_df_processed_3$reg_try[j-1] <- reg_ema$reg_try[i]
          break
        }
      }
    }
  }
  
  reg <- gps_df_processed_3 %>% 
    dplyr::select(uid, time, longitude, latitude, reg_try) %>%
    drop_na(reg_try) %>%
    distinct(time, .keep_all = TRUE)
  
  # For timeline
  reg_graph <- reg
  
  
  # Create ribbon data for the line graph
  drinking_or_not <- drinking_time_graph$fake_value
  drinking <- which(drinking_or_not == 10)
  ribbon_data <- rep(0, nrow(emotion_xts))
  ribbon_data[drinking] <- 1
  
  # Processed data are returned
  # ------------------------------------------------------------------
  
  return(list(
    # GPS Traces
    gps_df_processed = gps_df_processed,
    gps_sf = gps_sf,
    # Drinking Outcomes
    drinking_time_sf = drinking_time_sf,
    drinking_time_graph = drinking_time_graph,
    # Drinking urge
    urge = urge,
    # Drinking cue
    cue_sf = cue_sf,
    # Emotion events
    ema_emotion_sf = ema_emotion_sf,
    ema_emotion_graph = ema_emotion_graph,
    emotion_xts = emotion_xts,
    ribbon_data = ribbon_data,
    # Regulation trying
    reg_graph = reg_graph))
}
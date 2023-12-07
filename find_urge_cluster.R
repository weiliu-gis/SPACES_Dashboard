# Function to calculate Gi*
# --------------------------------------------------------

getgistar <- function(obs_values, sp_wt_mat){
  
  # obs_values: <vector> values of all observations stored in a vector
  # sp_wt_mat: <matrix> spatial weight matrix
  # Return the values of Gi* in a vector
  
  n_obs <- length(obs_values)
  mean <- mean(obs_values)
  sd <- sd(obs_values)
  
  gi_star <- c()
  for (i in 1:n_obs) {
    tmp_A <- 0
    for (j in 1:n_obs) {
      tmp_A <- tmp_A + sp_wt_mat[i,j]*obs_values[j]
    }
    tmp_B <- mean*sum(sp_wt_mat[i,])
    tmp_C <- sd*sqrt((n_obs*sum(sp_wt_mat[i,]*sp_wt_mat[i,])-sum(sp_wt_mat[i,])^2)/(n_obs-1))
    gi_star[i] <- (tmp_A-tmp_B)/tmp_C
  }
  
  return(gi_star)
}



# Function to identify clusters
# --------------------------------------------------------

findUrgeCluster <- function(urge){
  
  urge_sf <- st_as_sf(urge, coords = c("longitude", "latitude"), crs = crs_latlng)
  urge_sf_proj <- st_transform(urge_sf, crs=crs_proj)
  urge_proj <- sf::st_coordinates(urge_sf_proj) %>%
    cbind(urge_sf_proj) %>%
    st_drop_geometry()
  
  # Get neighbor list (5 nearest neighbors)
  urge_coord <- urge_proj %>% select(X, Y)
  dist_matrix <- dist(urge_coord, method = "euclidean", diag = TRUE, upper = TRUE) %>% as.matrix()
  nrow_urge <- nrow(urge_coord)
  
  k <- 5
  nb_list <- matrix(nrow=nrow_urge, ncol=k)
  nb_dist_list <- matrix(nrow=nrow_urge, ncol=k)
  for (i in 1:nrow_urge) {
    this_nb_list <- sort(dist_matrix[i,], index.return = TRUE)$ix[1:k+1]
    nb_list[i,] <- this_nb_list
    this_nb_dist_list <- sort(dist_matrix[i,], index.return = FALSE)[1:k+1]
    nb_dist_list[i,] <- this_nb_dist_list
  }
  
  # Get spatial weight matrix
  sp_wt_mat_knn <- matrix(0, nrow=nrow_urge, ncol=nrow_urge)
  for (i in 1:nrow_urge) {
    for (j in 1:k) {
      sp_wt_mat_knn[i, nb_list[i,j]] <- 1
    }
  }
  # Consider itself as a neighbor
  diag(sp_wt_mat_knn) <- 1
  
  # Get Gi*
  urge_proj$gi_star_knn <- getgistar(urge_proj$urge_alcohol, sp_wt_mat_knn)
  
  # Add radius
  urge_proj$r <- nb_dist_list[,5]
  
  # Identify hot/cold spot(s)
  urge_spot_proj <- urge_proj %>%
    mutate(cat_knn = case_when((gi_star_knn >= 2) ~ "hot_spot",
                               (gi_star_knn <= -2) ~ "cold_spot",
                               .default = "not_interested"))
  
  # For showing on the map
  urge_spot_sf_proj <- st_as_sf(urge_spot_proj, coords = c("X","Y"), crs = crs_proj)
  
  urge_spot_buffer_sf_proj <- st_buffer(urge_spot_sf_proj, urge_spot_sf_proj$r)
  temp_hot_buffer <- urge_spot_buffer_sf_proj %>%
    filter(cat_knn == "hot_spot") %>%
    sf::st_union() %>%
    st_sf() %>%
    mutate(cat_knn = "hot_spot")
  temp_cold_buffer <- urge_spot_buffer_sf_proj %>%
    filter(cat_knn == "cold_spot") %>%
    sf::st_union() %>%
    st_sf() %>%
    mutate(cat_knn = "cold_spot")
  urge_buffer_sf_proj <- rbind(temp_hot_buffer, temp_cold_buffer)
  
  urge_spot_sf <- st_transform(urge_spot_sf_proj, crs = crs_latlng)
  urge_buffer_sf <- st_transform(urge_buffer_sf_proj, crs = crs_latlng)

  
  return(list(
    urge_spot_sf = urge_spot_sf,
    urge_buffer_sf = urge_buffer_sf
  ))
}
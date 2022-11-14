# set of functions to calculate variables based on raw movebank data

# append metadata from movebank 
# Input: Raw movebank meta and gps-data
# Output: Matched columns for both df
append_metadata   <- function(GPS.data,meta.data){
  
  # load and install all required packages
  if (!require("data.table")) install.packages("data.table")  
  if (!require("bit64")) install.packages("bit64") 
  require(data.table)
  require(bit64)
  
  # convert input to data.table
  GPS.data               <- data.table(GPS.data)
  meta.data              <- data.table(meta.data)
  
  # add new.columns
  GPS.data$species_name  <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"animal.taxon"]
  GPS.data$depl_comments <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"deployment.comments"]
  GPS.data$stage         <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"animal.life.stage"]
  GPS.data$weight        <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"animal.mass"]
  GPS.data$colour_ring   <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"animal.ring.id"]
  GPS.data$sex           <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"animal.sex"]
  GPS.data$tag_location  <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"deployment.comments"]
  GPS.data$deploy_end  <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"deploy.off.date"]
  GPS.data$deploy_start  <- meta.data[match(GPS.data$`tag.local.identifier`,meta.data$`tag.id`),"deploy.on.date"]
  return(GPS.data)
}

# create polygon with certain radius (m)
# Input: Lon and lat of the point of interest, radius of the polygon to be created
# Output: A circular Spatialpolygondataframe of certain radius around a GPS-position of interest
polygon_circle <- function(lon, lat, radius){
  
  # load and install all required packages
  if (!require("dplyr")) install.packages("dplyr")  
  if (!require("sf")) install.packages("sf") 
  require(dplyr)
  require(sf)
  
  points <- data.frame(
    long = lon,
    lat = lat)
  
  points <- points %>%
    # Transform data.frame in Simple Feature object
    st_as_sf(coords = c('lat', 'long'), dim = "XY") %>% 
    # Determine the coordinate system
    st_set_crs("EPSG:4326") %>%
    # Tranform geographical coordinates to cartesian
    # so we can choose the size of the buffer in meters
    st_transform( crs = 27700)  %>%
    # Calculate buffer
    st_buffer(dist = radius) %>%
    # Return to WGS 84 geographical coordinate system to export in KML
    st_transform( crs = 4326)
  points <- as_Spatial(points)
  
  
}

# check whether GPS-fixes are within a certain polygon (i.e. colony boundaries)
# Input:   Movebank data and a spatialpointspolygon
# Output: Adds TRUE/FALSE column to df whether point was in polygon 
polygon_check <- function(GPS_data, polygon){
  if (!require("sp")) install.packages("sp") 
  if (!require("rgeos")) install.packages("rgeos") 
  require(sp)
  require(rgeos)
  y=GPS_data$location.lat
  x=GPS_data$location.long
  points = data.frame(x, y)
  points   <- SpatialPoints(points)
  proj4string(polygon) <- CRS("EPSG:4326")
  proj4string(points) <- CRS("EPSG:4326")
  GPS_data$In_poly <- gContains((polygon), points, byid =TRUE)
  
}

# removes all points until bird left colony for the last time, this function can only be used after using the polygon_check function!
# ! still need to adapt function to work per cycle instead of per bird!
remove_colony <- function(GPS_data){
  warning("READ-FIRST: Removes all points until bird left colony for the last time, this function can only be used after using the polygon_check function! Still need to adapt function to work per cycle instead of per bird!")
  dummy <- NULL
  for (bird in unique(GPS_data$individual.local.identifier)){ 
    
    ind_bird <- GPS_data %>% filter(individual.local.identifier == bird)
    
    t <-(as.data.frame(t(tapply(seq_along(ind_bird$In_poly), ind_bird$In_poly, max))))
    t <-t$`TRUE`
    if (!is.null(t)) {
      ind_bird <- ind_bird[-c(1:t), ]
      dummy <- rbind(dummy, ind_bird)
    }
  }
  GPS_data <- dummy
  
}

### Classify stopover sites
### Input = (subsampled )GPS data, an epsilon value, and a minimal amount of points per cluster (cf dbscan)
### Output = A df with 3 added columns: Cluster, Cluster ID (unique per bird), and Migratory bout ID (unique per bird)
clusterandlabel <- function(GPS_data, eps, amount){
  ### cluster data with dbscan
  y=GPS_data$location.lat
  x=GPS_data$location.long
  points = data.frame(x, y)
  ### check whether epsilon value makes sense
  dbscan::kNNdistplot(points, k =  amount)
  abline(h = eps, lty = 2)
  
  #loop over birds, cluster stopover sites
  clustered_roosts <- NULL
  for (bird in unique(GPS_data$individual.local.identifier)) {
    ind_bird <- GPS_data %>% filter(individual.local.identifier == bird)
    y=ind_bird$location.lat
    x=ind_bird$location.long
    points = data.frame(x, y)
    t <- dbscan(points, eps = eps, minPts = amount)
    t <- (t[["cluster"]])
    ind_bird$cluster <-  t
    clustered_roosts <- rbind(clustered_roosts,ind_bird)
  }
  
  #create unique label per bird_stopover
  clustered_roosts$cluster_ID <- paste0(clustered_roosts$individual.local.identifier, "_cluster_",clustered_roosts$cluster)
  GPS_data <- clustered_roosts
  
  ### add migr bout column, label every bout as bird_#bout
  dummy <- NULL
  for (bird in unique(GPS_data$individual.local.identifier)) {
    ind_bird <- GPS_data  %>% filter(individual.local.identifier == bird)
    ind_bird <- ind_bird %>% 
      mutate(t = lag(cluster, default=TRUE) != cluster & cluster == 0, 
             c_p = cumsum(t), 
             migr_bout_ID = ifelse(cluster == 0, paste0(bird,"_bout_",c_p), 'stopover'), 
             t = NULL, c_p = NULL)
    
    
    dummy <- rbind(dummy, ind_bird)
    
    
    
    
    
    
    
  }
  
}

### Subsample data to a certain resolution
### Input = A movebank df, the cutoff value for your minutes 
### Output = A subsapled dataframe where rows got removed until their cumulative time difference almost reaches the cutoff value (cutoff - 1min)
subsample <- function(movebank_data, minutes){
  
  # load and install all required packages
  if (!require("data.table")) install.packages("data.table")  
  if (!require("dplyr")) install.packages("dplyr") 
  if (!require("MESS")) install.packages("MESS") 
  require(data.table)
  require(dplyr)
  require(MESS)
  
  # calculate
  movebank_data <- movebank_data %>%
    arrange(individual.local.identifier, timestamp) %>%
    group_by(individual.local.identifier) %>%
    mutate(diff_sec = strptime(timestamp, "%Y-%m-%d %H:%M:%S") - lag(strptime(timestamp, "%Y-%m-%d %H:%M:%S"), default = strptime(timestamp, "%Y-%m-%d %H:%M:%S")[1]))
  #apply for all birds
  movebank_data$delta <- as.numeric(movebank_data$diff_sec)/60  
  movebank_data$delta2 <- as.numeric(movebank_data$diff_sec)/60  
  
  movebank_data <- setDT(movebank_data)[, .SD[.N], by = MESS::cumsumbinning(delta2, minutes - 1, cutwhenpassed = TRUE)]
  
  
  
  
  
}

### Looks up raster values for coordinates
### Input = set of coordinates, a .tif file 
### Output = Value(s) of rasterlayer corresponding to coordinates

append_rasterlayer <-function(lon, lat, tif_file){
  # load and install all required packages
  if (!require("raster")) install.packages("raster")  
  if (!require("sp")) install.packages("sp") 
  require(raster)
  require(sp)
  #initialise empty df
  coords <- NULL
  # make lonlat df
  coords$lon <- lon
  coords$lat <- lat
  # extract values
  raster_values   <- raster::extract(tif_file,SpatialPoints((coords), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))  )
  return(raster_values)
  
}


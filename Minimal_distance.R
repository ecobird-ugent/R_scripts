## Calculate the minimal distance between points from day X and any point from X-1 and X-2
## Input: movebank data
## output: added column with minimal distance to any of the points in the previous two days


# tracker functions
source("https://raw.githubusercontent.com/ReinoudAllaert/Gull_tracking/main/Tracker_functions.R")

# dummy GPS-data
GPS_data <- read.csv("~Path_to_movebank_data")

# add bird_day
GPS_data$bird_day <- paste0(GPS_data$date, "_", GPS_data$colour_ring)

# empty df
dist_df <- NULL

# loop over bird_days, add minimal distance
for (i in unique(GPS_data$bird_day)) {
  # subset to day of interest
  day <- GPS_data %>% filter(bird_day == i)
  date <- as.Date(unique(day$date))
  date_1 <- date - 1
  date_2 <- date - 2
  # add 2 previous days
  day_1 <- GPS_data %>% filter(date == date_1)
  day_2 <- GPS_data %>% filter(date == date_2)
  # check if days are empty, if so: dist = NA
  if(nrow(day_1) == 0 | nrow(day_2) == 0 ){
    day$mindist <- NA
    dist_df <- rbind(dist_df,day)
    # if not, calculate minimal distance
  } else {
    day_1_2 <- rbind(day_1,day_2)
    day$mindist <- minimal_distance(day$location.long, day$location.lat, day_1_2$location.long, day_1_2$location.lat)
    dist_df <- rbind(dist_df,day)
  }
  
}



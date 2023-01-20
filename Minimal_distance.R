## Calculate the minimal distance between points from a certain day and all the previous days. 
## Can be used to caculate distance between patches.
## Input: movebank data
## output: added column with minimal distance to any of the points in the previous two days


# tracker functions
source("https://raw.githubusercontent.com/ReinoudAllaert/Gull_tracking/main/Tracker_functions.R")

# dummy GPS-data
GPS_data <- read.csv("~Path_to_movebank_data")

# add bird_day
GPS_data$date <- as.Date(GPS_data$timestamp)
GPS_data$bird_day <- paste0(GPS_data$date, "_", GPS_data$individual.local.identifier)

# empty df
dist_df <- NULL
count <- 0
total <- length(unique(GPS_data$bird_day))
# loop over bird_days, add minimal distance
for (i in unique(GPS_data$bird_day)) {
  # subset to day of interest
  day <- GPS_data %>% filter(bird_day == i)
  bird <- unique(day$individual.local.identifier)
  date <- unique(day$date)
  date_1 <- date - 1
  date_2 <- date - 2
  # add 2 previous days
  day_1 <- GPS_data %>% filter(date == date_1 & individual.local.identifier == bird)
  day_2 <- GPS_data %>% filter(date == date_2 & individual.local.identifier == bird)
  # check if days are empty, if so: dist = NA
  if(nrow(day_1) == 0 | nrow(day_2) == 0 ){
    day$mindist <- NA
    dist_df <- rbind(dist_df,day)
    count <- count + 1
    print(paste0(count, "/", total))
    # if not, calculate minimal distance
  } else {
    day_1_2 <- rbind(day_1,day_2)
    day$mindist <- minimal_distance(day$location.long, day$location.lat, day_1_2$location.long, day_1_2$location.lat)
    dist_df <- rbind(dist_df,day)
    count <- count + 1
    print(paste0(count, "/", total))
  }
  
}

GPS_data <- dist_df
GPS_data$fidelity <- "FALSE"
GPS_data$fidelity[GPS_data$mindist <= 500] <- "TRUE"

# site fidelity: proportion of points per day within 500m of points from prev 2days
df<- GPS_data %>%
  group_by(bird_day) %>%
  count(bird_day )
df2<- GPS_data %>%
  group_by(bird_day) %>%
  count(fidelity)
df2 <- subset(df2, fidelity == TRUE)
df <- merge(x=df,y=df2,by="bird_day")
df$fidelity_prop <- df$n.y/df$n.x

df <- df %>% separate(bird_day, c("date", "bird"), sep = "_")
df$date <- as.Date(df$date)

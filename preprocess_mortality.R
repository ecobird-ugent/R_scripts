require(googlesheets4)
require(tidyr)
require(dplyr)
require(data.table)

# (meta-)data
animals <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1NZnLRdTBtqpktkQ3bjbwUPja7BXfkYZPvGqJplTR-Fs/edit#gid=0"
  )
GPS_data <- fread("~/Downloads/HG_JUVENILE.csv")


# remove undeployed tags
animals <- subset(animals,!is.na(project))


# movebank metadata to proper columns
animals <-
  separate(
    data = animals,
    col = track_session_remarks,
    into = c("wing_length_rel", "head_length_rel", "tarsus_length_rel", "death_info"),
    sep = "\\|"
  )
animals$wing_length_rel <- sub(".*:", "", animals$wing_length_rel)
animals$tarsus_length_rel <- sub(".*:", "", animals$tarsus_length_rel)
animals$head_length_rel <- sub(".*:", "", animals$head_length_rel)

# remove malfunctional tags
animals <-
  subset(animals,!grepl("malfunc", animals$death_info, fixed = TRUE))

# cause of death to proper format
animals <- animals %>%
  mutate(
    death_info_cleaned = case_when(
      grepl("no further", animals$death_info) ~ 'No information',
      grepl("fox", animals$death_info) ~ 'Fox',
      grepl("traffic", animals$death_info) ~ 'Roadkill',
      grepl("turbine", animals$death_info) ~ 'Wind turbine',
      grepl("field", animals$death_info) ~ 'No information'
    )
  )

# remove living birds
animals <- subset(animals,!is.na(death_info_cleaned))


# subset GPS-data to only dead birds
GPS_data <-
  subset(GPS_data,
         `individual-local-identifier` %in% animals$metal_ring)

# fix timestamp
GPS_data$timestamp <- as.POSIXct(GPS_data$timestamp)

# retain week of data leading up to death by looping over every individual
GPS_death <- NULL
for (bird in unique(GPS_data$`individual-local-identifier`)) {
  temp <- subset(GPS_data, `individual-local-identifier` == bird)
  stop_date <- subset(animals, metal_ring == bird)
  stop_date <- as.POSIXct(stop_date$track_session_end_date)
  start_date <- stop_date - 7 * 24 * 60 * 60
  temp <- subset(temp, timestamp >= start_date)
  temp <- subset(temp, timestamp <= stop_date)
  GPS_death <- rbind(GPS_death, temp)
}

# add date column
GPS_death$date <- as.Date(GPS_death$timestamp)

# only retain birds for which we have 8 unique days of data
t <- GPS_death %>% count(`individual-local-identifier`, date)
t2 <- as.data.frame(table(t$`individual-local-identifier`))

# left with 28 individuals (from 38) for which we have a complete week of data
t2 <- subset(t2, Freq == 8)

GPS_death <-
  subset(GPS_death, `individual-local-identifier` %in% t2$Var)
t <- GPS_death %>% count(`individual-local-identifier`)

# if we only want data with an average resolution of 20min: 18 birds left
# if we go for 30 min: 26 birds left
t <- subset(t, n >= 330)

# leaves us with this dataset to calculate from:
GPS_death <-
  subset(GPS_death,
         `individual-local-identifier` %in% t$`individual-local-identifier`)

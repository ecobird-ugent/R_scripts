require(data.table)
require(dplyr)
# get GPS + ODBA/ACC data
GPS_data <- fread("~/Downloads/HG_JUVENILE.csv")
ODBA_df <- fread("~/Downloads/ODBA_GPS.csv")

# fix some column name issues
setnames(ODBA_df, "Collecting time", "Collecting_time")
colnames(ODBA_df) <- gsub("-", "_", colnames(ODBA_df))
colnames(GPS_data) <- gsub("-", "_", colnames(GPS_data))
setnames(ODBA_df, "UUID", "ID")
setnames(GPS_data, "tag_local_identifier", "ID")


# match data based on ID and timestamp, only retain ODBA for GPS-fix if timedif <= 5 minutes
# depending on the df size this might take a while to run
out <- sqldf("select a.ID,
              a.event_ID,
              a.timestamp, 
              b.Collecting_time [Collecting_time.y], 
              min(abs(a.timestamp - b.Collecting_time)) seconds, 
              b.ODBA
  from GPS_data a
  left join ODBA_df b on a.ID = b.ID and
                      abs(a.timestamp - b.Collecting_time) < 60 * 5
  group by a.rowid")[-4]

# backtransform datetime to readable format
out$Collecting_time.y <- as.POSIXct(out$Collecting_time.y, origin = "1970-01-01")

#



library(sqldf)

out <- sqldf("select a.tag.local.identifier, 
              a.timestamp, 
              b.timestamp [Collecting_time.y], 
              min(abs(a.timestamp - b.Collecting_time)) seconds, 
              b.x1, 
              b.x2
  from GPS_data a
  left join ODBA_df b on a.tag.local.identifier = b.tag.local.identifier and
                      abs(a.timestamp - b.timestamp) < 60 * 5
  group by a.rowid")[-4]
out$timestamp.y <- as.POSIXct(out$timestamp.y, origin = "1970-01-01")

# check
all.equal(out, target)



setkey(GPS_data, timestamp )
setkey( ODBA_df, Collecting_time )
combined <- GPS_data[ODBA_df, roll = Inf ]
combined <- subset(combined, select = c("event.id", "Transmitting time", "ODBA", "i.tag.local.identifier", "tag.local.identifier"))
combined<- subset(combined, !is.na(event.id))
t <- merge(x = GPS_data, y = combined, by = "event.id", all.x = TRUE)
t <- subset(t, event.id != NA)

# make the time stamp a date format for R
#animal.data$datetime <- as.POSIXct(animal.data$datetime)
colnames(GPS_data) <- gsub("-", ".", colnames(GPS_data))
# check how the consistent the tracking was
ggplot(GPS_data) +
  geom_point(aes(x = timestamp, y = `individual.local.identifier`))

meta_data <- fread("~/Downloads/HG_JUVENILE - Juvenile herring gulls (Larus argentatus, Laridae) hatched and handraised in Ostend (Belgium)-reference-data.csv")
GPS_data <- append_metadata(GPS_data, meta_data)

# add column with sun height in radians

data <- data.frame(date = GPS_data$timestamp,
                   lat = GPS_data$`location.lat`,
                   lon = GPS_data$`location.long`)

sun_calc <- getSunlightPosition(data = data, 
                                keep = c("altitude"))

GPS_data$Sun_height <- sun_calc$altitude

#0.17 rad = 10 deg
# Assuming that birds only start foraging as soon as sun is over 15degrees under horizon

GPS_data$day <- "day"
GPS_data$day[GPS_data$Sun_height <= -0.17] <- "night"
#only day positions
GPS_data <- subset(GPS_data, (day == 'day'))


GPS_data$date <- as.Date(GPS_data$timestamp)

# and the distribution of time lags
p <- GPS_data %>%
  group_by(`individual.local.identifier`) %>%
  mutate(time.lag = as.numeric(difftime(timestamp, lag(timestamp), units = "mins"))) %>%
  ggplot() +
  geom_density(aes(x = time.lag), fill = "black", alpha = 0.8)
ggplotly(p)


GPS_data <- GPS_data %>%
  group_by(`individual.local.identifier`,) %>%
  mutate(time.lag = round(as.numeric(difftime(timestamp, lag(timestamp), units = "mins")))) 

p2 <- GPS_data %>%
  group_by(date) %>%
  ggplot() +
  geom_density(aes(x = mean(time.lag)), fill = "black", alpha = 0.8)

p2 <- GPS_data %>% aggregate(time.lag~date, FUN=mean)


p2 <- p2 %>% ggplot() +
  geom_area(aes(x = date, y=time.lag), fill = "black", alpha = 0.8)









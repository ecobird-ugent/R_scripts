#easy package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# dependencies
packages <- c("move", "dplyr", "keyring", "googlesheets4", "leaflet", "htmltools", "RColorBrewer")
p_load(character.only = TRUE, packages)

# movebank data
GPS_data <- getMovebankLocationData(study=2217728245 , sensorID="GPS", 
                                    login=movebankLogin(username="Reinoud Allaert", password=key_get("movebank", "Reinoud Allaert")), 	
                                      timestamp_start = as.POSIXct(Sys.Date()-7))

# metadata 
animal_meta <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1NZnLRdTBtqpktkQ3bjbwUPja7BXfkYZPvGqJplTR-Fs/edit?usp=sharing"
  )


# filter metadata based on date (only vogels Jolien)
animal_meta <- animal_meta %>%
  subset(as.Date(track_session_start_date) >= as.Date("2023-07-01") &
           as.Date(track_session_start_date) <= as.Date("2023-08-01"))

# filter main gps data
GPS_data <- GPS_data %>%
  subset(individual.local.identifier %in% unique(animal_meta$metal_ring))

# Convert timestamp to POSIXct format
GPS_data$timestamp <- as.POSIXct(GPS_data$timestamp)


# daily distance in km
daily_distance <- GPS_data %>%
  group_by(individual.local.identifier, date = as.Date(timestamp)) %>%
  summarize(daily_distance = tryCatch(sum(distHaversine(cbind(location.long, location.lat))), error = function(e) NA)/1000)

# keep birds without data or under 10km/day
daily_distance <- daily_distance %>%
  filter(is.na(daily_distance) | daily_distance < 5)

# keep last 50 positions for every bird
GPS_data <- GPS_data %>%
  group_by(individual.local.identifier) %>%
  slice_tail(n = 50) %>%
  ungroup()

# filter to only problematic birds
GPS_data <- GPS_data %>%
  subset(individual.local.identifier %in% unique(daily_distance$individual.local.identifier))
unique(GPS_data$tag.local.identifier)

### plotmap

lbls <- paste(GPS_data$tag.local.identifier)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col=sample(col_vector, 62)

pal <- colorFactor(
  palette = col,
  domain = GPS_data$individual.local.identifier)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 3.017036, lat = 51.022090, zoom = 8.5) %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
  addCircleMarkers(data = GPS_data,
                   lat = ~location.lat,
                   lng = ~location.long,
                   color = ~pal(individual.local.identifier),
                   weight = 3,
                   fill = TRUE,
                   opacity = 0.7,
                   fillOpacity = 0.7,
                   radius = 2,
                   label = lapply(lbls, HTML),
                   popup = lapply(lbls, HTML))



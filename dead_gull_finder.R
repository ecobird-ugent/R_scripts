#easy package loading
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# dependencies
packages <- c("move", "dplyr", "keyring", "googlesheets4", "leaflet", "htmltools", "RColorBrewer")
p_load(character.only = TRUE, packages)

# movebank (meta)data
GPS_data <- getMovebankLocationData(study=2217728245 , sensorID="GPS", 
                                    login=movebankLogin(username="Reinoud Allaert", password=key_get("movebank", "Reinoud Allaert")), 	
                                      timestamp_start = as.POSIXct(Sys.Date()-4))

meta_data <- getMovebankReferenceTable(study=2217728245 ,  
                                    login=movebankLogin(username="Reinoud Allaert", password=key_get("movebank", "Reinoud Allaert")))

# only meta of deployed tags
meta_data <- meta_data %>% filter(!is.na(animal_timestamp_start) & number_of_location_events >= 10)

# filter main gps data
GPS_data <- GPS_data %>%
  subset(individual.id %in% unique(meta_data$animal_id))

# calculate daily distance per bird
daily_distance <- GPS_data %>%
  group_by(individual.local.identifier, date = as.Date(timestamp)) %>%
  summarize(daily_distance = tryCatch(sum(distHaversine(cbind(location.long, location.lat))), error = function(e) NA)/1000)


# calculate the average daily distance for each individual
average_distance <- daily_distance %>%
  group_by(individual.local.identifier) %>%
  summarize(
    avg_daily_distance = mean(daily_distance, na.rm = TRUE),
    days_available = sum(!is.na(daily_distance))
  )

# determine if each individual has data for at least 3 out of 4 days
alive_birds <- average_distance %>%
  filter(days_available >= 3)

# check if the average daily distance for each individual is > 1 km per day
alive_birds <- alive_birds %>%
  filter(avg_daily_distance > 1)

# split the dataframe into two groups: alive birds and dead birds
alive_individuals <- alive_birds$individual.local.identifier
alive_df <- daily_distance %>%
  filter(individual.local.identifier %in% alive_individuals)
alive_df <- unique(alive_df$individual.local.identifier)
alive_df


dead_individuals <- setdiff(daily_distance$individual.local.identifier, alive_individuals)
dead_df <- daily_distance %>%
  filter(individual.local.identifier %in% dead_individuals)
dead_df <- unique((dead_df$individual.local.identifier))
dead_df

# Print the results
print("Alive Birds:")
print(alive_df)

print("Dead Birds:")
print(dead_df)

# keep last 50 positions for every bird
GPS_data <- GPS_data %>%
  group_by(individual.local.identifier) %>%
  slice_tail(n = 10) %>%
  ungroup()

# filter to only problematic birds
GPS_data <- GPS_data %>%
  subset(individual.local.identifier %in% dead_df)
unique(GPS_data$tag.local.identifier)

### plotmap

lbls <- paste(GPS_data$tag.local.identifier, " ",GPS_data$timestamp )
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



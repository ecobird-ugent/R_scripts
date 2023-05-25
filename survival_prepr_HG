library(dplyr)
library(stringr)

tag_metadata <- read_sheet('https://docs.google.com/spreadsheets/d/1NZnLRdTBtqpktkQ3bjbwUPja7BXfkYZPvGqJplTR-Fs/edit#gid=0')
tag_metadata <- subset(tag_metadata, sync == 'ok')

#Add status for survival analysis
tag_metadata$status <- 1
tag_metadata$status[is.na(tag_metadata$track_session_end_date)] <- 0

# Calculate survival time based on track_session_end_date and track_session_start_date
tag_metadata <- tag_metadata %>% mutate(
  survival_time = ifelse(
    is.na(track_session_end_date),
    as.Date(Sys.Date()) - as.Date(track_session_start_date),
    as.Date(track_session_end_date) - as.Date(track_session_start_date)
  ),
  group = as.factor(track_session_start_date)
)

# Format biometry
tag_metadata <- data.frame(tag_metadata, do.call(rbind, str_split(tag_metadata$animal_measurements, " | ")))
tag_metadata <- tag_metadata %>% 
  rename(
    wing_length = X2,
    head_length = X5,
    tarsus_length = X8
  ) %>% 
  select(-starts_with("X"))
tag_metadata$animal_weight <- as.numeric(tag_metadata$animal_weight)
tag_metadata$wing_length <- as.numeric(tag_metadata$wing_length)
tag_metadata$head_length <- as.numeric(tag_metadata$head_length)
tag_metadata$tarsus_length <- as.numeric(tag_metadata$tarsus_length)

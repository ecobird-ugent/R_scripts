require(tidyr)
require(stringi)

#preprep GPS-data, returns data from week leading up to death
source("~/Documents/Github/R_scripts/preprocess_mortality.R")

#combine gps-data with metrics from the lab
source("~/Documents/Github/R_scripts/read_gull_database.R")
path_to_db <- "~/Documents/Github/R_scripts/voc_gulls.sqlite"
read_database(path_to_db)


t <- chick_measurements %>%
  arrange(chick_id, moment) %>%
  pivot_wider(names_from = moment, values_from = date) %>%
  select(-c(comment, red_blood, red_flush, initials))

m1_meas <- subset(t,!is.na(`2022_m1`)) %>%
  select(c(chick_id, weight, head, tarsus_l1, `2022_m1`))
m1_meas <- merge(x = m1_meas, y = chicks, by = "chick_id") %>%
  rename("weight_m1" = "weight",
         "head_m1" = "head",
         "tarsus_m1" = "tarsus_l1") %>%
  select(-c(enclosure, experiment, comments, red_diet,))

m1_meas <-
  merge(x = m1_meas, y = egg_measurements, by = "egg_id") %>%
  rename("date_egg_m" = "date")

animals <- merge(x = animals, y = m1_meas, by = "colour_ring")
animals$egg_number <- stri_sub(animals$egg_id,-1)


animals_gps <-
  merge(x = animals_gps, y = m1_meas, by = "colour_ring")
animals_gps$egg_number <- stri_sub(animals_gps$egg_id,-1)

rm(list = setdiff(ls(), c("animals", "animals_gps", "GPS_death")))

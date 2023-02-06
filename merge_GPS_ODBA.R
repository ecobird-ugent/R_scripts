# merges GPS data and ODBA data to a single dataframe
# takes 

require(data.table)
require(dplyr)
require(sqldf)


source('https://raw.githubusercontent.com/cobe-lab/R_scripts/main/Tracker_functions.R')

# get GPS + ODBA/ACC data
GPS_data <- fread("~/Downloads/HG_JUVENILE.csv")
ODBA_df <- fread("~/Downloads/ODBA_GPS.csv")

# does not change data, but fixes colname issues
GPS_data <- subsample(GPS_data, 0.0001)


# fix some column name issues for sqldf 
setnames(ODBA_df, "Collecting time", "Collecting_time")
colnames(GPS_data) <- gsub("\\.", "_", colnames(GPS_data))
setnames(ODBA_df, "UUID", "ID")
setnames(GPS_data, "tag_local_identifier", "ID")


# match data based on ID and timestamp, only retain ODBA for GPS-fix if timedif <= 10 minutes
# depending on the df size this might take a while to run
out <- sqldf("select a.ID, 
              a.timestamp,
              b.Collecting_time  [Collecting_time.y], 
              min(abs(a.timestamp - b.Collecting_time)) seconds, 
              b.ODBA
  from GPS_data a
  left join ODBA_df b on a.ID = b.ID and
                      abs(a.timestamp - b.Collecting_time) < 60 * 10
  group by a.rowid")[-4]

out$Collecting_time.y <- as.POSIXct(out$Collecting_time.y, origin = "1970-01-01")

out <- merge(x = GPS_data, y = out, by = "timestamp", all.x = TRUE)
out<-subset(out, ID.y == ID.x)
out$diff <- out$timestamp-out$Collecting_time.y


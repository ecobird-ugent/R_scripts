#reads the gull sqlite database
read_database <- function(path_to_db){
  # load and install all required packages
  if (!require("RODBC")) install.packages("RODBC")  
  if (!require("DBI")) install.packages("DBI") 
  if (!require("tidyverse")) install.packages("tidyverse") 
  
  require(data.table)
  require(bit64)
  require(tidyverse)
  
  
  # READ FROM SQLITE
  db <- dbConnect(RSQLite::SQLite(), path_to_db)
  sqlite_tables <- as_tibble(dbListTables(db)) %>% set_names("table_name")
  for (i in 1:nrow(sqlite_tables)){
    assign(sqlite_tables$table_name[i], as_tibble(dbReadTable(db, sqlite_tables$table_name[i])), envir=.GlobalEnv)
    assign(sqlite_tables$table_name[i], get(sqlite_tables$table_name[i]) %>%
             mutate_at(vars(starts_with("date")), funs(as.Date(as.POSIXct(., origin="1970-01-01")))), envir=.GlobalEnv)
  }
  dbDisconnect(db)
}













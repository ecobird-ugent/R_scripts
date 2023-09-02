add_tags <- function(tag_ids, group_values = rep(1, length(tag_ids))) {
  if (length(tag_ids) != length(group_values)) {
    stop("Length of tag_ids and group_values must be the same!")
  }
  
  base_sql <- "INSERT INTO tag (id, addr, `group`) VALUES"
  
  # Convert each tag ID into a SQL value tuple
  value_tuples <- mapply(function(tag_id, group_value) {
    addr <- tag_id + 4096
    return(paste0("(", tag_id, ", ", addr, ", ", group_value, ")"))
  }, tag_ids, group_values)
  
  # Combine the value tuples into a single string
  combined_values <- paste(value_tuples, collapse = ",\n")
  
  # Combine the base SQL with the value tuples
  full_sql <- paste(base_sql, combined_values, ";")
  
  # Print the SQL
  cat(full_sql)
}

add_tags(tags)


add_device <- function(tag_ids) {
  base_sql <- "INSERT INTO device (addr, mac, vbattATdfu, added, uwbTxPower) VALUES"
  
  # Convert each tag ID into a SQL value tuple
  value_tuples <- sapply(tag_ids, function(tag_id) {
    addr <- tag_id + 4096
    mac <- paste0("tag", tag_id)
    vbatt <- 3.000
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    uwbTxPower <- 0
    return(paste0("(", addr, ", '", mac, "', ", vbatt, ", '", timestamp, "', ", uwbTxPower, ")"))
  })
  
  # Combine the value tuples into a single string
  combined_values <- paste(value_tuples, collapse = ",\n")
  
  # Combine the base SQL with the value tuples
  full_sql <- paste(base_sql, combined_values, ";")
  
  # Print the SQL
  cat(full_sql)
}

# Test the function
tags <- c(75, 79, 84)
add_device(tags)


add_plan <- function(tag_ids, interval_8 = 30) {
  base_sql <- "INSERT INTO plan (addr, scenario, interval) VALUES"
  
  # Convert each tag ID into two SQL value tuples, one for each scenario
  value_tuples <- unlist(lapply(tag_ids, function(tag_id) {
    addr <- tag_id + 4096
    
    scenario_8_tuple <- paste0("(", addr, ", 8, ", interval_8, ")")
    scenario_12_tuple <- paste0("(", addr, ", 12, 60)")
    
    return(c(scenario_8_tuple, scenario_12_tuple))
  }))
  
  # Combine the value tuples into a single string
  combined_values <- paste(value_tuples, collapse = ",\n")
  
  # Combine the base SQL with the value tuples
  full_sql <- paste(base_sql, combined_values, ";")
  
  # Print the SQL
  cat(full_sql)
}





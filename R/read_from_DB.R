#' reads LM data from DB and converts column types
#' 
#' @param con connection to the database created with DBI::dbConnect
#' @param table_name name of the table in the database
#' @param docu_df excel documenting the column types

read_from_DB <- function(con, table_name, docu_df){
  
  start_time <- Sys.time()
  data <- DBI::dbReadTable(conn = con, table_name) %>% as_tibble()
  message(sprintf("Read data from DB in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  bin_cols <- docu_df %>% filter(type == "binary") %>% pull(Column)
  bin_cols <- bin_cols[bin_cols %in% colnames(data)]
  
  data <- data %>% 
    mutate_at(all_of(bin_cols), as.numeric)
  
  message(sprintf("Read data from DB & postprocessing in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(data) 
}
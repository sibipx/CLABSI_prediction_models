#' writes LM data to DB specifying column types based on documentation
#' 
#' @param con connection to the database created with DBI::dbConnect
#' @param table_name name of the table in the database
#' @param data LM data dataframe to be written to DB
#' @param docu_df excel documenting the column types

write_to_DB <- function(con, table_name, data, docu_df){
  
  start_time <- Sys.time()
  
  # get column types from docu_df
  cont_cols <- docu_df %>% filter(type == "cont") %>% pull(Column)
  cont_cols <- c(cont_cols, "functioneelDossierNr")
  bin_cols <- docu_df %>% filter(type == "binary") %>% pull(Column)
  categ_cols <- docu_df %>% filter(type == "categ") %>% pull(Column)
  date_cols <- c("CAT_start_time_episode", "CAT_end_time_episode", "LM_start_time", "LM_end_time")
  
  documented_cols <- docu_df$Column[docu_df$Column != "Table description"]
  documented_cols <- documented_cols[documented_cols %in% colnames(data)]
  
  # build string with column types
  all_cols_txt <- c()
  for (col in documented_cols){
    
    # set LM and catheter episode as smallint (not tinyint or real)
    if (col %in% c("LM", "CAT_catheter_episode")){
      col_txt <- sprintf("%s = 'smallint NULL'", col)
    } else {
      
      if (col %in% cont_cols) {
        col_values <- data[,col, drop = TRUE]
        col_values <- col_values[!is.na(col_values)]
        if (all(col_values == as.integer(col_values)) & 
            max_quiet(col_values) <= 255 & min_quiet(col_values) >= 0){ 
          col_txt <- sprintf("%s = 'tinyint NULL'", col)
        }
        else {
          col_txt <- sprintf("%s = 'float NULL'", col)
        }
      } else if (col %in% bin_cols) {
        col_txt <- sprintf("%s = 'bit NULL'", col)
      } else if (col %in% categ_cols) {
        col_txt <- sprintf("%s = 'varchar(50) NULL'", col)
      } else if (col %in% date_cols){
        col_txt <- sprintf("%s = 'datetime NULL'", col)
      } else {
        stop(sprintf("Column %s is not documented", col))
      }
      
    }
    
    all_cols_txt <- c(all_cols_txt, col_txt)
    
  }
  
  all_cols_txt <- paste(all_cols_txt, collapse = ", \n")
  all_cols_txt <- sprintf("c(%s)", all_cols_txt)
  
  #write to DB
  tryCatch(
    {
      dbExecute(con, sprintf("TRUNCATE TABLE %s", table_name))
      dbExecute(con, sprintf("DROP TABLE %s", table_name))
    },
    error=function(cond) {
      message("Error during drop & truncate table:")
      message(cond)
    },
    warning=function(cond) {
      message("Warning:")
      message(cond)
    }
  )    
  
  DBI::dbWriteTable(conn = con, name = table_name, value = data, 
                    field.types = eval(parse(text=all_cols_txt)))
  
  ak_constraint <- paste0("ak_constraint_", sample.int(1000000, 1))
  
  DBI::dbExecute(con, sprintf("ALTER TABLE mirclabsi.dbo.LM_data_12_13
          ADD CONSTRAINT %s UNIQUE (functioneelDossierNr, CAT_catheter_episode, LM);", ak_constraint))
  
  message(sprintf("Wrote data in DB in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
}
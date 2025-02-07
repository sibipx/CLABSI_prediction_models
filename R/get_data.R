#' Queries a table using a query string to which it adds AND functioneelDossierNr IN (list of IDs)
#' Needed t avoid error in case of large list of admission_ids:
#' Internal error: An expression services limit has been reached. 
#' Please look for potentially complex expressions in your query, and try to simplify them. 
#' https://docs.microsoft.com/en-US/sql/relational-databases/errors-events/mssqlserver-8632-database-engine-error?view=sql-server-ver15
#' 
#' @param con connection string
#' @param query first part of the query on the table
#' @param admission_ids vector of admissions IDs to add as a constraint to the query
#' 
#' @return a tibble with the query result

get_data <- function(con, query, admission_ids){
  
  # if the list if small enough fetch the data directly
  if (length(admission_ids) <= 30000){ 
    
    # build query
    query_constraint <- sprintf(" AND functioneelDossierNr IN (%s)", paste(admission_ids, collapse = ", "))
    query_complete <- paste0(query, query_constraint)
    
    # return table
    rs <- DBI::dbSendQuery(con, query_complete) 
    table <- DBI::dbFetch(rs) %>% as_tibble()
    dbClearResult(rs)
    
  } else { # if the list if big split in chunks and fetch the data for each chunk
    
    chunks <- split_vector(admission_ids, 30000)
    
    # get data for each chunk
    message(sprintf("Getting data in %s chunks...", length(chunks)))
    
    data_chunks <- list()
    for (i in 1:length(chunks)){
      message(sprintf("Getting chunk %s...", i))
      # build query
      query_constraint <- sprintf(" AND functioneelDossierNr IN (%s)", paste(chunks[[i]], collapse = ", "))
      query_complete <- paste0(query, query_constraint)
      
      # return table
      rs <- DBI::dbSendQuery(con, query_complete) 
      table_chunk <- DBI::dbFetch(rs) %>% as_tibble()
      dbClearResult(rs)
      
      data_chunks[[i]] <- table_chunk
    }
    
    # merge the data chunks together
    table <- data_chunks %>% reduce(rbind)
    
  }
  
  return(table)
  
}


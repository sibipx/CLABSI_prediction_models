#' Adds model evaluation metrics to the results df
#'
#' @param config_file configuration file that should contain 2 lines like this:
#' user <- "ealbu0"
#' password <- "XXXX"
#' 
#' @return connection 

db_connect <- function(config_file){
  
  require(odbc)
  require(DBI)
  
  source(config_file)
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    dsn = 'real_DB_name_not_shared', # the real DB name is anonymized on purpose
    UID = user,
    PWD = password,
    #encoding = "latin1",
    encoding = "UTF-8",
    clientcharset = "UTF-8")
  
  rm(password, pos = 1)
  
  return(con)
  
}


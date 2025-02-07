#' Checks if all dataframe columns are documented
#'
#' @param LM_data dataframe for wich to check if the columns are documented
#' @param docu_df excel sheet containing the dataframe documentation
#' @param cols_excluded columns that should be documented but where exceptionally excluded
#' 
#' @return does not return anything, only displays messages

check_columns <- function(data, docu_df, cols_excluded = NULL){

  # get columns documented
  docu_cols <- docu_df$Column[docu_df$Column != "Table description"]
  
  if (!is.null(cols_excluded)){
    docu_cols <- docu_cols[!docu_cols %in% cols_excluded]
  }
  
  # get dataframe columns
  data_cols <- colnames(data)

  # columns not documented or documented but not present in the dataframe
  cols_not_in_docu <- data_cols[!data_cols %in% docu_cols]
  cols_not_in_df <- docu_cols[!docu_cols %in% data_cols]
  
  if (length(cols_not_in_docu) > 0){
  message(sprintf("The following colums are not documented:\n%s", 
                  paste(cols_not_in_docu, collapse = ", \n")))
  }
  if (length(cols_not_in_df) > 0){
  message(sprintf("The following colums are documented but not present in the dataframe:\n%s", 
                  paste(cols_not_in_df, collapse = ", \n")))
  }
  
  if (length(cols_not_in_docu) + length(cols_not_in_df) == 0){
    message("You're a hero! Everything is properly documented!")
  }
  
}

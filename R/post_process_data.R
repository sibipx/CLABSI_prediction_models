#' post-processing LM data
#' 
#' data LM data

post_process_data <- function(data){
  
  # exclusion criteria
  if (exclude_neonates) {
    data <- data %>% 
      filter(PAT_age_in_weeks >= 13 | is.na(PAT_age_in_weeks))
  }
  
  data <- data %>% 
    select(-PAT_age_in_weeks)
  
  # exclude columns based on configuration
  cols <- columns_to_exclude[columns_to_exclude %in% colnames(data)]
  data <- data %>% 
    select(-all_of(cols))
  
  return(data)
}
#' cleaning dynamic LM data
#' 
#' @param data LM data 

clean_dynamic_data_train <- function(data){
  
  n_col_original <- ncol(data)
  
  # keep only the largest set of columns
  cols_not_in_data <- feature_set_2[!feature_set_2 %in% colnames(data)]
  
  message(sprintf("Columns to keep not in data columns: %s", 
                  paste(cols_not_in_data, collapse = ", ")))
  
  cols_to_keep <- feature_set_2[feature_set_2 %in% colnames(data)]
  cols_to_keep <- c("functioneelDossierNr", "CAT_catheter_episode", "LM", 
                    "eventtime", "type", cols_to_keep)
  
  data <- data %>% 
    select(all_of(cols_to_keep))
  
  # keep the admission ward (at baseline, LM 0)
  data_baseline_ward <- data %>% 
    filter(CAT_catheter_episode == 1, LM == 0) %>%    
    select(functioneelDossierNr, MS_physical_ward) %>% 
    rename(MS_physical_ward_base = MS_physical_ward) 
  
  data <- data %>% 
    left_join(data_baseline_ward, 
              by = join_by(functioneelDossierNr)) %>% 
    select(-MS_physical_ward)
  
  # check - events before LM
  n_events_before_LM <- data %>% 
    filter(eventtime <= LM) %>% 
    nrow()
  
  if (n_events_before_LM > 0) stop("Events before LM exist in data")
  
  # lumens imputation (hardcoded for fixed values)
  data <- impute_lumens_train(data)
  
  # fill dialysis catheter location as colarbone (hardcoded) 
  data <- data %>% 
    mutate(CAT_catheter_location_binary_all_Collarbone = if_else(CAT_catheter_location_binary_all_dialysis_unknown == 1,
                                                                 1, CAT_catheter_location_binary_all_Collarbone)) %>% 
    select(-CAT_catheter_location_binary_all_dialysis_unknown)
  
  # MS_ICU_time_before_catheter - Time spent in ICU since admission to catheter placement. (so before LM0)
  # MS_ICU_time_before_LM  - Time spent in ICU between LM12 and LM13.
  # MS_is_ICU_unit – Is the patient now (LM13) in ICU?
  # calculate MS_total_ICU_time_before_LM = ICU time from catheter placement to current LM (e.g.: LM13)
  data <- data %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    arrange(LM) %>% 
    mutate(MS_total_ICU_time_before_LM = cumsum(MS_ICU_time_before_LM)) %>% 
    ungroup() %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    select(-MS_ICU_time_before_LM)
    
  # reduction in number of columns
  n_col_final <- ncol(data)
  
  message(sprintf("Original data had %s columns, cleaned data has: %s columns", 
                  n_col_original, n_col_final))
  
  return(data)
}


impute_lumens_train <- function(data){
  # lumens imputation (hardcoded for fixed values)
  data <- data %>% 
    # init values at baseline
    mutate(CAT_lumens_PICC = if_else(is.na(CAT_lumens_PICC) & LM == 0, 1, CAT_lumens_PICC),
           CAT_lumens_CVC = if_else(is.na(CAT_lumens_CVC) & LM == 0, 2, CAT_lumens_CVC),
           CAT_lumens_Tunneled_CVC = if_else(is.na(CAT_lumens_Tunneled_CVC) & LM == 0, 3, 
                                             CAT_lumens_Tunneled_CVC)) %>% 
    # LOCF
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    arrange(LM) %>% 
    fill(all_of(c("CAT_lumens_PICC", "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC")), 
         .direction = "down") %>% 
    ungroup() %>% 
    # calculate total lumens
    mutate(CAT_lumens_total = CAT_lumens_PICC + CAT_lumens_CVC +
             CAT_lumens_Tunneled_CVC + CAT_lumens_Dialysis_CVC +
             CAT_lumens_Port_a_cath) %>% 
    select(-c(CAT_lumens_PICC, CAT_lumens_CVC,
              CAT_lumens_Tunneled_CVC, CAT_lumens_Dialysis_CVC,
              CAT_lumens_Port_a_cath))
  
  return(data)
}


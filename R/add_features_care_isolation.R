#' Adds features related to patient isolation to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_isolation <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_ISO_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE isolation measures features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_isolation$Dutch, collapse = "', '")
  
  message("Reading CARE data from the database...")
  
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, zorgDefinitieCode
  FROM careModule_care_tb
  WHERE zorgDefinitieCode in ('%s')", attribs_str)
  CARE_data <- get_data(con, query, admission_ids)
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, startDate, endDate 
                   FROM pdmsParamTaken WHERE parameterName in ('%s')", attribs_str)
  PDMS_data <- get_data(con, query, admission_ids)
  
  # Calculate isolation feature
  # ---------------------------
  
  # map names
  CARE_data <- CARE_data %>% 
    rename(value = zorgDefinitieCode) %>% 
    mutate(value = map_names(value, dict_isolation)) 
  
  PDMS_data <- PDMS_data %>% 
    rename(value = parameterName) %>% 
    mutate(value = map_names(value, dict_isolation))
  
  # get values during LM separately for CARE and PDMS 
  # (CARE has values at times, PDMS has start - end date)
  CARE_data_during_LM <- CARE_data %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= LM_start_time, 
           tijdstip <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, tijdstip))
  
  PDMS_data_during_LM <- PDMS_data %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    mutate(endDate = if_else(is.na(endDate), LM_end_time + hours(1), endDate)) %>% # this should work dynamically too
    filter(endDate >= LM_start_time, 
           startDate <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, startDate, endDate))
  
  data_during_LM <- CARE_data_during_LM %>% 
    rbind(PDMS_data_during_LM) %>% 
    distinct() %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(CARE_ISO = list(value)) %>% 
    ungroup() %>% 
    select(-value) %>% 
    distinct()
  
  data_during_LM <- data_during_LM %>% 
    make_col_binary_all(cols = c("CARE_ISO")) %>% 
    select(-CARE_ISO) %>% 
    distinct()
  
  # make combined isolation both source and protective
  data_during_LM <- data_during_LM %>% 
    mutate(CARE_ISO_binary_all_source_isolation = if_else(CARE_ISO_binary_all_combined_isolation == 1,
                                                          1, CARE_ISO_binary_all_source_isolation),
           CARE_ISO_binary_all_protective_isolation = if_else(CARE_ISO_binary_all_combined_isolation == 1,
                                                          1, CARE_ISO_binary_all_protective_isolation)) %>% 
    select(-CARE_ISO_binary_all_combined_isolation)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(data_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("CARE_ISO_")), ~ if_else(is.na(.), 0, .)) # make 0 when not recorded
  
  message(sprintf("CARE isolation features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
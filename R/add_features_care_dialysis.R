#' Adds features related to dialysis to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_dialysis <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_DIA_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE dialysis features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_dialysis$Dutch, collapse = "', '")
  
  message("Reading CARE data from the database...")
  
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, zorgDefinitieCode
  FROM careModule_care_tb
  WHERE zorgDefinitieCode in ('%s')", attribs_str)
  CARE_data <- get_data(con, query, admission_ids)
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date 
                   FROM pdmsParamNumeric WHERE parameterName in ('%s')", attribs_str)
  PDMS_num <- get_data(con, query, admission_ids)
  
  # Note Elena: I am using Time and ToTime as start/stop dialysis as per the explanation received
  # It might not be the right choice, EnteredOn is sometimes after Time 
  # It might be a future leak, but I leave like this for now
  query <- sprintf("SELECT functioneelDossierNr, parameterName, Time, ToTime 
                   FROM pdmsParamClinicalEvent WHERE parameterName in ('%s')", attribs_str)
  PDMS_clin <- get_data(con, query, admission_ids)
  
  # Calculate dialysis feature
  # --------------------------
  
  # map names
  CARE_data <- CARE_data %>% 
    rename(value = zorgDefinitieCode,
           date = tijdstip) %>% 
    mutate(value = map_names(value, dict_dialysis)) 
  
  PDMS_num <- PDMS_num %>% 
    rename(value = parameterName) %>% 
    mutate(value = map_names(value, dict_dialysis))
  
  PDMS_clin <- PDMS_clin %>% 
    rename(value = parameterName) %>% 
    mutate(value = map_names(value, dict_dialysis))
  
  # get instantaneous values during LM
  dialysis_values <- CARE_data %>% 
    rbind(PDMS_num)
  
  dialysis_values_during_LM <- dialysis_values %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, date))
  
  # get start /end dialysis during LM (from PDMS_clin)
  dialysis_start_stop_during_LM <- PDMS_clin %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    mutate(ToTime = if_else(is.na(ToTime), LM_end_time + hours(1), ToTime)) %>% # this should work dynamically too
    filter(ToTime >= LM_start_time, 
           Time <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, Time, ToTime))
  
  data_during_LM <- dialysis_values_during_LM %>% 
    rbind(dialysis_start_stop_during_LM) %>% 
    distinct()
  
  data_during_LM <- data_during_LM %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(CARE_DIA = list(value)) %>% 
    ungroup() %>% 
    select(-value) %>% 
    distinct()
  
  data_during_LM <- data_during_LM %>% 
    make_col_binary_all(cols = c("CARE_DIA")) %>% 
    select(-CARE_DIA) %>% 
    distinct()
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(data_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("CARE_DIA_")), ~ if_else(is.na(.), 0, .)) # make 0 when not recorded
  
  message(sprintf("CARE dialysis features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
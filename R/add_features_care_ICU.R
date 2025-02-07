#' Adds ICU specific features to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_ICU <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_ICU_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE ICU features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_ICU_params$Dutch, collapse = "', '")
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, startDate, endDate 
                   FROM pdmsParamTaken WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_taken <- get_data(con, query, admission_ids)
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date, value 
                   FROM pdmsParamNumeric WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_num <- get_data(con, query, admission_ids)
  
  PDMS_data_taken <- PDMS_data_taken %>% 
    mutate(parameterName = map_names(parameterName, dict_ICU_params)) %>% 
    mutate(value = parameterName) 
  
  # Calculate ECMO feature
  # ----------------------
  
  # PDMS has start - end date  
  ECMO_during_LM <- PDMS_data_taken %>% 
    filter(parameterName == "ECMO") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    mutate(endDate = if_else(is.na(endDate), LM_end_time + hours(1), endDate)) %>% # this should work dynamically too
    filter(endDate >= LM_start_time, 
           startDate <= LM_end_time) %>% 
    mutate(CARE_ICU_ECMO = 1) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CARE_ICU_ECMO)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(ECMO_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_ICU_ECMO = if_else(is.na(CARE_ICU_ECMO), 0, CARE_ICU_ECMO)) # make 0 when not recorded
  
  # Add feature - percent of body burned
  # ------------------------------------
  
  percent_body_burned_during_LM <- PDMS_data_num %>% 
    mutate(parameterName = map_names(parameterName, dict_ICU_params)) %>% 
    filter(parameterName == "percent_body_burned") %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(value <= percent_range[[2]],
           value >= percent_range[[1]]) %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, value, date)
  
  percent_body_burned_during_LM <- percent_body_burned_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    arrange(date) %>% 
    mutate(CARE_ICU_percent_body_burned = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_ICU_percent_body_burned) %>% 
    distinct()
  
  percent_body_burned_during_LM <- percent_body_burned_during_LM %>% 
    make_col_max(cols = c("CARE_ICU_percent_body_burned")) %>% 
    select(-CARE_ICU_percent_body_burned)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(percent_body_burned_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))

  message(sprintf("CARE ICU features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
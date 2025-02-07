#' Adds features related to woundsto the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_wound <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_WND_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE wound features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_wound$Dutch, collapse = "', '")
  
  message("Reading CARE data from the database...")
  
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, waarde, attribuutDefinitieCode
  FROM careModule_care_tb
  WHERE attribuutDefinitieCode in ('%s')", attribs_str)
  CARE_data <- get_data(con, query, admission_ids)
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date, txt 
                   FROM pdmsParamText WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_text <- get_data(con, query, admission_ids)
  
  # Align and prepare data
  # ----------------------
  
  # align CARE with PDMS
  CARE_data <- CARE_data %>% 
    rename(date = tijdstip,
           value = waarde, 
           parameterName = attribuutDefinitieCode) 
  
  PDMS_data_text <- PDMS_data_text %>% 
    rename(value = txt)
  
  # merge CARE and PDMS 
  CARE_data <- CARE_data %>% 
    rbind(PDMS_data_text) 
  
  CARE_data <- CARE_data %>% 
    mutate(parameterName = map_names(parameterName, dict_wound))
  
  # Add feature - wound type
  # ------------------------
  
  message("Adding features - wound type...")
  
  wound_type_during_LM <- CARE_data %>% 
    filter(parameterName == "wound_type") %>% 
    mutate(value = map_names(value, dict_wound_type)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time))
  
  wound_type_during_LM <- wound_type_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(CARE_WND_wound_type = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_WND_wound_type) %>% 
    distinct()
  
  wound_type_during_LM <- wound_type_during_LM %>% 
    make_col_binary_all(cols = c("CARE_WND_wound_type")) %>% 
    select(-c(CARE_WND_wound_type))
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(wound_type_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  message(sprintf("CARE wound features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
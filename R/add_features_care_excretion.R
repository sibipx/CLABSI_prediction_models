#' Adds features related to excretion to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_excretion <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_EXC_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE excretion features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_excretion$Dutch, collapse = "', '")
  
  message("Reading CARE data from the database...")
  
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, waarde, attribuutDefinitieCode
  FROM careModule_care_tb
  WHERE attribuutDefinitieCode in ('%s')", attribs_str)
  CARE_data <- get_data(con, query, admission_ids)
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date, txt 
                   FROM pdmsParamText WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_text <- get_data(con, query, admission_ids)
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date, value 
                   FROM pdmsParamCheckBox WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_check <- get_data(con, query, admission_ids)
  
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
    rbind(PDMS_data_text) %>% 
    rbind(PDMS_data_check) 
  
  CARE_data <- CARE_data %>% 
    mutate(parameterName = map_names(parameterName, dict_excretion))
  
  # for this topic, I take features one by one (different mapping and subject to change)
  
  # Add feature - faecal incontinence
  # --------------------------------
  
  message("Adding features - faecal incontinence...")
  
  faecal_incontinence_during_LM <- CARE_data %>% 
    filter(parameterName == "faecal_incontinence") %>% 
    mutate(value = map_names(value, dict_faecal_incontinence)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, parameterName, date, value)) %>% 
    mutate(CARE_EXC_faecal_incontinence = 1) %>% # the dictionary is mapped to one value only
    distinct()
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(faecal_incontinence_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_EXC_faecal_incontinence = if_else(is.na(CARE_EXC_faecal_incontinence), 0, CARE_EXC_faecal_incontinence))
  
  # Add feature - urinary incontinence
  # ----------------------------------
  
  message("Adding features - urinary incontinence...")
  
  urinary_incontinence_during_LM <- CARE_data %>% 
    filter(parameterName == "urinary_incontinence") %>% 
    mutate(value = map_names(value, dict_urinary_incontinence)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, parameterName, date, value)) %>% 
    mutate(CARE_EXC_urinary_incontinence = 1) %>% # the dictionary is mapped to one value only
    distinct()
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(urinary_incontinence_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_EXC_urinary_incontinence = if_else(is.na(CARE_EXC_urinary_incontinence), 0, CARE_EXC_urinary_incontinence))
  
  message(sprintf("CARE excretion features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
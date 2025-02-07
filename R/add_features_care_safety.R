#' Adds features related to safety parameters to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_safety <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_SAF_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE safety measures features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_safety_attributes$Dutch, collapse = "', '")
  
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
                   FROM pdmsParamFreeText WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_free_text <- get_data(con, query, admission_ids)
  
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
    rbind(PDMS_data_free_text)
  
  CARE_data <- CARE_data %>% 
    mutate(parameterName = map_names(parameterName, dict_safety_attributes))
  
  # for this topic, I take features one by one (different mapping and subject to change)
  
  # Add feature - decubitus risk
  # ----------------------------
  
  # Decision to drop due to inconsistency in recording
  
#   message("Adding features - decubitus risk...")
#   
#   decubitus_risk_during_LM <- CARE_data %>% 
#     filter(parameterName == "decubitus_risk") %>% 
#     mutate(value = map_names(value, dict_decubitus_risk_values)) %>% 
#     filter(value != "UNKNOWN_VALUE") %>% 
#     left_join(LM_data %>% 
#                 select(functioneelDossierNr, CAT_catheter_episode, 
#                        LM, LM_start_time, LM_end_time),
#               by = "functioneelDossierNr", multiple = "all") %>% 
#     filter(date >= LM_start_time, 
#            date <= LM_end_time) %>% 
#     select(-c(LM_start_time, LM_end_time))
#   
#   decubitus_risk_during_LM <- decubitus_risk_during_LM %>% 
#     arrange(date) %>% 
#     group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
#     arrange(date) %>% 
#     mutate(CARE_SAF_decubits_risk = list(value)) %>% 
#     ungroup() %>% 
#     select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_decubits_risk) %>% 
#     distinct()
#   
#   decubitus_risk_during_LM <- decubitus_risk_during_LM %>% 
#     make_col_binary_last(cols = c("CARE_SAF_decubits_risk")) %>% 
#     select(-c(CARE_SAF_decubits_risk, CARE_SAF_decubits_risk_binary_last_NO))
#   
#   # merge in LM data
#   LM_data <- LM_data %>% 
#     left_join(decubitus_risk_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
#   
  # Add feature - patient positioning
  # ---------------------------------
  
  message("Adding features - patient positioning...")
  
  pat_pos_during_LM <- CARE_data %>% 
    filter(parameterName == "patient_position") %>% 
    mutate(value = map_names(value, dict_position_values)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time))
  
  pat_pos_during_LM <- pat_pos_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(CARE_SAF_patient_position = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_patient_position) %>% 
    distinct()
  
  pat_pos_during_LM <- pat_pos_during_LM %>% 
    make_col_binary_all(cols = c("CARE_SAF_patient_position")) %>% 
    select(-CARE_SAF_patient_position)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(pat_pos_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # Add feature - delirium
  # ----------------------
  
  message("Adding features - delirium...")
  
  delirium_during_LM <- CARE_data %>% 
    filter(parameterName == "delirium") %>% 
    mutate(value = map_names(value, dict_delirium_values)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time))

  delirium_during_LM <- delirium_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(CARE_SAF_delirium = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_delirium) %>% 
    distinct()
  
  delirium_during_LM <- delirium_during_LM %>% 
    make_col_binary_last(cols = c("CARE_SAF_delirium")) %>% 
    select(-c(CARE_SAF_delirium, CARE_SAF_delirium_binary_last_NO))
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(delirium_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_SAF_delirium_binary_last_YES = if_else(is.na(CARE_SAF_delirium_binary_last_YES),
                                                      0, CARE_SAF_delirium_binary_last_YES))
  
  # Add feature - freedom_resctriction_measure
  # ------------------------------------------
  
  message("Adding features - freedom resctriction measure...")
  
  freedom_restriction_during_LM <- CARE_data %>% 
    filter(parameterName == "freedom_resctriction_measure") %>% 
    mutate(value = map_names(value, dict_freedom_resctriction_measure_values)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time))
  
  freedom_restriction_during_LM <- freedom_restriction_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(CARE_SAF_freedom_restriction = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_freedom_restriction) %>% 
    distinct()
  
  freedom_restriction_during_LM <- freedom_restriction_during_LM %>% 
    make_col_categorical_last(cols = c("CARE_SAF_freedom_restriction")) %>% 
    select(-c(CARE_SAF_freedom_restriction))
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(freedom_restriction_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # Add feature - risk_of_falling
  # -----------------------------
  
  # Decision to drop due to inconsistency in recording
  
#  message("Adding features - risk of falling...")
#  
#  risk_of_falling_during_LM <- CARE_data %>% 
#    filter(parameterName == "risk_of_falling") %>% 
#    mutate(value = map_names(value, dict_risk_of_falling_values)) %>% 
#    filter(value != "UNKNOWN_VALUE") %>% 
#    left_join(LM_data %>% 
#                select(functioneelDossierNr, CAT_catheter_episode, 
#                       LM, LM_start_time, LM_end_time),
#              by = "functioneelDossierNr", multiple = "all") %>% 
#    filter(date >= LM_start_time, 
#           date <= LM_end_time) %>% 
#    select(-c(LM_start_time, LM_end_time))
#  
#  risk_of_falling_during_LM <- risk_of_falling_during_LM %>% 
#    arrange(date) %>% 
#    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
#    arrange(date) %>% 
#    mutate(CARE_SAF_risk_of_falling = list(value)) %>% 
#    ungroup() %>% 
#    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_risk_of_falling) %>% 
#    distinct()
#  
#  risk_of_falling_during_LM <- risk_of_falling_during_LM %>% 
#    make_col_binary_last(cols = c("CARE_SAF_risk_of_falling")) %>% 
#    select(-c(CARE_SAF_risk_of_falling, CARE_SAF_risk_of_falling_binary_last_NO))
#  
#  # merge in LM data
#  LM_data <- LM_data %>% 
#    left_join(risk_of_falling_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # Add feature - mobility assistance
  # ---------------------------------
  
  message("Adding features - mobility assistance...")
  
  mobility_during_LM <- CARE_data %>% 
    filter(parameterName == "mobility_assistance") %>% 
    mutate(value = map_names(value, dict_mobility_assistance_values)) %>% 
    filter(value != "UNKNOWN_VALUE") %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time)) %>% 
    distinct()
  
  mobility_during_LM <- mobility_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(CARE_SAF_mobility_assistance = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_SAF_mobility_assistance) %>% 
    distinct()
  
  mobility_during_LM <- mobility_during_LM %>% 
    make_col_binary_all(cols = c("CARE_SAF_mobility_assistance")) %>% 
    select(-CARE_SAF_mobility_assistance)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(mobility_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  message(sprintf("CARE safety measures features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
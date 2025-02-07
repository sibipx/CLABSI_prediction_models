#' Adds features related to physical parameters to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_physical_params <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_PHY_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE physical parameters features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  attribs_str <- paste(dict_physical_params$Dutch, collapse = "', '")
  
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
  
  query <- sprintf("SELECT functioneelDossierNr, parameterName, date, value 
                   FROM pdmsParamNumeric WHERE parameterName in ('%s')", attribs_str)
  PDMS_data_num <- get_data(con, query, admission_ids)
  
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
    rbind(PDMS_data_check) %>%
    rbind(PDMS_data_num)
  
  CARE_data <- CARE_data %>% 
    mutate(parameterName = map_names(parameterName, dict_physical_params))
  
  # for this topic, I take features one by one (different mapping and subject to change)
  
  # Add feature - flap
  # ------------------
  
  message("Adding features - flap...")
  
  flap_during_LM <- CARE_data %>% 
    filter(parameterName == "flap") %>% 
    mutate(CARE_PHY_flap = 1) %>% # presence of a flap
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CARE_PHY_flap)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(flap_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_PHY_flap = if_else(is.na(CARE_PHY_flap), 0, CARE_PHY_flap))
  
  # Add feature - drain
  # -------------------
  
  message("Adding features - drain...")
  
  drain_during_LM <- CARE_data %>% 
    filter(parameterName == "drain") %>% 
    mutate(CARE_PHY_drain = 1) %>% # presence of a drain
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CARE_PHY_drain)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(drain_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CARE_PHY_drain = if_else(is.na(CARE_PHY_drain), 0, CARE_PHY_drain))
  
  # Add features - weight and length
  # --------------------------------
  
  message("Adding features - weight and length...")
  
  # weight
  weight_during_LM <- CARE_data %>% 
    filter(parameterName == "weight") %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(value >= weight_range[[1]],
           value <= weight_range[[2]]) %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, value, date)
  
  weight_during_LM <- weight_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    arrange(date) %>% 
    mutate(CARE_PHY_weight = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_PHY_weight) %>% 
    distinct()
  
  weight_during_LM <- weight_during_LM %>% 
    make_col_mean(cols = c("CARE_PHY_weight")) %>% 
    select(-CARE_PHY_weight)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(weight_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # length
  length_during_LM <- CARE_data %>% 
    filter(parameterName == "length") %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(value >= length_range[[1]],
           value <= length_range[[2]]) %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, value, date)
  
  length_during_LM <- length_during_LM %>% 
    arrange(date) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    arrange(date) %>% 
    mutate(CARE_PHY_length = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_PHY_length) %>% 
    distinct()
  
  length_during_LM <- length_during_LM %>% 
    make_col_mean(cols = c("CARE_PHY_length")) %>% 
    select(-CARE_PHY_length)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(length_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # transform length in m (for later BMI calculation)
  LM_data <- LM_data %>% 
    mutate(CARE_PHY_length_mean = CARE_PHY_length_mean/100)
  
  # Note: I do not calculate BMI here as I think it's better to impute first and derive BMI afterwards
  
  # Add feature - glycemia
  # ----------------------
  
  # Decision to drop due to inconsistency in recording
  
#  message("Adding features - glycemia...")
#  
#  glycemia_during_LM <- CARE_data %>% 
#    filter(parameterName == "glycemia") %>% 
#    mutate(value = as.numeric(value)) %>% 
#    filter(value >= glycemia_range[[1]],
#           value <= glycemia_range[[2]]) %>% 
#    left_join(LM_data %>% 
#                select(functioneelDossierNr, CAT_catheter_episode, 
#                       LM, LM_start_time, LM_end_time),
#              by = "functioneelDossierNr", multiple = "all") %>% 
#    filter(date >= LM_start_time, 
#           date <= LM_end_time) %>% 
#    distinct(functioneelDossierNr, CAT_catheter_episode, LM, value, date)
#  
#  glycemia_during_LM <- glycemia_during_LM %>% 
#    arrange(date) %>% 
#    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
#    arrange(date) %>% 
#    mutate(CARE_PHY_glycemia = list(value)) %>% 
#    ungroup() %>% 
#    select(functioneelDossierNr, CAT_catheter_episode, LM, CARE_PHY_glycemia) %>% 
#    distinct()
#  
#  if (nrow(glycemia_during_LM) >  0){
#    
#    glycemia_during_LM <- glycemia_during_LM %>% 
#      make_col_max(cols = c("CARE_PHY_glycemia")) %>% 
#      make_col_min(cols = c("CARE_PHY_glycemia")) %>% 
#      select(-CARE_PHY_glycemia)
#    
#    # merge in LM data
#    LM_data <- LM_data %>% 
#      left_join(glycemia_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
#    
#  } else {
#    LM_data$CARE_PHY_glycemia_min <- NA_real_
#    LM_data$CARE_PHY_glycemia_max <- NA_real_
#  }
  
  message(sprintf("CARE physical parameters features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
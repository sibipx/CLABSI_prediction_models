#' Adds features related to neurology parameters to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_neuro <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_NEU_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE neurology features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE & PDMS data
  # --------------------
  
  neu_attribs_str <- paste(dict_neurology_params$Dutch, collapse = "', '")
  
  message("Reading CARE data from the database...")

  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, waarde, attribuutDefinitieCode
  FROM careModule_care_tb
  WHERE attribuutDefinitieCode in ('%s')", neu_attribs_str)
  CARE_data <- get_data(con, query, admission_ids)
  
  message("Reading PDMS data from the database...")
  
  query <- sprintf("SELECT date, parameterName, value, functioneelDossierNr 
           FROM pdmsParamNumeric
           WHERE parameterName in ('%s')", neu_attribs_str)
  
  PDMS_data <- get_data(con, query, admission_ids)
  
  # Align and prepare data
  # ----------------------
  
  # align CARE with PDMS
  CARE_data <- CARE_data %>% 
    rename(date = tijdstip,
           value = waarde, 
           parameterName = attribuutDefinitieCode) %>% 
    mutate(value = as.numeric(value))
  
  # merge CARE and PDMS 
  CARE_data <- CARE_data %>% 
    rbind(PDMS_data) 
  
  CARE_data <- CARE_data %>% 
    mutate(parameterName = map_names(parameterName, dict_neurology_params))
  
  # check UNKNOWN_VALUE
  number_unknown <- CARE_data %>% 
    filter(parameterName == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message(sprintf("There are %s attributes with UNKNOWN_VALUE. These are removed (if any).",  number_unknown))
  
  CARE_data <- CARE_data %>% 
    filter(parameterName != "UNKNOWN_VALUE") 
  
  # prefix the soon to be columns
  CARE_data <- CARE_data %>% 
    mutate(parameterName = paste0("CARE_NEU_", parameterName)) %>% 
    arrange(functioneelDossierNr, date)
  
  # Extract features during LM
  # --------------------------
  
  CARE_data_during_LM <- CARE_data %>% 
    filter(!is.na(value)) %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date >= LM_start_time, 
           date <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time))
  
  CARE_data_during_LM <- CARE_data_during_LM %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, parameterName) %>% 
    arrange(date) %>% 
    mutate(value_list = list(value)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, parameterName, value_list) %>% 
    distinct()
  
  CARE_data_during_LM <- CARE_data_during_LM %>% 
    pivot_wider(names_from = parameterName, values_from = value_list) 
  
  # add columns if no values were found
  cols_features <- unique(dict_neurology_params$English)
  cols_features <- paste0("CARE_NEU_", cols_features)
  
  for (col in cols_features){
    if(!col %in% colnames(CARE_data_during_LM)){
      CARE_data_during_LM[,col] <- NA_real_
    }
  }
  
  # take last recorded value
  CARE_data_during_LM <- CARE_data_during_LM %>%  
    make_col_numeric_last(cols_features) %>% 
    select(-all_of(cols_features))
  
  # Merge in LM data
  LM_data <- LM_data %>% 
    left_join(CARE_data_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  message(sprintf("CARE neurology features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
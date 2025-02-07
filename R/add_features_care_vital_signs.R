#' Adds features related to vital signs to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_care_vital_signs <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^CARE_VS_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding CARE features VITAL SIGNS...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get CARE VS data
  # ----------------
  
  message("Reading CARE - vital signs data from the database...")
  
  # take mapped attributes from dictionary
  attributes_str <- dict_vital_signs_attributes$Dutch %>% paste0(collapse = "', '")
  attributes_str_CARE <- sprintf(" AND attribuutDefinitieCode IN ('%s')", attributes_str)
  
  query <- "SELECT functioneelDossierNr, waarde, tijdstip, attribuutDefinitieCode 
           FROM careModule_care_tb WHERE 1 = 1"
  query <- paste0(query, attributes_str_CARE)
  
  CARE_vital_signs <- get_data(con, query, admission_ids)
  
  # Get PDMS VS data
  # ----------------
  
  message("Reading PDMS - vital signs data from the database...")
  
  attributes_str_PDMS <- sprintf(" AND parameterName IN ('%s')", attributes_str)
  
  query <- "SELECT date, parameterName, value, unit, functioneelDossierNr 
           FROM pdmsParamNumeric WHERE 1 = 1"
  query <- paste0(query, attributes_str_PDMS)
  
  PDMS_vital_signs <- get_data(con, query, admission_ids)
  
  # MV data
  query <- "SELECT functioneelDossierNr, date, parameterName
           FROM pdmsParamText WHERE 1 = 1"
  query <- paste0(query, attributes_str_PDMS)
  PDMS_MV <- get_data(con, query, admission_ids)
  
  # check if there is any parameter that has more than 1 unit of measurement
  # this check can only be done for PDMS, not for CARE. CARE does not include units. 
  # if there are units problems with CARE, they can only be detected later when doing data exploration
  units_check <- PDMS_vital_signs %>% 
    distinct(parameterName, unit) %>% 
    count(parameterName) %>% 
    filter(n > 1)
  
  if (nrow(units_check) > 0) {
    message("There are parameters recorded in different units")
    message(units_check)
    stop("There are parameters recorded in different units. Adapt the function to correctly account for unit changes.")
  }

  # align with CARE
  PDMS_vital_signs <- PDMS_vital_signs %>% 
    rename(tijdstip = date,
           waarde = value, 
           attribuutDefinitieCode = parameterName) %>% 
    select(-unit)
  
  PDMS_MV <- PDMS_MV %>% 
    rename(tijdstip = date,
           attribuutDefinitieCode = parameterName) %>% 
    mutate(waarde = oxygen_admin_waarde)
  
  # Prepare VS data
  # ---------------
  
  # merge CARE and PDMS (keep in CARE df - maybe it is more memory efficient like this?)
  CARE_vital_signs <- CARE_vital_signs %>% 
    rbind(PDMS_vital_signs) %>% 
    rbind(PDMS_MV)
  
  CARE_vital_signs <- CARE_vital_signs %>% 
    mutate(attribuutDefinitieCode = map_names(attribuutDefinitieCode, dict_vital_signs_attributes))
  
  # check UNKNOWN_VALUE
  number_unknown <- CARE_vital_signs %>% 
    filter(attribuutDefinitieCode == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message(sprintf("There are %s attributes with UNKNOWN_VALUE. These are removed (if any).",  number_unknown))
  
  CARE_vital_signs <- CARE_vital_signs %>% 
    filter(attribuutDefinitieCode != "UNKNOWN_VALUE") 
  
  # keep categorical and continuous variables apart (easier to clean and make numeric when separate)
  cols_features_categ <- unique(dict_vital_signs_attributes$English[dict_vital_signs_attributes$type == "categ"])
  cols_features_categ <- paste0("CARE_VS_", cols_features_categ)
  cols_features_cont <- unique(dict_vital_signs_attributes$English[dict_vital_signs_attributes$type == "cont"])
  cols_features_cont <- paste0("CARE_VS_", cols_features_cont)
  
  # prefix the soon to be columns
  CARE_vital_signs <- CARE_vital_signs %>% 
    mutate(attribuutDefinitieCode = paste0("CARE_VS_", attribuutDefinitieCode)) %>% 
    arrange(functioneelDossierNr, tijdstip)
  
  # Extract features during LM
  # --------------------------
  
  # process data in chunks and keep all chunks in a list
  data_chunks <- list()
  chunk_size <- 10000

  if (length(admission_ids) > chunk_size) {
    chunks <- split_vector(admission_ids, chunk_size)
  } else {
    chunks <- list(admission_ids)
  }
  
  message(sprintf("Processing vital signs data in %s chunks...", length(chunks)))
  
  # break in admission chunks
  for (i in 1:length(chunks)) {
    message(sprintf("Processing chunk %s...", i))
    
    admissions_chunk <- chunks[[i]]
    
    # join with LM_data 
    VS_data_temp <- CARE_vital_signs %>% 
      filter(!is.na(waarde)) %>% 
      filter(functioneelDossierNr %in% admissions_chunk) %>% 
      left_join(LM_data %>% 
                  filter(functioneelDossierNr %in% admissions_chunk) %>% 
                  select(functioneelDossierNr, CAT_catheter_episode, 
                         LM, LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(tijdstip >= LM_start_time, 
             tijdstip <= LM_end_time) %>% 
      select(-c(LM_start_time, LM_end_time))
    
    # keep temporary df in list
    data_chunks[[i]] <- VS_data_temp
    
    # free up memory
    rm(VS_data_temp, admissions_chunk)
    gc()
    
  }
  
  # merge the data chunks together
  CARE_vital_signs <- data_chunks %>% reduce(rbind)
  rm(data_chunks)
  gc()
  
  # process continuous variables - during LM
  # ----------------------------------------
  
  message("Processing vital signs - continuous variables...")
  
  CARE_vital_signs_during_LM_cont <- CARE_vital_signs %>% 
    filter(attribuutDefinitieCode %in% cols_features_cont) %>% 
    mutate(waarde = as.numeric(waarde)) # in the DB it is char, make numeric
  
  # filter temperatures out of range (mainly happens for PDMS - low temperatures registered by devices)
  # filter BP out of range BP_range 
  # filter respiratory_rate our of range
  # filter heart rate out of range
  # filter oxygen saturation range
  CARE_vital_signs_during_LM_cont <- CARE_vital_signs_during_LM_cont %>% 
    filter(!(attribuutDefinitieCode == "CARE_VS_temperature" & 
               (waarde <= temperature_range[[1]] | waarde >= temperature_range[[2]])),
           !(attribuutDefinitieCode %in% c("CARE_VS_systolic_BP", "CARE_VS_diastolic_BP", "CARE_VS_SD_mean_BP") & 
               (waarde <= BP_range[[1]] | waarde >= BP_range[[2]])),
           !(attribuutDefinitieCode == "CARE_VS_respiratory_rate" & 
               (waarde <= respiratory_rate_range[[1]] | waarde >= respiratory_rate_range[[2]])),
           !(attribuutDefinitieCode == "CARE_VS_heart_rate" & 
               (waarde <= heart_rate_range[[1]] | waarde >= heart_rate_range[[2]])),
           !(attribuutDefinitieCode == "CARE_VS_oxygen_saturation" & 
               (waarde <= oxygen_saturation_range[[1]] | waarde >= oxygen_saturation_range[[2]])),
           !(attribuutDefinitieCode == "CARE_VS_CVP" & 
               (waarde <= oxygen_saturation_range[[1]] | waarde >= oxygen_saturation_range[[2]])),)
  
  CARE_vital_signs_during_LM_cont <- CARE_vital_signs_during_LM_cont %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, attribuutDefinitieCode) %>% 
    arrange(tijdstip) %>% 
    mutate(value_list = list(waarde)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, attribuutDefinitieCode, value_list) %>% 
    distinct()

  CARE_vital_signs_during_LM_cont <- CARE_vital_signs_during_LM_cont %>% 
    pivot_wider(names_from = attribuutDefinitieCode, values_from = value_list) 
  
  # add columns if no values were found
  for (col in cols_features_cont){
    if(!col %in% colnames(CARE_vital_signs_during_LM_cont)){
      CARE_vital_signs_during_LM_cont[,col] <- NA_real_
    }
  }
  
  # extract based on logic: last, mean, max.. 
  # take max temperature (to filter the low temperatures in PDMS) and last for the other columns
  cols_num_max <- c("CARE_VS_temperature", "CARE_VS_heart_rate")
  #cols_num_min <- c("CARE_VS_heart_rate") # probably not clinically interesting
  cols_num_last <- cols_features_cont[!cols_features_cont %in% cols_num_max]
  
  CARE_vital_signs_during_LM_cont <- CARE_vital_signs_during_LM_cont %>%  
    make_col_max(cols_num_max) %>% 
    #make_col_min(cols_num_min) %>% 
    make_col_numeric_last(cols_num_last) %>% 
    select(-all_of(cols_features_cont))
  
  # Merge in LM data
  LM_data <- LM_data %>% 
    left_join(CARE_vital_signs_during_LM_cont, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # add indicator that the CVP was measured 
  # this is a catheter manipulation that poses CLABSI risk (feedback from Christel)
  LM_data <- LM_data %>% 
    mutate(CARE_VS_CVP_measured = if_else(!is.na(CARE_VS_CVP_last), 1, 0))
  
  # keep indicator for MV if PEEP or FiO2 are present
  LM_data <- LM_data %>% 
    mutate(CARE_VS_MV = if_else(!is.na(CARE_VS_PEEP_last) | !is.na(CARE_VS_FiO2_last), 1, 0)) %>% 
    select(-c(CARE_VS_PEEP_last, CARE_VS_FiO2_last))
  
  # process categorical variables - during LM
  # -----------------------------------------
  
  message("Processing vital signs - categorical variables...")
  
  CARE_vital_signs_during_LM_categ <- CARE_vital_signs %>% 
    filter(attribuutDefinitieCode %in% cols_features_categ) %>% 
    filter(waarde == oxygen_admin_waarde) %>% # in CARE we have zonder / met zuurstoftoediening 
    mutate(waarde = 1) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, attribuutDefinitieCode, waarde)
  
  CARE_vital_signs_during_LM_categ <- CARE_vital_signs_during_LM_categ %>% 
    pivot_wider(names_from = attribuutDefinitieCode, values_from = waarde, 
                values_fill = 0, values_fn = function(x) 1) 
  
  # add columns if no values were found
  for (col in cols_features_categ){
    if(!col %in% colnames(CARE_vital_signs_during_LM_categ)){
      CARE_vital_signs_during_LM_categ[,col] <- NA_character_
    }
  }
  
  # Merge in LM data
  LM_data <- LM_data %>% 
    left_join(CARE_vital_signs_during_LM_categ, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    # if no MV or oxygen admin recorded, make 0
    mutate(CARE_VS_breathing_aid = if_else(is.na(CARE_VS_breathing_aid), 0, CARE_VS_breathing_aid)) 
  
  message(sprintf("Vital signs features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)

}

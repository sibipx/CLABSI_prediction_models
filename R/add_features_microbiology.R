#' Adds features related to microbiology lab results to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_microbiology <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^MB_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding MICROBIOLOGY lab results features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get microbiology lab results data
  # ---------------------------------
  
  message("Reading microbiology data from the database...")
  
  query <- "SELECT functioneelDossierNr, tijdstip, validatietijdstip, staalsoort,  extensie, resultaat 
            FROM lwsPositieveKweken WHERE 1 = 1"
  lab_MB <- get_data(con, query, admission_ids)
  
  message(sprintf("Microbiology lab data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # rename columns to friendly names
  lab_MB <- lab_MB %>% 
    rename(date_foreseen = tijdstip,
           date_validated = validatietijdstip,
           sample_type = staalsoort,
           germ = resultaat) %>% 
    select(functioneelDossierNr, date_foreseen, date_validated, sample_type, extensie, germ)
  
  # fix some germ names 
  lab_MB <- lab_MB %>% 
    mutate(germ = if_else(germ == "Candida guilliermondii", "Candida guillermondii", germ))
  
  # filter invalid / cancelled samples
  lab_MB <- lab_MB %>%
    filter(!germ %in% MB_invalid_results) 
  
  # infection site
  # --------------
  
  message("Adding feature - infection site...")
  
  lab_MB_sample <- lab_MB %>%
    mutate(sample_type = map_names(sample_type, dict_MB_sample_type)) %>% 
    filter(sample_type != "UNKNOWN_VALUE") %>% 
    distinct(functioneelDossierNr, date_validated, sample_type) %>% 
    mutate(MB_other_infection_than_BSI = as.numeric(sample_type != "blood"))
  
  # infection site in the LM timeframe
  lab_MB_sample_during_LM <- lab_MB_sample %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_validated >= LM_start_time, 
           date_validated <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, sample_type) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MB_infection = list(sample_type)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MB_infection)
  
  lab_MB_sample_during_LM <- lab_MB_sample_during_LM %>% 
    make_col_binary_all(cols = c("MB_infection")) %>% 
    select(-MB_infection)
  
  LM_data <- LM_data %>% 
    left_join(lab_MB_sample_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("MB_infection_")), ~ if_else(is.na(.), 0, .)) # make 0 when not recorded
  
  # any other infection site than blood in the LM timeframe
  lab_MB_sample_during_LM_not_blood <- lab_MB_sample %>% 
    filter(MB_other_infection_than_BSI == 1) %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_validated >= LM_start_time, 
           date_validated <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MB_other_infection_than_BSI)
  
  LM_data <- LM_data %>% 
    left_join(lab_MB_sample_during_LM_not_blood, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MB_other_infection_than_BSI = if_else(is.na(MB_other_infection_than_BSI), 0, MB_other_infection_than_BSI)) # make 0 when not recorded
  
  # infection site in outcome time window (17 days)
  lab_MB_sample_during_window <- lab_MB_sample %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_validated >= LM_end_time - days(time_window_secondary[1]), 
           date_validated <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, sample_type) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MB_infection_time_window = list(sample_type)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MB_infection_time_window)
  
  lab_MB_sample_during_window <- lab_MB_sample_during_window %>% 
    make_col_binary_all(cols = c("MB_infection_time_window")) %>% 
    select(-MB_infection_time_window)
  
  LM_data <- LM_data %>% 
    left_join(lab_MB_sample_during_window, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("MB_infection_time_window_")), ~ if_else(is.na(.), 0, .)) # make 0 when not recorded

  # any other infection site than blood in outcome time window (17 days)
  lab_MB_sample_during_window_not_blood <- lab_MB_sample %>% 
    filter(MB_other_infection_than_BSI == 1) %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_validated >= LM_end_time - days(time_window_secondary[1]), 
           date_validated <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MB_other_infection_than_BSI) %>% 
    rename(MB_other_infection_than_BSI_during_window = MB_other_infection_than_BSI)
  
  LM_data <- LM_data %>% 
    left_join(lab_MB_sample_during_window_not_blood, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MB_other_infection_than_BSI_during_window = if_else(is.na(MB_other_infection_than_BSI_during_window), 
                                                               0, MB_other_infection_than_BSI_during_window)) # make 0 when not recorded
  # germ
  # ----
  
  message("Adding feature - contaminant germs...")
  
  # contaminant germs
  list_germs_contaminants <- table_germs %>% 
    filter(`Huidcontaminant?` == "x") %>% 
    pull(naam)
  
  lab_MB_contaminants <- lab_MB %>% 
    mutate(sample_type = map_names(sample_type, dict_MB_sample_type)) %>% 
    mutate(is_germ_contaminant = as.numeric(sample_type == "blood" & 
                                              germ %in% list_germs_contaminants)) %>% 
    filter(is_germ_contaminant == 1) %>% 
    select(functioneelDossierNr, date_validated, is_germ_contaminant)
  
  lab_MB_contaminants_during_LM <- lab_MB_contaminants %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_validated >= LM_start_time, 
           date_validated <= LM_end_time) %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, is_germ_contaminant) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MB_number_contaminants_in_blood = sum(is_germ_contaminant)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM,MB_number_contaminants_in_blood)
    
  LM_data <- LM_data %>% 
    left_join(lab_MB_contaminants_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MB_number_contaminants_in_blood = if_else(is.na(MB_number_contaminants_in_blood), 
                                                               0, MB_number_contaminants_in_blood)) # make 0 when not recorded
  
  message(sprintf("Microbiology features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}

  
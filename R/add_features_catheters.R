#' Adds features related to catheters to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_catheters <- function(LM_data, con){
  
  start_time <- Sys.time()
  message("Adding cathetes features...")
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[col_names %in% c("CAT_nr_bandage_obersations",                                   
                                          "CAT_bandage_observation_binary_all_Normal",                   
                                          "CAT_bandage_observation_binary_all_Bloody_or_Moist",           
                                          "CAT_bandage_observation_binary_all_Red",                       
                                          "CAT_bandage_observation_binary_all_Other_Hema_Pus_Loose_Necro",
                                          "CAT_days_since_last_bandage_obs",                             
                                          "CAT_bandage_type_binary_last_polyurethane",                    
                                          "CAT_bandage_type_binary_last_gauze",                          
                                          "CAT_bandage_change",                                          
                                          "CAT_days_since_last_bandage_changed",                          
                                          "CAT_tube_change",                                              
                                          "CAT_days_since_last_tube_change",                              
                                          "CAT_needle_length_max",                                        
                                          "CAT_result_aspiration_binary_all_normal",                      
                                          "CAT_result_aspiration_binary_all_impossible",                  
                                          "CAT_result_aspiration_binary_all_difficult",                   
                                          "CAT_result_infusion_binary_all_normal",                        
                                          "CAT_result_infusion_binary_all_difficult",                     
                                          "CAT_result_infusion_binary_all_impossible",                    
                                          "CAT_lumens_flushed",                                           
                                          "CAT_lumens_locked")]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # for trigger mode dataframe, keep start and end of catheter episodes
  if (landmark_mode == "trigger") {
    LM_data_episodes <- LM_data %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode)
  }
  
  # get data
  # --------
  
  # PDMS bandage
  message("Reading PDMS insert points from the database...")
  query <- "SELECT * FROM pdmsInsertionPoints WHERE 1 = 1"
  PDMS_insertion_points <- get_data(con, query, admission_ids)
  
  # PDMS change tubes
  message("Reading PDMS change tubes from the database...")
  query <- "SELECT * FROM pdmsCathetersChangeTubes WHERE 1 = 1"
  PDMS_change_tubes <- get_data(con, query, admission_ids)
  
  # CARE CINAS (infusion / aspiration results)
  message("Reading CARE infusion / aspiration results from the database...")
  list_attributes_CINAS <- dict_CINAS_attributes$orig
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, tijdstip, waarde, attribuutDefinitieCode, zorgDefinitieCode
  FROM careModule_catheterCare_tb
  WHERE attribuutDefinitieCode in ('%s')", paste(list_attributes_CINAS, collapse = "', '"))
  CARE_results_CINAS <- get_data(con, query, admission_ids)
  
  # CARE observations
  message("Reading CARE observations from the database...")
  query <- "SELECT DISTINCT functioneelDossierNr, tijdstip, rEenheid, zorgNr, waarde, attribuutDefinitieCode, zorgDefinitieCode
  FROM careModule_catheterCare_tb
  WHERE attribuutDefinitieCode in ('TPtpT', 'TPobGVKcvkPICCtk', 'TPtpV', 'TPtpBZ',
        'TPobGVKcvkPICCvi','TPobPKVva', 'TPobGVKcvkPICCip','TPobPKVap', 'TPtpG', 'TPtpSH')"
  CARE_catheters <- get_data(con, query, admission_ids)
  
  # prepare CARE observations
  message("Preparing CARE observations...")
  CARE_catheters_wide <- CARE_catheters %>% 
    mutate(attribuutDefinitieCode = map_names(attribuutDefinitieCode, dict = dict_care_attributes)) %>% 
    pivot_wider(names_from = attribuutDefinitieCode, values_from = waarde, values_fn = list) 
  
  # add columns that don't exist for unnesting later
  # this happens mostly when running on one (or few) admission IDs for which eg: vandage observation does not exist
  cols_unnest <- c("catheter_type", "bandage_change", 
                   "tube_change", 
                   "bandage_obs", "insert_point_obs", "needle_length", "flush_lock")
  
  for (col in cols_unnest){
    if(!col %in% colnames(CARE_catheters_wide)){
      CARE_catheters_wide[,col] <- NA_character_
    }
  }
  
  if (!nrow(CARE_catheters_wide) == 0) {
    CARE_catheters_wide <- CARE_catheters_wide %>% 
      unnest(cols = c(catheter_type, bandage_change, tube_change, 
                      bandage_obs, needle_length), 
             keep_empty = TRUE) %>% 
      unnest(cols = c(insert_point_obs), keep_empty = TRUE) %>% # unnest this separately to avoid Error: Incompatible lengths:
      unnest(cols = c(flush_lock), keep_empty = TRUE) %>% 
      arrange(functioneelDossierNr, tijdstip) %>% 
      distinct() 
  }
  
  # fill in Port-a-cath (like in init_catheters.R) and make indicator for central line
  CARE_catheters_wide <- CARE_catheters_wide %>% 
    mutate(catheter_type = if_else(zorgDefinitieCode == bandage_code_port_a_cath, 
                                   catheter_type_port_a_cath, catheter_type)) %>% 
    mutate(is_central_line = if_else(catheter_type %in% 
                                       dict_catheters$catheter_orig[dict_catheters$is_central], 1, 0)) 
  
  # add feature - bandage or insert observation
  # -------------------------------------------
  
  message("Adding features - bandage or insert observation...")
  
  # keep all bandage information (including orphaned = no linked catheter type) apart 
  CARE_catheter_observations <- CARE_catheters_wide %>% 
    filter(!is.na(bandage_obs) | !is.na(insert_point_obs)) %>% 
    distinct(functioneelDossierNr, tijdstip, bandage_obs, insert_point_obs) %>% 
    mutate(bandage_obs = map_names(bandage_obs, dict_bandage_obs),
           insert_point_obs = map_names(insert_point_obs, dict_bandage_obs)) %>% 
    pivot_longer(cols = c(bandage_obs, insert_point_obs), names_to = "type", values_to = "bandage_obs") %>% 
    filter(!is.na(bandage_obs)) %>% 
    select(functioneelDossierNr, tijdstip, bandage_obs) %>% 
    arrange(functioneelDossierNr, tijdstip, bandage_obs)
  
  PDMS_catheter_observations <- PDMS_insertion_points %>% 
    mutate(Katheter = map_names(Katheter, dict_catheters_pdms)) %>% # map to names in catheters 
    filter(Katheter %in% dict_catheters$catheter_orig[dict_catheters$is_central]) %>% # filter only central lines
    rename(tijdstip = DateTime,
           bandage_obs = Status) %>% 
    select(functioneelDossierNr, tijdstip, bandage_obs) %>% 
    mutate(bandage_obs = map_names(bandage_obs, dict_bandage_obs)) %>% 
    arrange(functioneelDossierNr, tijdstip, bandage_obs)
  
  catheter_observations <- CARE_catheter_observations %>% 
    rbind(PDMS_catheter_observations)
  
  if (use_CARE_dialisys_catheters){
    
    query <- sprintf("SELECT functioneelDossierNr, tijdstip, waarde 
                     FROM careModule_care_tb WHERE attribuutDefinitieCode IN ('%s')", 
                     paste0(attr_bandage_obs_dialysis, collapse = "','"))
    
    dialysis_catheter_observations <- get_data(con, query, admission_ids)
    
    dialysis_catheter_observations <- dialysis_catheter_observations %>% 
      rename(bandage_obs = waarde) %>% 
      mutate(bandage_obs = map_names(bandage_obs, dict_bandage_obs)) %>% 
      arrange(functioneelDossierNr, tijdstip, bandage_obs)
    
    catheter_observations <- catheter_observations %>% 
      rbind(dialysis_catheter_observations)
    
  }
  
  number_unknown_value <- catheter_observations %>% 
    filter(bandage_obs == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message(sprintf("There are %s bandage observations (%s) with no mapping. These will be removed (if any)", 
                  number_unknown_value, 
                  make_percent(number_unknown_value/nrow(catheter_observations))))
  
  catheter_observations <- catheter_observations %>% 
    filter(bandage_obs != "UNKNOWN_VALUE")
  
  if (landmark_mode == "trigger") {
    
    catheter_observations_in_episode <- catheter_observations %>% 
      filter(functioneelDossierNr %in% LM_data_episodes$functioneelDossierNr) %>% 
      rename(time = tijdstip) %>% 
      left_join(LM_data_episodes, by = "functioneelDossierNr", multiple = "all") %>% 
      filter(time >= CAT_start_time_episode - days(1), # leave 1 day for LM 0
             time <= CAT_end_time_episode) %>% 
      rename(CAT_bandage_observation = bandage_obs) %>% 
      make_col_binary_last(c("CAT_bandage_observation")) %>% 
      select(-CAT_bandage_observation)
    
    LM_data <- LM_data %>% 
      bind_rows(catheter_observations_in_episode)
    
  } else {
    
    catheter_observations <- catheter_observations %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(tijdstip >= LM_start_time, 
             tijdstip <= LM_end_time) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
      arrange(tijdstip) %>% 
      mutate(CAT_bandage_observation = list(bandage_obs),
             CAT_nr_bandage_obersations = lengths(CAT_bandage_observation)) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, 
               CAT_bandage_observation, CAT_nr_bandage_obersations)
    
    # make binary variables
    catheter_observations <- catheter_observations %>% 
      make_col_binary_all(cols = c("CAT_bandage_observation")) %>% 
      select(-CAT_bandage_observation)
    
    LM_data <- LM_data %>% 
      left_join(catheter_observations, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(CAT_nr_bandage_obersations = if_else(is.na(CAT_nr_bandage_obersations), 0L, 
                                                  CAT_nr_bandage_obersations)) # make 0 when not recorded
    
    # create "days since last observation" - it's a type of missing data indicator, but I create it here
    LM_data <- LM_data %>% 
      mutate(bandage_observed = as.integer(CAT_nr_bandage_obersations != 0),
             # at catheter placement we can consider that the bandage was observed
             bandage_observed = if_else(LM == 0L, 1L, bandage_observed)) %>% 
      arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>%
      group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
      arrange(LM) %>% 
      mutate(last_observation = if_else(bandage_observed == 1L, LM, NA_integer_)) %>% # mark last observation
      fill(last_observation) %>% # and carry it forward
      mutate(CAT_days_since_last_bandage_obs = LM - last_observation) %>% # then calculate difference
      fill(CAT_days_since_last_bandage_obs) %>% 
      ungroup() %>% 
      select(-c(bandage_observed, last_observation)) 
  }
  
  
  # add feature - bandage type & bandage change
  # -------------------------------------------
  
  message("Adding features - bandage type & bandage change...")
  
  # CARE bandage change
  CARE_bandage_change <- CARE_catheters_wide %>% 
    filter(is_central_line == 1,
           !is.na(bandage_change)) %>% 
    select(functioneelDossierNr, tijdstip, catheter_type, bandage_change) %>% 
    mutate(bandage_change = map_names(bandage_change, dict_bandage_type)) %>% 
    distinct(functioneelDossierNr, tijdstip, bandage_change)
  # Note: In CARE bandage type is registered only when a bandage change takes place. In PDMS it is different.
  
  # PDMS bandage type
  PDMS_bandage_type <- PDMS_insertion_points %>% 
    select(functioneelDossierNr, Katheter, Locatie, Zorg, DateTime, DateringVerband) %>% 
    mutate(Katheter = map_names(Katheter, dict_catheters_pdms)) %>% # map to names in catheters 
    filter(Katheter %in% dict_catheters$catheter_orig[dict_catheters$is_central]) %>% # filter only central lines
    rename(bandage_change = Zorg,
           tijdstip = DateTime) %>% 
    mutate(bandage_change = map_names(bandage_change, dict_bandage_type)) %>% 
    distinct(functioneelDossierNr, tijdstip, bandage_change)
  
  # PDMS bandage change
  PDMS_bandage_change <- PDMS_insertion_points %>% 
    select(functioneelDossierNr, Katheter, Locatie, Zorg, DateTime, DateringVerband) %>% 
    mutate(Katheter = map_names(Katheter, dict_catheters_pdms)) %>% # map tp names in catheters 
    filter(Katheter %in% dict_catheters$catheter_orig[dict_catheters$is_central]) %>% # filter only central lines
    group_by(functioneelDossierNr, Katheter, Locatie) %>% 
    arrange(DateTime) %>% 
    # make first row bandage change (we consider the bandage is changed when fist observation in ICU is made)
    mutate(is_bandage_change = if_else(row_number() == 1 , 1, 0),
           DateringVerband = if_else(row_number() == 1, as.Date(DateTime), DateringVerband)) %>% 
    fill(DateringVerband, .direction = "down") %>% # when date is not registered we consider no change
    # if DateringVerband changes, consider a bandage change
    mutate(is_bandage_change = if_else(row_number() != 1 & DateringVerband != lag(DateringVerband), 1, is_bandage_change)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, DateTime, is_bandage_change)
  
  bandage_type <- CARE_bandage_change %>% 
    mutate(source = "CARE") %>% 
    rbind(PDMS_bandage_type %>% mutate(source = "PDMS")) %>% 
    arrange(functioneelDossierNr, tijdstip, bandage_change)
  
  # check UNKNOWN_VALUE after merging and remove UNKNOWN_VALUE 
  number_bandage_type_location <- bandage_type %>% 
    filter(bandage_change == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message(sprintf("There are %s (%s) bandage_type without mapping (unknown). These are removed (if any).", 
                  number_bandage_type_location,
                  make_percent(number_bandage_type_location/nrow(bandage_type))))
  
  bandage_type <- bandage_type %>% 
    filter(bandage_change != "UNKNOWN_VALUE")
  
  # in CARE the bandage type is recorded only when the bandage is changed.
  # acc. to the zorgbundle, polyurethane bandage is changed at least every 7 days and gauze bandage at least every 2 days
  # do 1 LM carried forward for gauze bandage type and 6 days carried forward  for polyurethane - for CARE only
  # (it is easier to add rows here, only for CARE, than to impute in LM_data)
  bandage_type <- bandage_type %>% 
    group_by(functioneelDossierNr) %>% 
    arrange(tijdstip, source) %>% 
    mutate(till_date_gauze = pmin(tijdstip + hours(24), lead(tijdstip), na.rm = TRUE), # 1 day forward
           till_date_poly = pmin(tijdstip + hours(6*24), lead(tijdstip), na.rm = TRUE)) %>% # 6 days forward
    ungroup() %>% 
    mutate(values_to_add = if_else(bandage_change == "gauze", 
                                   map2(tijdstip, till_date_gauze, function(x, y) seq(x, y, by = "day")),
                                   map2(tijdstip, till_date_poly, function(x, y) seq(x, y, by = "day")))) %>% 
    mutate(values_to_add = if_else(source == "PDMS", map2(tijdstip, tijdstip, function(x, y) seq(x, y, by = "day")), values_to_add)) %>% 
    unnest(cols = "values_to_add") %>% 
    mutate(tijdstip = values_to_add) %>% 
    distinct(functioneelDossierNr, tijdstip, bandage_change, source)
  
  if (landmark_mode == "trigger") {
    
    bandage_type <- bandage_type %>% 
      rename(time = tijdstip) %>% 
      filter(functioneelDossierNr %in% LM_data_episodes$functioneelDossierNr) %>% 
      left_join(LM_data_episodes, by = "functioneelDossierNr", multiple = "all") %>% 
      filter(time >= CAT_start_time_episode - days(1), # leave 1 day for LM 0
             time <= CAT_end_time_episode) %>% 
      rename(CAT_bandage_change = bandage_change) %>% 
      make_col_binary_last(c("CAT_bandage_change")) %>% 
      select(-CAT_bandage_change)
    
    LM_data <- LM_data %>% 
      bind_rows(bandage_type)
    
  } else {
    
    # merge bandage type in LM dataframe 
    bandage_type_during_LM <- bandage_type %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(tijdstip >= LM_start_time, 
             tijdstip <= LM_end_time) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
               CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time) %>% 
      arrange(tijdstip) %>% 
      mutate(CAT_bandage_type = list(bandage_change)) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, CAT_bandage_type) 
    
    # make binary variables
    bandage_type_during_LM <- bandage_type_during_LM %>% 
      make_col_binary_last(cols = c("CAT_bandage_type")) %>% 
      select(-CAT_bandage_type)
    
    LM_data <- LM_data %>% 
      left_join(bandage_type_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
    
    # merge bandage change in LM dataframe
    bandage_change <- CARE_bandage_change %>% 
      mutate(is_bandage_change = 1L) %>% 
      select(-bandage_change) %>% 
      rbind(PDMS_bandage_change %>% rename(tijdstip = DateTime)) %>% 
      arrange(functioneelDossierNr, tijdstip)
    
    bandage_change_during_LM <- bandage_change %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(tijdstip >= LM_start_time, 
             tijdstip <= LM_end_time) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
               CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time) %>% 
      arrange(tijdstip) %>% 
      mutate(CAT_bandage_change = as.integer(max(is_bandage_change))) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, CAT_bandage_change) 
    
    LM_data <- LM_data %>% 
      left_join(bandage_change_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(CAT_bandage_change = if_else(is.na(CAT_bandage_change), 0L, CAT_bandage_change),
             CAT_bandage_change = if_else(LM == 0L, 1L, CAT_bandage_change)) # at catheter placement we can consider that the bandage was place / = changed
    
    # calculate time since last bandage change
    LM_data <- LM_data %>% 
      arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>%
      group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
      arrange(LM) %>% 
      mutate(last_bandage_change = if_else(CAT_bandage_change == 1L, LM, NA_integer_)) %>% # mark last observation
      fill(last_bandage_change) %>% # and carry it forward
      mutate(CAT_days_since_last_bandage_changed = LM - last_bandage_change) %>% # then calculate difference
      fill(CAT_days_since_last_bandage_changed) %>% 
      ungroup() %>% 
      select(-last_bandage_change)
    
  }
  
  # add feature - tube change
  # -------------------------
  
  message("Adding features - tube change...")
  
  # merge CARE and PDMS
  # filter for central_line in CARE, but not in PDMS
  # answer from Sara: normally only for central lines, catheter type can not be linked directly. 
  # But when e.g. IV leidingen (vethoudende infusen) has a date, this means that all central lines are changed.
  tube_change <- CARE_catheters_wide %>% 
    filter(is_central_line == 1) %>% 
    select(functioneelDossierNr, tijdstip, tube_change) %>% 
    filter(!is.na(tube_change),
           tube_change %in% dict_tube_change$orig) %>% 
    distinct() %>% 
    rbind(PDMS_change_tubes %>% 
            select(functioneelDossierNr, WisselDatum, Wissel) %>% 
            rename(tijdstip = WisselDatum,
                   tube_change = Wissel) %>% 
            distinct()) %>% 
    arrange(functioneelDossierNr, tijdstip)
  
  # map tube_change
  tube_change <- tube_change %>% 
    mutate(tube_change = map_names(tube_change, dict_tube_change)) %>% 
    mutate(tube_change = 1) # there is only 1 value now in the dict (no need to make binary)
  
  if (landmark_mode == "trigger") {
    
    tube_change <- tube_change %>% 
      rename(time = tijdstip) %>% 
      filter(functioneelDossierNr %in% LM_data_episodes$functioneelDossierNr) %>% 
      left_join(LM_data_episodes, by = "functioneelDossierNr", multiple = "all") %>% 
      filter(time >= CAT_start_time_episode - days(1), # leave 1 day for LM 0
             time <= CAT_end_time_episode) %>% 
      rename(CAT_tube_change = tube_change) 
    
    LM_data <- LM_data %>% 
      bind_rows(bandage_type)
    
  } else {
    
    # merge tube_change in LM dataframe
    tube_change <- tube_change %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(tijdstip >= LM_start_time, 
             tijdstip <= LM_end_time) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
               LM_start_time, LM_end_time) %>% 
      arrange(tijdstip) %>% 
      mutate(CAT_tube_change = as.integer(any(tube_change == 1))) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, CAT_tube_change)
    
    LM_data <- LM_data %>% 
      left_join(tube_change, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(CAT_tube_change = if_else(is.na(CAT_tube_change), 0L, CAT_tube_change)) 
    # When tube change is not registered we assume that there is no tube change
    
    # create "days since last tube change" - it's a type of missing data indicator, but I create it here
    # as it is part of the "zorg bundel" (catheter care protocol)
    LM_data <- LM_data %>% 
      mutate(CAT_tube_change = if_else(LM == 0L, 1L, CAT_tube_change)) %>% # we can consider the tube placed as a tube change
      arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>%
      group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
      arrange(LM) %>% 
      mutate(last_observation = if_else(CAT_tube_change == 1L, LM, NA_integer_)) %>% # mark last tube change
      fill(last_observation) %>% # and carry it forward
      mutate(CAT_days_since_last_tube_change = LM - last_observation) %>% # then calculate difference
      fill(CAT_days_since_last_tube_change) %>% 
      ungroup() %>% 
      select(-last_observation)
    
  }
  
  # add feature - needle length
  # ---------------------------
  
  message("Adding features - needle length...")
  
  # needle length is not registered in PDMS - we use only CARE values
  CARE_needle_length <- CARE_catheters_wide %>% 
    filter(is_central_line == 1,
           !is.na(needle_length)) %>% 
    distinct(functioneelDossierNr, tijdstip, catheter_type, needle_length) 
  
  # make needle length numeric 
  CARE_needle_length <- CARE_needle_length %>% 
    mutate(needle_length = str_trim(str_replace(needle_length, "mm", "")),
           needle_length = as.numeric(needle_length))
  
  # merge needle length in LM dataframe
  CARE_needle_length_during_LM <- CARE_needle_length %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= LM_start_time, 
           tijdstip <= LM_end_time) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
             CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time) %>% 
    arrange(tijdstip) %>% 
    mutate(CAT_needle_length = list(needle_length)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CAT_needle_length)
  
  # take maximum needle length if more values are registered
  # a patient can have more than 1 catheter with more needles
  # the longest needle should pose the highest risk (acc. to Christel)
  CARE_needle_length_during_LM <- CARE_needle_length_during_LM %>% 
    make_col_max(cols = c("CAT_needle_length")) %>% 
    select(-CAT_needle_length)
  
  LM_data <- LM_data %>% 
    left_join(CARE_needle_length_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # add feature - catheter infusion / aspiration result
  # ---------------------------------------------------
  
  # this is only available in CARE, missing in PDMS
  CARE_results_CINAS <- CARE_results_CINAS %>% 
    mutate(CAT_result = map_names(attribuutDefinitieCode, dict_CINAS_attributes)) %>% 
    # clean observations
    mutate(waarde = if_else(str_detect(tolower(waarde), "moeilijk"), "Moeilijk", waarde),
           waarde = if_else(str_detect(tolower(waarde), "normaal"), "Normaal", waarde),
           waarde = if_else(str_detect(tolower(waarde), "onmogelijk"), "Onmogelijk", waarde)) %>% 
    # map to English
    mutate(waarde = map_names(waarde, dict_CINAS_values)) %>% 
    distinct(functioneelDossierNr, tijdstip, CAT_result, waarde)
  
  if (use_CARE_dialisys_catheters){
    query <- sprintf("SELECT functioneelDossierNr, tijdstip, waarde 
                     FROM careModule_care_tb WHERE attribuutDefinitieCode IN ('%s')", 
                     paste0(attr_aspiration_dialysis, collapse = "','"))
    
    dialysis_catheters_aspiration <- get_data(con, query, admission_ids)
    
    # map names
    dialysis_catheters_aspiration <- dialysis_catheters_aspiration %>% 
      mutate(waarde = map_names(waarde, dict_CINAS_values),
             CAT_result = "aspiration") %>% # only aspiration available for CARE dialysis 
      distinct(functioneelDossierNr, tijdstip, waarde, CAT_result) 
    
    CARE_results_CINAS <- CARE_results_CINAS %>% 
      rbind(dialysis_catheters_aspiration)
    
  }
  
  # check UNKNOWN_VALUE and remove UNKNOWN_VALUE 
  number_results_UNKNOWN_VALUE <- CARE_results_CINAS %>% 
    filter(waarde == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message(sprintf("There are %s (%s) infusion / aspiration results without mapping (unknown). These are removed (if any).", 
                  number_results_UNKNOWN_VALUE,
                  make_percent(number_results_UNKNOWN_VALUE/nrow(CARE_results_CINAS))))
  
  CARE_results_CINAS <- CARE_results_CINAS %>% 
    filter(waarde != "UNKNOWN_VALUE")
  
  # spread
  CARE_results_CINAS <- CARE_results_CINAS %>% 
    pivot_wider(names_from = CAT_result, names_prefix = "CAT_result_", 
                values_from = waarde, values_fn = list) %>% 
    unnest(cols = c(CAT_result_aspiration), keep_empty = TRUE) %>% 
    unnest(cols = c(CAT_result_infusion), keep_empty = TRUE) 
  
  CARE_results_CINAS_during_LM <- CARE_results_CINAS %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= LM_start_time, 
           tijdstip <= LM_end_time) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    arrange(tijdstip) %>% 
    mutate(CAT_result_aspiration = list(CAT_result_aspiration),
           CAT_result_infusion = list(CAT_result_infusion)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, 
             CAT_result_aspiration, CAT_result_infusion)
  
  # make binary variables
  CARE_results_CINAS_during_LM <- CARE_results_CINAS_during_LM %>% 
    make_col_binary_all(cols = c("CAT_result_aspiration", "CAT_result_infusion")) %>% 
    select(-c(CAT_result_aspiration, CAT_result_infusion))
  
  LM_data <- LM_data %>% 
    left_join(CARE_results_CINAS_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) 
  
  # add feature - flush / lock
  # --------------------------
  
  message("Adding features - flush / lock ...")
  
  # keep flush and lock separately
  CARE_flush <- CARE_catheters_wide %>% 
    filter(is_central_line == 1,
           !is.na(flush_lock),
           map_names(flush_lock, dict_flush_lock) == "flush") %>% 
    distinct(functioneelDossierNr, tijdstip, catheter_type, flush_lock) 
  
#  CARE_lock <- CARE_catheters_wide %>% 
#    filter(is_central_line == 1,
#           !is.na(flush_lock),
#           map_names(flush_lock, dict_flush_lock) == "lock") %>% 
#    distinct(functioneelDossierNr, tijdstip, catheter_type, flush_lock) 
  
  if (use_CARE_dialisys_catheters){
    
    query <- sprintf("SELECT functioneelDossierNr, tijdstip, waarde 
                     FROM careModule_care_tb WHERE attribuutDefinitieCode IN ('%s')", 
                     paste0(attr_flush_lock_dialysis, collapse = "','"))
    
    dialysis_catheters_flush_lock <- get_data(con, query, admission_ids)
    
    # keep flush and lock separately
    dialysis_flush <- dialysis_catheters_flush_lock %>% 
      rename(flush_lock = waarde) %>% 
      filter(map_names(flush_lock, dict_flush_lock) == "flush") %>% 
      mutate(catheter_type = "Dialysis CVC") %>% 
      distinct(functioneelDossierNr, tijdstip, catheter_type, flush_lock) 
    
    CARE_flush <- CARE_flush %>% 
      rbind(dialysis_flush)
    
#    dialysis_lock <- dialysis_catheters_flush_lock %>% 
#      rename(flush_lock = waarde) %>% 
#      filter(map_names(flush_lock, dict_flush_lock) == "lock") %>% 
#      mutate(catheter_type = "Dialysis CVC") %>% 
#      distinct(functioneelDossierNr, tijdstip, catheter_type, flush_lock) 
#    
#    CARE_lock <- CARE_lock %>% 
#      rbind(dialysis_lock)
    
  }
  
  # keep values in the time interval between LMs for flush
  CARE_flush_during_LM <- CARE_flush %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= LM_start_time, 
           tijdstip <= LM_end_time) %>% 
    # get distinct flushes per lumen (if lumen 1 is flushed twice it will be counted as 1)
    # (if lumen 1 and lumen 2 are flushed the count will be 2)
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, catheter_type, flush_lock) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    count(name = "CAT_lumens_flushed") %>% 
    ungroup() 
  
  # keep values in the time interval between LMs for lock
#  CARE_lock_during_LM <- CARE_lock %>% 
#    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
#                                 LM_start_time, LM_end_time),
#              by = "functioneelDossierNr", multiple = "all") %>% 
#    filter(tijdstip >= LM_start_time, 
#           tijdstip <= LM_end_time) %>% 
#    # get distinct locks per lumen (if lumen 1 is flushed twice it will be counted as 1)
#    # (if lumen 1 and lumen 2 are flushed the count will be 2)
#    distinct(functioneelDossierNr, CAT_catheter_episode, LM, catheter_type, flush_lock) %>% 
#    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
#    count(name = "CAT_lumens_locked") %>% 
#    ungroup() 
  
  # locking and flushing are unreliable in PDMS (see feedback from Kim)
  # will be missing in ICU
  
  # add flush and lock to LM data
  LM_data <- LM_data %>% 
    left_join(CARE_flush_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) #%>% 
    #left_join(CARE_lock_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))

  # return LM data
  # --------------
  
  message(sprintf("Finished data processing in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}

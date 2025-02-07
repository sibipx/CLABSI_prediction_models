#' Initializes landmark dataframe and extracts catheter features
#'

#'1. Catheter type and location are mapped between PDMS and CARE module types / locations using dict_catheters and dict_location.
#'Catheters are uniquely identified by catheter type and location 
#'(Placement date is not used because it is not consistently recorded and in reality a patient should not have the same catheter type twice placed at the same location)
#'The PDMS (pdmsCatheters table) and CARE catheters (careModule_catheterCare_tb, attributes described below) are merged together.
#'
#'2. Catheter start and end date are calculated. 
#'A CARE catheter is started at the first catheter observation with catheter type and location. 
#'For PDMS this can be just the presence of start_date (eg: catheters placed in ICU)
#'A catheter is ended at:
#'  - the last observation retrieved through the query 
#'(eg: For a catheter just placed in ICU start date = end date. This is a technical detail.). 
#'The end date of the catheter will be the time of the last observation before the 48h gap.
#'Verwijderen observation is not used as catheter end because it is not recorded consistently.
#'As this is normally the last observation for the catheter, and a catheter is still considered at risk 48h after removal,
#'using Verwijderen observation would be redundant (it is the last observation anyhow)
#'- For PDMS catheters, the start date and end date are considered observations, only that a gap of more than 48h is allowed. 
#'If nothing follows in CARE for that catheter type and location (that have been mapped) within 48h the PDMS end date will be the end date.
#'- For CARE module, following attributes are considered observations:
#'  - catheter_type (TPtpT, TPobGVKcvkPICCtk)
#'  - location (TPtpP, TPobGVKcvkPICCpk)
#'  - placement (TPtpK)
#'  - lumens (TPtpL, TPobGVKcvkPICCal)
#'  - placement_date (TPtpPD)
#'  - insert_point_obs (TPobGVKcvkPICCip, TPobPKVap)
#'  - bandage_obs (TPobPKVva, TPobGVKcvkPICCvi)
#'  - tube_change(TPtpBZ)
#'  - bandage_change (TPtpV)
#'
#'insert_point_obs and bandage_obs are sometimes "orphaned" = these recordings do not have a linked catheter type and location.
#'These are either:
#'  - excluded OR
#'  - linked to the previously known catheter type and location. In the case there are 2 different catheters as previous at the exact same time one of them will be picked (no specific logic)
#'(configuration setting use_orphaned_bandage_observation_in_catheter_episode_calculation)
#'
#'3. Catheters are flagged as central line or not (peripheral catheter) using the mapping table dict_catheters
#'
#'4. Catheter episodes are calculated (using central lines only)
#'A catheter episode groups together overlapping catheters and catheters separated apart by less 
#'than 48 hours (between the end date of a catheter and start date of any another catheter).
#'
#'# (------------)
#'#          (------)
#'#           (---------) 
#'#                                       (---------)
#'#                                           (----------------)
#'# --------------------------------------------------------------> (time axis)
#'#     episode 1       |       > 48h   |   episode 2
#'
#'5. Extract features related to catheters
#'
#' - lumens - Port-a-cath catheters have always 1 lumen
#' - lumens - if a catheter changes location (e.g.: from left to right) and each has one lumen, 2 lumens will be calculated 
#' during the landmark (as we don't know if the first catheter was removed, with other words we don't know if one or 2 catheters are in place)
#' - catheter placement (at the bedside or in OR)
#' - number of central line and number of peripheral catheters
#'
#' @param con connection to the database created with DBI::dbConnect
#' @param admission_ids vector of admission IDs (functioneelDossierNr) included in the dataframe
#' 
#' @return LM_data - landmark dataframe with catheter features

init_data_catheters <- function(con, admission_ids, filter_date_time = NULL){
  
  start_time <- Sys.time()
  message("Dataframe initialization started...")
  
  # libraries and functions
  # -----------------------
  require(odbc)
  require(DBI)
  require(tidyverse)
  require(lubridate)
  require(zoo)
  
  # check configuration
  # -------------------
  
  if (!landmark_mode %in% c("24 hours", "calendar day", "trigger")) 
    stop("landmark_mode configration itme should be one of: 24 hours, calendar day, trigger.")
  
  # get data
  # --------
  
  # PDMS catheters
  message("Reading PDMS catheters from the database...")
  query <- "SELECT * FROM pdmsCatheters WHERE 1 = 1"
  PDMS_catheters <- get_data(con, query, admission_ids)
  
  # Care catheters
  message("Reading CARE catheters from the database...")
  start_time_2 <- Sys.time()
  query <- "SELECT DISTINCT functioneelDossierNr, tijdstip, rEenheid, zorgNr, waarde, attribuutDefinitieCode, zorgDefinitieCode
  FROM careModule_catheterCare_tb
  WHERE attribuutDefinitieCode in ('TPtpT', 'TPtpK', 'TPtpP', 'TPtpL', 'TPtpPD' , 'TPtpV', 'TPtpBZ',
        'TPobGVKcvkPICCvi','TPobPKVva', 'TPobGVKcvkPICCip','TPobPKVap', 'TPobGVKcvkPICCal',
        'TPobGVKcvkPICCtk', 'TPobGVKcvkPICCpk')"
  CARE_catheters <- get_data(con, query, admission_ids)
  message(sprintf("Got care catheters in %s minutes.", difftime(Sys.time(), start_time_2, units = "mins") %>% as.numeric()))
  
  # filter on date 
  # this is used ONLY for testing if the dynamic building of the observation is the same as the one for train/test/..
  if (!(is.null(filter_date_time))){
    filter_date_time <- as.POSIXct(filter_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # keep tz UTC - the default
    
    if (is.na(filter_date_time) | is.null(filter_date_time)){
      message("filter_date_time was not provided in the correct format, it will not be used")
    } else {
      PDMS_catheters <- PDMS_catheters %>% filter(StartTime <= filter_date_time)
      CARE_catheters <- CARE_catheters %>% filter(tijdstip <= filter_date_time)
    }
  }
  
  # prepare PDMS data
  # -----------------
  
  message("Preparing PDMS catheters...")
  
  # PDMS central lines
  PDMS_catheters <- PDMS_catheters %>% 
    rename(catheter_start_time = StartTime,
           catheter_end_time = EndTime,
           catheter_type = Katheter,
           location = Location) %>% 
    mutate(catheter_type = str_trim(catheter_type),
           location = str_trim(location)) %>% 
    select(functioneelDossierNr, catheter_type, location, catheter_start_time, catheter_end_time) %>% 
    distinct(functioneelDossierNr, catheter_type, location, catheter_start_time, catheter_end_time)
  
  # map catheter and location names
  PDMS_catheters <- PDMS_catheters %>% 
    mutate(catheter_type = map_names(catheter_type, dict_catheters),
           location = map_names(location, dict_location),
           catheter_start_time = as.POSIXct(catheter_start_time),
           catheter_end_time = as.POSIXct(catheter_end_time))
  # make character time POSIXct format (to avoid errors when adding hours(48))
  
  # prepare CARE data
  # -----------------
  
  message("Preparing CARE catheters...")
  CARE_catheters_wide <- CARE_catheters %>% 
    mutate(attribuutDefinitieCode = map_names(attribuutDefinitieCode, dict = dict_care_attributes)) %>% 
    pivot_wider(names_from = attribuutDefinitieCode, values_from = waarde, values_fn = list) 
  
  # add columns that don't exist for unnesting later
  # this happens mostly when running on one (or few) admission IDs for which eg: bandage observation does not exist
  cols_unnest <- c("catheter_type", "location", "placement", "lumens", "placement_date", "bandage_change", 
                   "tube_change", 
                   "bandage_obs", "insert_point_obs")
  
  for (col in cols_unnest){
    if(!col %in% colnames(CARE_catheters_wide)){
      CARE_catheters_wide[,col] <- NA_character_
    }
  }
  
  if (!nrow(CARE_catheters_wide) == 0) {
    CARE_catheters_wide <- CARE_catheters_wide %>% 
      unnest(cols = c(catheter_type, location, placement, lumens, placement_date, bandage_change, tube_change, 
                      bandage_obs), 
             keep_empty = TRUE) %>% 
      unnest(cols = c(insert_point_obs), keep_empty = TRUE) %>% # unnest this separately to avoid Error: Incompatible lengths:
      arrange(functioneelDossierNr, rEenheid, tijdstip) %>% 
      distinct() 
  }
  
  # map catheter and location names
  CARE_catheters_wide <- CARE_catheters_wide %>% 
    mutate(catheter_type = str_trim(catheter_type),
           location = str_trim(location),
           catheter_type = map_names(catheter_type, dict_catheters),
           location = map_names(location, dict_location)) 
  
  # make lumens numeric 
  CARE_catheters_wide <- CARE_catheters_wide %>% 
    mutate(lumens = str_trim(str_replace(lumens, "lumen", "")),
           lumens = as.numeric(lumens))
  
  # link bandage observations on zorgdeifitiecode TPobPKV to "Poortkatheter veneus"
  # according to excel this is:
  # TPobPKV	Observatie PKV (Poortkatheter veneus)
  # and indeed, all admissions with  TPobPKV have a Poortkatheter veneus"
  # this should be done after mapping the names
  CARE_catheters_wide <- CARE_catheters_wide %>% 
    mutate(catheter_type = if_else(zorgDefinitieCode == bandage_code_port_a_cath, catheter_type_port_a_cath, catheter_type)) %>% 
    group_by(functioneelDossierNr, catheter_type) %>% 
    arrange(tijdstip) %>% 
    mutate(location = if_else(zorgDefinitieCode == "TPobPKV",
                              zoo::na.locf(location, na.rm = FALSE), location)) %>% 
    ungroup()
  
  # take next observation for the first row if both are recorded at the exact time (tijdstip)
  CARE_catheters_wide <- CARE_catheters_wide %>%  
    arrange(functioneelDossierNr, tijdstip) %>% 
    group_by(functioneelDossierNr) %>% 
    arrange(tijdstip) %>% 
    mutate(location = if_else(is.na(location) & lead(tijdstip) == tijdstip & row_number()==1,
                              lead(location), location),
           catheter_type = if_else(is.na(catheter_type) & lead(tijdstip) == tijdstip & row_number()==1,
                                   lead(catheter_type), catheter_type),
           lumens = if_else(is.na(lumens) & lead(tijdstip) == tijdstip & row_number()==1,
                            lead(lumens), lumens)) %>% 
    ungroup()
  
  # check missing catheter type and location and give message
  n_rows_CARE_catheters_wide <- dim(CARE_catheters_wide)[[1]]
  
  missing_catheter_type <- CARE_catheters_wide %>% 
    filter(is.na(catheter_type)) 
  number_missing_catheter_type <- dim(missing_catheter_type)[[1]]
  
  missing_location <- CARE_catheters_wide %>% 
    filter(is.na(location)) 
  number_missing_location <- dim(missing_location)[[1]]
  
  if (number_missing_catheter_type > 0){
    message(sprintf("There are %s missing values for catheter type (%s of total records) for the following zorgDefinitieCode:",
                    number_missing_catheter_type, make_percent(number_missing_catheter_type/n_rows_CARE_catheters_wide)))
    print(missing_catheter_type %>% 
            group_by(zorgDefinitieCode) %>% 
            count())
  }
  
  if (number_missing_location > 0){
    message(sprintf("There are %s missing values for catheter location (%s of total records) for the following zorgDefinitieCode:",
                    number_missing_location, make_percent(number_missing_location/n_rows_CARE_catheters_wide)))
    print(missing_location %>% 
            group_by(zorgDefinitieCode) %>% 
            count())
  }
  
  # fill missing catheter type and location for bandage_obs and insert_point_obs
  # use the last catheter type and location regardless of the time between
  if (number_missing_catheter_type > 0 | number_missing_location > 0){
    
    if (use_orphaned_bandage_observation_in_catheter_episode_calculation){
      CARE_catheters_wide <- CARE_catheters_wide %>% 
        arrange(functioneelDossierNr, tijdstip, catheter_type) %>% 
        group_by(functioneelDossierNr) %>% 
        arrange(tijdstip, catheter_type) %>% # this should put the NA after the ones filled in 
        fill(c("catheter_type","location"), .direction = "down") %>% 
        ungroup()
      
      message("Configuration item use_orphaned_bandage_observation_in_catheter_episode_calculation = TRUE. Catheter type and location have been imputed with last observation")
      
      missing_catheter_type <- CARE_catheters_wide %>% 
        filter(is.na(catheter_type)) %>% 
        group_by(zorgDefinitieCode) %>% 
        count()
      
      missing_location <- CARE_catheters_wide %>% 
        filter(is.na(location)) %>% 
        group_by(zorgDefinitieCode) %>% 
        count()
      
      message("Remaining missing values for catheter type for the following zorgDefinitieCode. These will be removed:")
      print(missing_catheter_type)
      
      message("Remaining missing values for location for the following zorgDefinitieCode These will be romoved:")
      print(missing_location)
    } else {
      
      # fill only location
      CARE_catheters_wide <- CARE_catheters_wide %>% 
        arrange(functioneelDossierNr, tijdstip, catheter_type) %>% 
        group_by(functioneelDossierNr, catheter_type) %>% 
        arrange(tijdstip) %>% # this should put the NA after the ones filled in 
        fill(c("location"), .direction = "down") %>% 
        ungroup()
      
      message("Configuration item use_orphaned_bandage_observation_in_catheter_episode_calculation = FALSE. Catheter location registrations will be filled in. Empty catheter type will be removed")
      
      missing_catheter_type <- CARE_catheters_wide %>% 
        filter(is.na(catheter_type)) %>% 
        group_by(zorgDefinitieCode) %>% 
        count()
      
      missing_location <- CARE_catheters_wide %>% 
        filter(is.na(location)) %>% 
        group_by(zorgDefinitieCode) %>% 
        count()
      
      message("Remaining missing values for catheter type for the following zorgDefinitieCode. These will be removed:")
      print(missing_catheter_type)
      
      message("Remaining missing values for location for the following zorgDefinitieCode These will be removed:")
      print(missing_location)
      
    }
    
    # remove the remaining NAs (these are the first line arranged by date)
    CARE_catheters_wide <- CARE_catheters_wide %>% 
      filter(!is.na(catheter_type),
             !is.na(location))
    
  }
  
  # merge CARE catheters with PDMS_catheters and calculate catheter episodes
  # ------------------------------------------------------------------------
  
  # calculate catheter start time and catheter end time for CARE catheters
  # first catheter time is start time
  # whenever a lag of > 48 hours happens a new catheter starts
  message("Calculating catheter start and end time for CARE catheters...")
  
  CARE_catheters_wide <- CARE_catheters_wide %>% 
    arrange(functioneelDossierNr, tijdstip) %>% 
    group_by(functioneelDossierNr, catheter_type, location) %>% 
    arrange(tijdstip) %>% 
    mutate(lag_tijdstip = as.numeric(difftime(tijdstip, lag(tijdstip), units = "hours")),
           catheter_start_time = if_else(is.na(lag_tijdstip) | lag_tijdstip > catheter_end_time_lag, tijdstip, NA_POSIXct_)) %>% 
    fill(catheter_start_time, .direction = "down") %>% 
    ungroup() %>% 
    group_by(functioneelDossierNr, catheter_type, location, catheter_start_time) %>% 
    arrange(tijdstip) %>% 
    mutate(catheter_end_time = last(tijdstip)) %>% 
    ungroup() 
  
  # merge CARE catheters with PDMS catheters
  message("Merging CARE and PDMS catheters...")
  catheters <- PDMS_catheters %>% 
    rbind(CARE_catheters_wide %>% 
            distinct(functioneelDossierNr, catheter_type, location,
                     catheter_start_time, catheter_end_time)) %>% 
    arrange(functioneelDossierNr, catheter_start_time, catheter_end_time) %>% 
    distinct()
  
  # remove entries tagged as NOT-A-CATHETER - these are neither central lines, nor peripheral catheters
  catheters <- catheters %>% 
    filter(catheter_type != "NOT-A-CATHETER")
  
  # check UNKNOWN_VALUE after merging and remove unknown catheters / locations
  number_unknown_location <- catheters %>% 
    filter(location == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  number_unknown_catheter<- catheters %>% 
    filter(catheter_type == "UNKNOWN_VALUE") %>% 
    dim() %>% `[`(1)
  
  message("Catheters with unknown type or unkown location will be removed (if any).")
  message(sprintf("There are %s catheters with unknown type and %s catheters with unknown location after merge.", 
                  number_unknown_catheter, number_unknown_location))
  
  catheters <- catheters %>% 
    filter(catheter_type != "UNKNOWN_VALUE",
           location != "UNKNOWN_VALUE")
  
  # make central line after mapping catheters
  catheters <- catheters %>% 
    mutate(is_central_line = if_else(catheter_type %in% 
                                       dict_catheters$catheter_new[dict_catheters$is_central], 1, 0))
  
  # use CARE observation in Hoofding Dialyse zorgen to recover Dialysis catheters
  if (use_CARE_dialisys_catheters){ 
    
    message("Dialysis catheters from CARE hoofding Dialyse zorgen...")
    
    # get all times when anything is recorded
    query <- sprintf("SELECT functioneelDossierNr, tijdstip, zorgDefinitieCode, attribuutDefinitieCode, waarde 
                     FROM careModule_care_tb WHERE zorgDefinitieCode IN ('%s')", 
                     paste0(zorgDefinitieCodes_dialisys, collapse = "','"))
    
    dialysis_catheters <- get_data(con, query, admission_ids)
    
    # keep catheter placement apart in function of catheter type
    dialysis_catheters_placement <- dialysis_catheters %>% 
      filter(attribuutDefinitieCode %in% attr_catheter_type_dialysis) %>% 
      mutate(catheter_type = "Dialysis CVC",
             location = "dummy_location", # as we filter only for the catheter type attribute I leave this dummy
             placement = case_when(waarde %in% permanent_dialysis_catheters ~ "OR",
                                            waarde %in% temporary_dialysis_catheters ~ "bedside",
                                            TRUE ~ NA_character_)) %>% 
      distinct(functioneelDossierNr, tijdstip, catheter_type, location, placement)
      
    # get catheter location
    query <- sprintf("SELECT functioneelDossierNr, tijdstip, waarde 
                     FROM careModule_care_tb WHERE attribuutDefinitieCode IN ('%s')", 
                     paste0(attr_location_dialysis, collapse = "','"))
    
    dialysis_catheters_location <- get_data(con, query, admission_ids)
    
    dialysis_catheters_location <- dialysis_catheters_location %>% 
      rename(location = waarde) %>% 
      mutate(location = str_trim(location),
             location = map_names(location, dict_location)) %>% 
      rename(time_location = tijdstip)
    
    dialysis_catheters <- dialysis_catheters %>% 
      mutate(catheter_type = "Dialysis CVC") %>% 
      distinct(functioneelDossierNr, tijdstip, catheter_type) %>% 
      arrange(tijdstip)
    
    dialysis_catheters_location_tmp <- dialysis_catheters %>% 
      left_join(dialysis_catheters_location, by = "functioneelDossierNr", multiple = "all") %>% 
      filter(time_location <= tijdstip) %>% 
      arrange(functioneelDossierNr, tijdstip) %>% 
      group_by(functioneelDossierNr) %>% 
      arrange(tijdstip) %>% 
      fill(location, .direction = "down") %>% 
      ungroup() %>% 
      select(-time_location) %>% 
      distinct() 
    
    dialysis_catheters <- dialysis_catheters %>% 
      left_join(dialysis_catheters_location_tmp,
                by = c("functioneelDossierNr", "tijdstip", "catheter_type"),
                multiple = "all") %>% 
      mutate(location = if_else(is.na(location), "dialysis_unknown", location)) 
    
    # keep lumens apart for later
    # dialysis catheters have always 2 lumens
    dialysis_catheters_lumens <- dialysis_catheters %>% 
      mutate(lumens = 2,
             catheter_type = "Dialysis_CVC") 
    
    # calculate catheter start - end time
    dialysis_catheters <- dialysis_catheters %>% 
      arrange(functioneelDossierNr, tijdstip) %>% 
      group_by(functioneelDossierNr, catheter_type, location) %>% 
      arrange(tijdstip) %>% 
      mutate(lag_tijdstip = as.numeric(difftime(tijdstip, lag(tijdstip), units = "hours")),
             catheter_start_time = if_else(is.na(lag_tijdstip) | lag_tijdstip > catheter_end_time_lag, tijdstip, NA_POSIXct_)) %>% 
      fill(catheter_start_time, .direction = "down") %>% 
      ungroup() %>% 
      group_by(functioneelDossierNr, catheter_type, location, catheter_start_time) %>% 
      arrange(tijdstip) %>% 
      mutate(catheter_end_time = last(tijdstip)) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, catheter_type, location, catheter_start_time, catheter_end_time) 
    
    dialysis_catheters <- dialysis_catheters %>% 
      mutate(is_central_line = 1) 
    
    catheters <- catheters %>% 
      rbind(dialysis_catheters) %>% 
      arrange(functioneelDossierNr, catheter_start_time, catheter_end_time) 
    
  }
  
  # use ACTA procedures to recover Dialysis catheters
  if (use_ACTA_dialysis_catheters){ 
    
    message("Dialysis catheters from ACTA...")
    
    # get all times when anything is recorded
    query <- sprintf("SELECT functioneelDossierNr, performDate, actaDescription
                     FROM patientActivities_tb WHERE actaDescription IN ('%s')", 
                     paste0(acta_descriptions_dialysis, collapse = "','"))
    
    #dialysis_catheters_foo <- get_data(con, query, admission_ids)
    dialysis_catheters <- get_data(con, query, admission_ids)
    
    message("Reading Patient baseline Information from the database...")
    query <- "SELECT DISTINCT functioneelDossierNr, HospitalEndDate
            FROM PMbaseline_tb WHERE 1 = 1"
    admission_data <- get_data(con, query, admission_ids)
    
    # using start / end
    
    dialysis_catheters <- dialysis_catheters %>% 
      mutate(start_end = case_when(actaDescription %in% acta_descriptions_dialysis_start ~ "1_start",
                                   actaDescription %in% acta_descriptions_dialysis_end ~ "2_end",
                                   TRUE ~ NA_character_)) %>% 
      select(-actaDescription) %>% 
      arrange(functioneelDossierNr, performDate, start_end)
    
    dialysis_catheters <- dialysis_catheters %>% 
      arrange(functioneelDossierNr, performDate, desc(start_end)) %>% 
      group_by(functioneelDossierNr) %>% 
      arrange(performDate, desc(start_end)) %>% 
      mutate(change = if_else(dplyr::lag(start_end, default = "2_end") != start_end, 1, 0)) %>% 
      ungroup() %>% 
      arrange(functioneelDossierNr, performDate, desc(start_end)) 
    
    dialysis_catheters <- dialysis_catheters %>% 
      filter(change == 1) %>% 
      arrange(functioneelDossierNr, performDate, desc(start_end)) %>% 
      group_by(functioneelDossierNr, start_end) %>% 
      arrange(performDate) %>% 
      mutate(cathe_no = row_number()) %>% 
      ungroup() %>% 
      arrange(functioneelDossierNr, performDate, desc(start_end))
    
    dialysis_catheters <- dialysis_catheters %>% 
      select(-change) %>% 
      mutate(start_end = if_else(start_end == "1_start", "catheter_start_time", "catheter_end_time")) %>% 
      pivot_wider(names_from = "start_end", values_from = performDate) %>% 
      left_join(admission_data, by = "functioneelDossierNr")
    
    if (!"catheter_end_time" %in% colnames(dialysis_catheters)){
      dialysis_catheters <- dialysis_catheters %>% 
        mutate(catheter_end_time = NA_POSIXct_)
    }
    
    dialysis_catheters <- dialysis_catheters %>% 
      mutate(catheter_end_time = if_else(is.na(catheter_end_time), HospitalEndDate, catheter_end_time)) %>% 
      select(-HospitalEndDate)
    
    dialysis_catheters <- dialysis_catheters %>% 
      mutate(catheter_type = "Dialysis CVC",
             location = "dialysis_unknown_acta",
             is_central_line = 1) %>% 
      select(-cathe_no) 
    
    catheters <- catheters %>% 
      rbind(dialysis_catheters) %>% 
      arrange(functioneelDossierNr, catheter_start_time, catheter_end_time) 
    
  }
  
  # re-calculate catheter start and end time based on merged CARE and PDMS
  message("Calculating catheter start and end time based on both CARE and PDMS...")
  
  catheters <- catheters %>% 
    group_by(functioneelDossierNr, catheter_type, location) %>% 
    mutate(catheter_id = group_overlap(catheter_start_time, catheter_end_time)) %>% 
    ungroup() %>% 
    group_by(functioneelDossierNr, catheter_type, location, catheter_id) %>% 
    mutate(catheter_start_time = min_quiet(catheter_start_time),
           catheter_end_time = max_quiet(catheter_end_time)) %>% 
    ungroup() 
  
  # calculate catheter episodes
  # ---------------------------
  
  message("Calculating catheter episodes...")
  
  # keep only central lines
  central_lines <- catheters %>% 
    filter(is_central_line == 1) %>% 
    distinct(functioneelDossierNr, catheter_type, location, catheter_start_time, catheter_end_time)
  
  # if baseline is 48h after admission, move catheter start time
  if (baseline_is_48h_after_admission){
    
    message("Adjusting catheter start time 48 h after admission...")
    
    admission_ids_CL <- unique(central_lines$functioneelDossierNr)
    
    # read in admission datetime
    message("Reading admission data...")
    query <- "SELECT functioneelDossierNr, HospitalStartDate, HospitalEndDate
            FROM PMbaseline_tb WHERE 1 = 1"
    baseline_data <- get_data(con, query, admission_ids_CL)
    
    # adjust start time as max(admission date + 48h, catheter_start_time)
    central_lines <- central_lines %>% 
      left_join(baseline_data, by = "functioneelDossierNr") %>% 
      mutate(catheter_start_time = pmax(catheter_start_time, HospitalStartDate + hours(48))) %>% 
      filter(catheter_start_time <= HospitalEndDate) %>% 
      select(-HospitalStartDate, HospitalEndDate)
    
    # adjust start time as max(admission date + 48h, catheter_start_time) in catheters table too 
    # (used later to extract "during episode" features)
    catheters <- catheters %>% 
      left_join(baseline_data, by = "functioneelDossierNr") %>% 
      mutate(catheter_start_time = if_else(is_central_line == 1,
                                           pmax(catheter_start_time, HospitalStartDate + hours(48)),
                                           catheter_start_time)) %>% 
      filter(catheter_start_time <= HospitalEndDate) %>% 
      select(-HospitalStartDate, HospitalEndDate)
    
  }
  
  # calculate catheter episodes
  # a new episode starts if there is a gap in central lines of more than 48 hours
  # example: 
  
  # [------------]
  #          [------]
  #           [---------] 
  #                                       [---------]
  #                                           [----------------]
  # --------------------------------------------------------------> (time axis)
  #     episode 1       |       > 48h   |   episode 2
  
  central_lines <- central_lines %>% 
    mutate(catheter_end_time = catheter_end_time + catheter_end_time_lag*60*60) %>% # add 48 hours to the catheter end time (only in central_lines)
    # change hours(48) to 48*60*60 because functioneelDossierNr = 5405099 has catheter end time 2013-03-29 02:17:20 + 2 days, which is the shift date (Mar 31st) of daylight saving time in 2013, using hours (48) will cause NA value
    arrange(functioneelDossierNr, catheter_start_time) %>% 
    group_by(functioneelDossierNr) %>% 
    arrange(catheter_start_time) %>% 
    mutate(CAT_catheter_episode = group_overlap(catheter_start_time, catheter_end_time),
           CAT_catheter_episode = as.integer(CAT_catheter_episode)) %>% 
    ungroup() %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    mutate(CAT_start_time_episode = min_quiet(catheter_start_time),
           CAT_end_time_episode = max_quiet(catheter_end_time)) %>% 
    ungroup() 
  
  # to check
  #central_lines %>% 
  #  group_by(functioneelDossierNr ) %>% 
  #  summarise(n = max(CAT_catheter_episode)) %>% 
  #  arrange(desc(n)) %>% 
  #  head(20)
  
  # create landmark dataframe
  # -------------------------
  
  message("Creating landmark dataframe...")
  
  if (nrow(central_lines) == 0){
    LM_data <- central_lines %>% 
      mutate(LM = NA_integer_, 
             LM_start_time = NA_POSIXct_, 
             LM_end_time = NA_POSIXct_) %>% 
      select(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode,
             LM, LM_start_time, LM_end_time)
  } else {
    
    if (landmark_mode == "24 hours") {
      LM_data <- central_lines %>% 
        distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode) %>% 
        group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
        mutate(LM_end_time = map2(min(CAT_start_time_episode), max(CAT_end_time_episode),
                                  function(x, y) seq(x, y, by = "day"))) %>% 
        ungroup() %>% 
        unnest(LM_end_time) %>% 
        group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
        arrange(LM_end_time) %>% 
        mutate(LM = as.integer(row_number() - 1)) %>% 
        ungroup() %>% 
        arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
        mutate(LM_start_time = LM_end_time - hours(24) + seconds(1)) %>% 
        select(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode,
               LM, LM_start_time, LM_end_time)
    } else if (landmark_mode == "calendar day") {
      LM_data_temp <- central_lines %>% 
        distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode) %>% 
        group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
        mutate(LM_end_time = map2(as.Date(min(CAT_start_time_episode)), as.Date(max(CAT_end_time_episode) + days(1)),
                                  function(x, y) seq(x, y, by = "day"))) %>% 
        ungroup() %>% 
        unnest(LM_end_time) %>% 
        group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
        arrange(LM_end_time) %>% 
        mutate(LM = as.integer(row_number())) %>% 
        ungroup() %>% 
        arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
        mutate(LM_start_time = LM_end_time + seconds(1) - seconds(1), # this makes it datetime
               LM_end_time = LM_end_time + hours(23) + minutes(59) + seconds(59),
               LM_start_time = if_else(LM == 1, CAT_start_time_episode, LM_start_time)
        ) %>% # keep baseline at exact time
        select(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, #CAT_end_time_episode,
               LM, LM_start_time, LM_end_time)
      
      LM_data <- LM_data_temp %>% 
        filter(LM == 1L) %>% 
        mutate(LM = 0L,
               LM_end_time = LM_start_time,
               LM_start_time = as.Date(LM_start_time) + seconds(1) - seconds(1)) %>% # this makes it datetime
        rbind(LM_data_temp) %>% 
        arrange(functioneelDossierNr, CAT_catheter_episode, LM) 
      
    } else if (landmark_mode == "trigger") {
      # Note: for now, trigger mode is not used. If we use it later, this is the initialization of it
      
      # keep catheter episodes 
      LM_data <- central_lines %>% 
        distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode) 
      
      # keep central lines with start and end date
      LM_data_central_lines <- LM_data %>% 
        left_join(central_lines %>% 
                    select(functioneelDossierNr, catheter_type, location, 
                           catheter_start_time, catheter_end_time), 
                  by = "functioneelDossierNr", multiple = "all") %>% 
        filter(catheter_start_time >= CAT_start_time_episode,
               catheter_start_time <= CAT_end_time_episode) %>% 
        rename(CAT_catheter_type = catheter_type, 
               CAT_location = location, 
               CAT_catheter_start_time = catheter_start_time,
               CAT_catheter_end_time = catheter_end_time)
      
      # keep peripheral catheters with start and end date
      peripheral_catheters <- catheters %>% 
        filter(is_central_line == 0) %>% 
        rename(CAT_peripheral_catheter_type = catheter_type,
               CAT_peripheral_location = location,
               CAT_peripheral_catheter_start_time = catheter_start_time,
               CAT_peripheral_catheter_end_time = catheter_end_time) %>% 
        select(functioneelDossierNr, CAT_peripheral_catheter_type, CAT_peripheral_location, 
               CAT_peripheral_catheter_start_time, CAT_peripheral_catheter_end_time)
      
      LM_data_peripheral_catheters <- LM_data %>% 
        left_join(peripheral_catheters, by = "functioneelDossierNr", multiple = "all") %>% 
        filter((CAT_peripheral_catheter_start_time >= CAT_start_time_episode - days(1) &
                  CAT_peripheral_catheter_start_time <= CAT_end_time_episode) |
                 (CAT_peripheral_catheter_end_time >= CAT_start_time_episode - days(1) &
                    CAT_peripheral_catheter_end_time <= CAT_end_time_episode) 
        )
      
      # merge with bind rows 
      LM_data <- LM_data_central_lines %>% 
        bind_rows(LM_data_peripheral_catheters)
      
      return(LM_data)
      
    } else {
      stop("Landmark setting not supported")
    }
  }
  
  # filter landmarks to be before filtered date
  if (!(is.null(filter_date_time))){
    LM_data <- LM_data %>% filter(LM_end_time <= filter_date_time)
  }
  
  # add feature - catheter, location and lumens
  # -------------------------------------------
  
  message("Adding features - catheter type, location and lumens...")
  
  # check:
  #LM_data %>%
  #  group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
  #  mutate(last_LM = last(LM)) %>% 
  #  ungroup() %>% 
  #  filter(lengths(catheter_type_location_at_LM) == 0,
  #         LM != last_LM,
  #         LM != last_LM - 1)
  #
  # Note: there can be LMs with no active catheters (except of last 2 LMs) for very short scattered catheters, eg:
  # cath 1 [-]
  # cath 2     [-]
  # cath 3            [--] 
  # --------------------------------------------------------------> (time axis)
  #        |       |       |       |       |           
  #           LM_1    LM_2    LM_3    LM_4    
  #        st      end     st      end    st     
  
  # catheters during LANDMARK
  central_lines_during_LM <- catheters %>% 
    filter(is_central_line == 1) %>% 
    rename(catheter_location = location) %>% 
    mutate(catheter_type = clean_strings(catheter_type)) %>% 
    mutate(catheter_type_location_list = paste(catheter_type, catheter_location, sep = "_"),
           catheter_type_location = 
             case_when(catheter_type == "Tunneled_CVC" ~ "tunneled_CVC",
                       catheter_type == "PICC" ~ "PICC",
                       catheter_type == "Port_a_cath" ~ "Port_a_cath",
                       str_detect(catheter_type, "CVC") & str_detect(catheter_location, "Neck") ~ "CVC_neck",
                       str_detect(catheter_type, "CVC") & str_detect(catheter_location, "Collarbone") ~ "CVC_collarbone",
                       str_detect(catheter_type, "CVC") ~ "CVC_other"),
           catheter_location = str_trim(str_replace(catheter_location, "left|right", "")),
           # map locations in a second step to more condensed/collapsed values
           catheter_location = map_names(catheter_location, dict_location_2),
           catheter_location = clean_strings(catheter_location)
    ) %>%
    distinct(functioneelDossierNr, catheter_type_location, catheter_type_location_list,
             catheter_type, catheter_location,
             catheter_start_time, catheter_end_time) %>% 
    left_join(LM_data, by = "functioneelDossierNr", multiple = "all") %>% 
    filter(strptime(LM_start_time, "%Y-%m-%d %H:%M:%S", tz="UTC") <= 
             strptime(catheter_end_time + hours(catheter_end_time_lag), "%Y-%m-%d %H:%M:%S", tz="UTC") , # either LM start or end time intersect the catheter segment
           strptime(LM_end_time, "%Y-%m-%d %H:%M:%S", tz="UTC") >=   # strptime strips the miliseconds 
             strptime(catheter_start_time, "%Y-%m-%d %H:%M:%S", tz="UTC")) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
             CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time) %>% 
    arrange(catheter_start_time) %>% 
    mutate(catheter_type_location_list = list(unique(catheter_type_location_list)),
           CAT_catheter_type_location = list(unique(catheter_type_location)),
           CAT_catheter_type = list(unique(catheter_type)),
           CAT_catheter_location = list(unique(catheter_location))
    ) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM,
             CAT_catheter_type_location, 
             CAT_catheter_type, CAT_catheter_location,
             catheter_type_location_list) 
  
  central_lines_during_LM <- central_lines_during_LM %>% 
    make_col_binary_all(cols = c("CAT_catheter_type_location",
                                 "CAT_catheter_type", "CAT_catheter_location")) %>% 
    select(-c(CAT_catheter_type_location, 
              CAT_catheter_type, CAT_catheter_location))
  
  LM_data <- LM_data %>% 
    left_join(central_lines_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # check:
  #LM_data %>%
  #  group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
  #  mutate(last_LM = last(LM)) %>% 
  #  ungroup() %>% 
  #  filter(lengths(catheter_type_location_during_LM) == 0,
  #         LM != last_LM,
  #         LM != last_LM - 1) %>% 
  #  select(functioneelDossierNr, CAT_catheter_episode, last_LM, LM, LM_start_time, LM_end_time)
  #
  # Note: there can be LMs with no active catheters (except of last 2 LMs) for gaps of 24h (<48h), eg:
  # cath 1 [------]
  # cath 2     [--] 
  # cath 3                   [--] 
  # --------------------------------------------------------------> (time axis)
  #        |       |       |       |       |           
  #           LM_1    LM_2    LM_3    LM_4    
  #        st      end     st      end    st  
  
  # add feature - lumens during LANDMARK
  # lumens are only registered in CARE, not in PDMS
  
  catheter_lumens <- CARE_catheters_wide %>% 
    mutate(is_central_line = if_else(catheter_type %in% 
                                       dict_catheters$catheter_new[dict_catheters$is_central], 1, 0)) %>% 
    filter(is_central_line == 1,
           !is.na(lumens)) %>% 
    distinct(functioneelDossierNr, tijdstip, catheter_type, location, lumens) %>% 
    mutate(catheter_type = clean_strings(catheter_type)) 
  
  if (use_CARE_dialisys_catheters){
    catheter_lumens <- catheter_lumens %>% 
      rbind(dialysis_catheters_lumens)
  }
  
  lumens_during_LM <- catheter_lumens %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= LM_start_time, 
           tijdstip <= LM_end_time) %>% 
    arrange(functioneelDossierNr, tijdstip) %>% 
    pivot_wider(names_from = c(catheter_type), names_prefix = "CAT_lumens_",
                values_from = lumens, values_fn = last, values_fill = NA_real_) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, location) %>% # there can be 2 CVC with different locations
    arrange(tijdstip) %>% 
    fill(starts_with("CAT_lumens_"), .direction = "down") %>% # fill within LM and location to take last non-NA
    slice_tail() %>% 
    ungroup() %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% # sum per catheter type (without location now)
    mutate_at(vars(starts_with("CAT_lumens_")), ~if_else(all(is.na(.)), NA_real_, sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    select(-c(tijdstip, location, CAT_start_time_episode, CAT_end_time_episode, 
              LM_start_time, LM_end_time)) %>% 
    distinct()
  
  LM_data <- LM_data %>% 
    left_join(lumens_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # add lumens columns if they don't exist
  # e.g., in 2013, there are no dialysis catheters in CARE (only in PDMS)
  for (col in c("CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC", "CAT_lumens_Port_a_cath", 
                "CAT_lumens_PICC", "CAT_lumens_Dialysis_CVC",
                "CAT_catheter_type_binary_all_CVC",
                "CAT_catheter_type_binary_all_Tunneled_CVC",
                "CAT_catheter_type_binary_all_Port_a_cath",
                "CAT_catheter_type_binary_all_PICC",
                "CAT_catheter_type_binary_all_Dialysis_CVC")){
    if(!col %in% colnames(LM_data)){
      LM_data[,col] <- 0
    }
  }
  
  # fix NAs (I find no better way to do it)
  LM_data <- LM_data %>% 
    mutate(CAT_lumens_CVC = if_else(CAT_catheter_type_binary_all_CVC == 0,
                                    0, CAT_lumens_CVC),
           CAT_lumens_Tunneled_CVC = if_else(CAT_catheter_type_binary_all_Tunneled_CVC == 0,
                                             0, CAT_lumens_Tunneled_CVC),
           CAT_lumens_Port_a_cath = if_else(CAT_catheter_type_binary_all_Port_a_cath == 0,
                                            0, CAT_lumens_Port_a_cath),
           CAT_lumens_PICC = if_else(CAT_catheter_type_binary_all_PICC == 0,
                                     0, CAT_lumens_PICC),
           CAT_lumens_Dialysis_CVC = if_else(CAT_catheter_type_binary_all_Dialysis_CVC == 0,
                                             0, CAT_lumens_Dialysis_CVC))
  
  # impute lumens = 1 for port catheters (acc. to Thijs and Christel, port a cath have 1 lumen always)
  # impute lumens = 2 for dialysis catheters (acc. to Amaryllis and Marieke, these are always 2 lumens catheters)
   LM_data <- LM_data %>% 
    mutate(CAT_lumens_Port_a_cath = if_else(CAT_catheter_type_binary_all_Port_a_cath == 1, 1, 
                                            CAT_lumens_Port_a_cath),
           CAT_lumens_Dialysis_CVC = if_else(CAT_catheter_type_binary_all_Dialysis_CVC == 1, 2, 
                                             CAT_lumens_Dialysis_CVC))
  
  # calculate total number of lumens and delete the individual number
  # if lumens per catheter are preferred, this part can be commented
  # LM_data <- LM_data %>% 
  #   mutate(CAT_total_lumens = CAT_lumens_CVC + CAT_lumens_Tunneled_CVC +
  #            CAT_lumens_Port_a_cath + CAT_lumens_PICC + CAT_lumens_Dialysis_CVC) %>% 
  #   select(-c(CAT_lumens_CVC, CAT_lumens_Tunneled_CVC,
  #             CAT_lumens_Port_a_cath, CAT_lumens_PICC, CAT_lumens_Dialysis_CVC))
  
  # peripheral catheters during LANDMARK
  peripheral_catheters_during_LM <- catheters %>% 
    filter(is_central_line == 0) %>% 
    mutate(peripheral_catheter_type_location = paste(catheter_type, location, sep = "_")) %>% 
    distinct(functioneelDossierNr, peripheral_catheter_type_location, catheter_start_time, catheter_end_time) %>% 
    left_join(LM_data, by = "functioneelDossierNr", multiple = "all") %>% 
    filter(strptime(LM_start_time, "%Y-%m-%d %H:%M:%S", tz="UTC") <= 
             strptime(catheter_end_time + hours(catheter_end_time_lag), "%Y-%m-%d %H:%M:%S", tz="UTC") , # either LM start or end time intersect the catheter segment
           strptime(LM_end_time, "%Y-%m-%d %H:%M:%S", tz="UTC") >=   # strptime strips the miliseconds 
             strptime(catheter_start_time, "%Y-%m-%d %H:%M:%S", tz="UTC")) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, 
             CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time) %>% 
    arrange(catheter_start_time, peripheral_catheter_type_location) %>% 
    mutate(peripheral_catheter_type_location = list(unique(peripheral_catheter_type_location))) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM,
             peripheral_catheter_type_location) 
  
  LM_data <- LM_data %>% 
    left_join(peripheral_catheters_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # add feature - catheter placement
  # --------------------------------
  
  message("Adding features - catheter placement...")
  
  # this variable is available only in CARE
  # acc. to Thijs & Geert if a catheter is placed in OKA, it will always be registered in CARE (not in PDMS)
  # so there should not be missing data for PDMS
  # also, this variable is registered later than the placement before 2016
  # starting 2015-2016, it should be correctly registered (at the right tijdstip)
  
  # keep central lines, map placement
  catheter_placement <- CARE_catheters_wide %>% 
    mutate(is_central_line = if_else(catheter_type %in% 
                                       dict_catheters$catheter_new[dict_catheters$is_central], 1, 0)) %>% 
    filter(is_central_line == 1,
           !is.na(placement)) %>% 
    mutate(placement = map_names(placement, dict_catheter_placement)) %>% 
    filter(placement != "UNKNOWN_VALUE") %>% 
    distinct(functioneelDossierNr, tijdstip, catheter_type, location, placement)
  
  if (use_CARE_dialisys_catheters){
    catheter_placement <- catheter_placement %>% 
      rbind(dialysis_catheters_placement)
  }
  
  # filter on LM
  catheter_placement_during_episode <- catheter_placement %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 CAT_start_time_episode, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(tijdstip >= CAT_start_time_episode, 
           tijdstip <= LM_end_time) 
  
  # make list (for multiple catheters) and make binary
  catheter_placement_during_episode <- catheter_placement_during_episode %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(CAT_catheter_placement = list(unique(placement))) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CAT_catheter_placement) %>% 
    make_col_binary_all("CAT_catheter_placement") %>% 
    select(-CAT_catheter_placement)
  
  LM_data <- LM_data %>% 
    left_join(catheter_placement_during_episode, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # add columns if they don't exist
  for (col in c("CAT_catheter_placement_binary_all_OR", "CAT_catheter_placement_binary_all_bedside") ){
    if(!col %in% colnames(LM_data)){
      LM_data[,col] <- 0
    }
  }
  
  # if a catheter is not placed in OR or VE, the value will be 0
  LM_data <- LM_data %>% 
    mutate(CAT_catheter_placement_binary_all_OR = if_else(is.na(CAT_catheter_placement_binary_all_OR), 
                                                          0, CAT_catheter_placement_binary_all_OR),
           CAT_catheter_placement_binary_all_bedside = if_else(is.na(CAT_catheter_placement_binary_all_bedside), 
                                                               0, CAT_catheter_placement_binary_all_bedside))
  
  # add feature - number of catheters
  # ---------------------------------
  
  message("Adding features - number of catheters...")
  
  LM_data <- LM_data %>% 
    mutate(CAT_number_central_lines = lengths(catheter_type_location_list),
           CAT_number_peripheral_catheters = lengths(peripheral_catheter_type_location)) %>% 
    select(-c(peripheral_catheter_type_location, 
              catheter_type_location_list))
  
  # add feature - number of catheter days per catheter type
  # -------------------------------------------------------
  
  message("Adding features - number of catheter days...")
  
  LM_data <- LM_data %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    mutate(CAT_total_episode_days_CVC = cumsum(CAT_catheter_type_binary_all_CVC),
           CAT_consecutive_current_days_CVC = cumsum_in_blocks(CAT_catheter_type_binary_all_CVC),
           CAT_total_episode_days_Tunneled_CVC = cumsum(CAT_catheter_type_binary_all_Tunneled_CVC),
           CAT_consecutive_current_days_Tunneled_CVC = cumsum_in_blocks(CAT_catheter_type_binary_all_Tunneled_CVC),
           CAT_total_episode_days_Port_a_cath = cumsum(CAT_catheter_type_binary_all_Port_a_cath),
           CAT_consecutive_current_days_Port_a_cath = cumsum_in_blocks(CAT_catheter_type_binary_all_Port_a_cath),
           CAT_total_episode_days_PICC = cumsum(CAT_catheter_type_binary_all_PICC),
           CAT_consecutive_current_days_PICC = cumsum_in_blocks(CAT_catheter_type_binary_all_PICC),
           CAT_total_episode_days_Dialysis_CVC = cumsum(CAT_catheter_type_binary_all_Dialysis_CVC),
           CAT_consecutive_current_days_Dialysis_CVC = cumsum_in_blocks(CAT_catheter_type_binary_all_Dialysis_CVC)
    ) %>% 
    ungroup()
  
  # add feature - start time of episode (first catheter placement time)
  # -------------------------------------------------------------------
  
  message("Adding features - hour of first catheter...")
  
  LM_data <- LM_data %>% 
    mutate(CAT_start_episode_hour_num = hour(CAT_start_time_episode)) %>% 
    mutate(CAT_start_episode_hour_categ = case_when(CAT_start_episode_hour_num >= 0 &
                                                      CAT_start_episode_hour_num <= 6 ~ "00_06",
                                                    CAT_start_episode_hour_num > 6 &
                                                      CAT_start_episode_hour_num <= 9 ~ "06_09",
                                                    CAT_start_episode_hour_num > 9 &
                                                      CAT_start_episode_hour_num <= 18 ~ "09_18",
                                                    CAT_start_episode_hour_num > 18 ~ "18_00",
                                                    TRUE ~ NA_character_)) %>% 
    mutate(CAT_start_episode_hour_sin = sin(CAT_start_episode_hour_num * (2*pi/24)),
           CAT_start_episode_hour_cos = cos(CAT_start_episode_hour_num * (2*pi/24)))
  
  message(sprintf("Data processing finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}

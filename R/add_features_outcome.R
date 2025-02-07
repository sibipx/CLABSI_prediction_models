#' Adds features related to patient outcome to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' @param include_days_after_CLABSI if FALSE, the days in the catheter epiosde after CLABSI are removed (training mode); if TRUE, the days after CLABSI are kept (test mode)
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_outcome <- function(LM_data, con, 
                                 include_days_after_CLABSI = FALSE){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[col_names %in% c("eventtime", "type", "CLABSI_history")]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding patient outcome features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  if (length(admission_ids) == 0) {
    LM_data <- LM_data %>% 
      mutate(eventtime = NA_real_,
             type = NA_character_)
    
    return(LM_data)
  }
  
  # Get admission data (Discharge or death event)
  # ----------------------------------------------
  
  message("Reading Patient baseline Information from the database...")
  query <- "SELECT DISTINCT functioneelDossierNr, HospitalStartDate, HospitalEndDate, 
            typeOntslag, eadnrPatientKey 
            FROM PMbaseline_tb WHERE 1 = 1"
  admission_data <- get_data(con, query, admission_ids)
  
  # map discharge type (discharge or death)
  baseline_data <- admission_data %>% 
    mutate(discharge_type = map_names(typeOntslag, dict_discharge_type)) %>% 
    distinct(functioneelDossierNr, HospitalEndDate, discharge_type) %>% 
    rename(eventtime = HospitalEndDate, 
           type = discharge_type)
  
  # Get ward data (used to exclude Palliative care landmarks)
  # ---------------------------------------------------------
  
  query <- "SELECT DISTINCT functioneelDossierNr, presentFrom, presentTill FROM PMwhere_tb WHERE eenheid IN "
  query_complete = sprintf("%s (%s)", query, paste(palliative_care_eenhied, collapse = ", "))
  palliative_care_data <- get_data(con, query_complete, admission_ids)
  
  palliative_care_data <- palliative_care_data %>% 
    mutate(type = "Death") %>% 
    rename(eventtime = presentFrom) %>% 
    select(functioneelDossierNr, eventtime, type)
  
  if (use_contact_with_palliative_care){
    
    # get contact with palliative care
    query <- "SELECT * FROM palliativeCare_tb WHERE 1 = 1"
    palliative_care_contact <- get_data(con, query, admission_ids)
    
    palliative_care_contact <- palliative_care_contact %>% 
      mutate(type = "Death") %>% 
      rename(eventtime = tijdRec)
    
    # filter only the contacts that take place after the admission date
    palliative_care_contact <- palliative_care_contact %>% 
      left_join(admission_data %>% select(functioneelDossierNr, HospitalStartDate),
                by = "functioneelDossierNr") %>% 
      filter(eventtime >= HospitalStartDate) %>% 
      select(-HospitalStartDate)
    
    # merge palliative care transfer with palliative care contact and take whichever happens first
    palliative_care_data <- palliative_care_data %>% 
      rbind(palliative_care_contact) %>% 
      arrange(functioneelDossierNr, eventtime) %>% 
      group_by(functioneelDossierNr) %>% 
      mutate(eventtime = min(eventtime)) %>% 
      ungroup() %>% 
      distinct(functioneelDossierNr, eventtime, type)
  }
  
  # Get CLABSI data (CLABSI event)
  # ------------------------------
  
  if (use_CLABSI_definition_Veerle){
    
    message("Reading Patient CLABSI Information from the database...")
    
    query <- "SELECT * FROM CLABSIevents_tb2 WHERE 1 = 1"
    clabsi_data <- get_data(con, query, admission_ids)
    
    clabsi_data$datumAfname <- as.POSIXct(format(clabsi_data$datumAfname, "%Y-%m-%d"), tz = "UTC")
  } else {
    
    message("Calculating CLABSI...")
    
    clabsi_data <- calculate_BSI(con, samples_blood, samples_exclude, table_germs, 
                                 admission_ids = admission_ids)
  }
  
  clabsi_data <- subset(clabsi_data, clabsiType == 1 | clabsiType == 2)
  
  clabsi_data <- clabsi_data %>% 
    distinct(functioneelDossierNr, datumAfname) %>% 
    rename(eventtime = datumAfname) %>% 
    mutate(type = "CLABSI")
  
  # Get catheter removal from LM_data
  # ---------------------------------
  
  catheter_removal <- LM_data %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, CAT_end_time_episode) %>% 
    rename(eventtime = CAT_end_time_episode) %>% 
    mutate(type = "Discharge") %>% # the event is "discharge or 48h after catheter removal"
    select(-CAT_catheter_episode)
  
  # calculate event of all types (death / discharge / catheter removal / CLABSI)
  # -----------------------------------------------------------------------------
  
  message("Calculating event...")
  
  # merge the 3 types of events in one common format
  event_data <- baseline_data %>% 
    rbind(clabsi_data) %>% 
    rbind(catheter_removal) %>% 
    rbind(palliative_care_data)
  
  # keep only LMs before palliative care
  LM_data <- LM_data %>% 
    left_join(palliative_care_data, by = "functioneelDossierNr") %>% 
    filter((LM_end_time < eventtime) | is.na(eventtime)) %>% 
    select(-c(eventtime, type))
  
  # keep only LMs before death or discharge
  LM_data <- LM_data %>% 
    left_join(baseline_data, by = "functioneelDossierNr") %>% 
    filter((LM_end_time < eventtime) | is.na(eventtime)) %>% 
    select(-c(eventtime, type))

  # keep only events that happen during the catheter episode (catheter episodes end 48 h after catheter removal)
  LM_data <- LM_data %>% 
    left_join(event_data, by = "functioneelDossierNr", multiple = "all") %>% 
    filter(eventtime >= CAT_start_time_episode,
           eventtime <= CAT_end_time_episode) 
  
  # keep only the first event (CLABSI, catheter removal, discharge, death, whichever happens first)
  LM_data <- LM_data %>%  
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    arrange(eventtime) %>% 
    slice(1) %>% 
    ungroup()
  
  # filter landmarks after CLABSI event has happened (training mode)
  if (!include_days_after_CLABSI) {
    # if CLABSI is encountered, cut the episode at the CLABSI event type
    LM_data <- LM_data %>% 
      filter(LM_end_time < eventtime)
  }
  
  # add feature - history of CLABSI
  # -------------------------------
  
  message("Adding feature - history of CLABSI...")
  
  # CLABSIs linked to patient
  patient_CLABSI <- LM_data %>% # use LM data as it is already filtered for events during CAT episode
    distinct(functioneelDossierNr, eventtime, type) %>% 
    filter(type == "CLABSI") %>% 
    left_join(admission_data %>% 
                select(functioneelDossierNr, eadnrPatientKey),
              by = "functioneelDossierNr") %>% 
    mutate(CLABSI_history = 1) %>% 
    rename(CLABSI_time = eventtime) %>% 
    select(eadnrPatientKey, CLABSI_history, CLABSI_time)
  
  # CLABSIs previous to LM (but within 3 months)
  previous_CLABSI <- LM_data %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, LM_end_time, eventtime, type) %>% 
    left_join(admission_data %>% 
                select(functioneelDossierNr, eadnrPatientKey),
              by = "functioneelDossierNr") %>% 
    left_join(patient_CLABSI, by = "eadnrPatientKey", multiple = "all") %>% 
    filter(CLABSI_time >= LM_end_time - months(3),
           CLABSI_time < LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, CLABSI_history)
  
  LM_data <- LM_data %>% 
    left_join(previous_CLABSI, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(CLABSI_history = if_else(is.na(CLABSI_history), 0, CLABSI_history))
  
  # make event time in days since the start of the catheter episode
  LM_data <- LM_data %>% 
    mutate(eventtime = as.numeric(difftime(eventtime, CAT_start_time_episode, units = "days")))
  
  # final check - check if there is any LM without outcome
  if (dim(LM_data %>% filter(is.na(eventtime)))[[1]] > 0 |
      dim(LM_data %>% filter(is.na(type)))[[1]] > 0){
    stop("There are landmarks without outcome. Something went wrong!")
  }
  
  message(sprintf("Patient outcome features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
}



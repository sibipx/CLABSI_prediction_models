#' Adds patient in study feature to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features
#' 
#' to be changed later

add_features_patientinstudy <- function(LM_data, con){
  
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^ST_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  

  message("Adding patient study features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  
  # Get patientInStudy_tb ##
  
  message("Reading Patient Study information from the database...")
  query <- "SELECT functioneelDossierNr, studyInDate, studyOutDate FROM patientInStudy_tb WHERE 1 = 1"
  patientInStudy_tb <- get_data(con, query, admission_ids)
  

  # get the list of functioneelDossierNr's who are in a study and their start & end date
  # put overlapped study time together
  
  ST_patientInStudy <- patientInStudy_tb %>%
    select(functioneelDossierNr, studyInDate, studyOutDate) %>%
    unique() %>%
    arrange(functioneelDossierNr, studyInDate) %>%
    group_by(functioneelDossierNr) %>%
    mutate(study_group = group_overlap(studyInDate, studyOutDate)) %>%
    ungroup() %>% 
    group_by(functioneelDossierNr, study_group) %>% 
    mutate(start_study = min_quiet(studyInDate),
           end_study = max_quiet(studyOutDate)) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, start_study, end_study) %>%
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode,  LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>%
    # it should be merged with LMs to identify whether a patient is in study or not
    filter(LM_end_time >= start_study, LM_end_time <= end_study) %>%
    mutate(ST_is_in_study = 1) 
    
    

  LM_data <- LM_data %>%
    left_join(ST_patientInStudy, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    select(-c("start_study", "end_study")) %>%
    mutate(ST_is_in_study = if_else(is.na(ST_is_in_study), 0, ST_is_in_study))
  
  
  message(sprintf("whether the patient is in a clinical study added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  
  return(LM_data)
}

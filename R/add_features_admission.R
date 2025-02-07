#' Adds features related to patient (age, gender, ...) and admission (referral, admission source ...) to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_admission <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^PAT_|^ADM_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding patient and admission baseline features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get admission data
  # ------------------
  
  message("Reading Patient baseline Information from the database...")
  query <- "SELECT functioneelDossierNr, age, ageInWeeks, gender, 
            placeBeforeAdmissionCode, admissionType,
            reasonForAdmission, referredBy, isUnplannedReadmission, 
            numberOfEmergencyAdmissionsInPast180days,
            HospitalStartDate, numberOfAdmissionsInPast180days
            FROM PMbaseline_tb WHERE 1 = 1"
  baseline_data <- get_data(con, query, admission_ids)
  
  # Map features
  # ------------
  
  message("Adding admission features...")
  
  baseline_data <- baseline_data %>% 
    rename(ADM_unplanned_readmission = isUnplannedReadmission,
           ADM_nr_emergency_adm_past_180_days = numberOfEmergencyAdmissionsInPast180days,
           ADM_nr_adm_past_180_days = numberOfAdmissionsInPast180days) %>% 
    mutate(ADM_admission_source = map_names(placeBeforeAdmissionCode, dict_place_before_admission),
           ADM_admission_source = if_else(ADM_admission_source == "NA", NA_character_, ADM_admission_source),
           ADM_admission_type = map_names(admissionType, dict_admission_type),
           ADM_admission_type = if_else(ADM_admission_type == "NA", NA_character_, ADM_admission_type),
           ADM_admission_reason = map_names(reasonForAdmission, dict_admission_reason),
           ADM_admission_reason = if_else(ADM_admission_reason == "NA", NA_character_, ADM_admission_reason),
           ADM_admission_referral = map_names(referredBy, dict_referral),
           ADM_admission_referral = if_else(ADM_admission_referral == "NA", NA_character_, ADM_admission_referral),
           PAT_gender_M = as.numeric(map_names(gender, dict_gender))) %>% 
    distinct(functioneelDossierNr, age, ageInWeeks, PAT_gender_M, ADM_admission_source, 
             ADM_admission_type, ADM_admission_reason, ADM_admission_referral, 
             ADM_unplanned_readmission, ADM_nr_emergency_adm_past_180_days, ADM_nr_adm_past_180_days,
             HospitalStartDate) %>% 
    rename(PAT_age = age,
           PAT_age_in_weeks = ageInWeeks)

  # give warning if the mapping returns any UNKNOWN_VALUE
  n_unknown <- baseline_data %>% 
    filter(ADM_admission_source == "UNKNOWN_VALUE") %>% 
    dim %>% `[`(1)
  if (n_unknown > 0) {
    message(sprintf("Admission source could not be mapped for %s records. These will be considered missing (NAs)"))
  }
  
  n_unknown <- baseline_data %>% 
    filter(ADM_admission_source == "UNKNOWN_VALUE") %>% 
    dim %>% `[`(1)
  if (n_unknown > 0) {
    message(sprintf("Admission type could not be mapped for %s records. These will be considered missing (NAs)"))
  }
  
  n_unknown <- baseline_data %>% 
    filter(ADM_admission_reason == "UNKNOWN_VALUE") %>% 
    dim %>% `[`(1)
  if (n_unknown > 0) {
    message(sprintf("Admission reason could not be mapped for %s records. These will be considered missing (NAs)"))
  }
  
  n_unknown <- baseline_data %>% 
    filter(ADM_admission_referral == "UNKNOWN_VALUE") %>% 
    dim %>% `[`(1)
  if (n_unknown > 0) {
    message(sprintf("Admission referral could not be mapped for %s records. These will be considered missing (NAs)"))
  }
  
  # replace UNKNOWN_VALUE with NA (missing)
  baseline_data <- baseline_data %>% 
    mutate(ADM_admission_source = if_else(ADM_admission_source == "UNKNOWN_VALUE", NA_character_, ADM_admission_source),
           ADM_admission_type = if_else(ADM_admission_type == "UNKNOWN_VALUE", NA_character_, ADM_admission_type),
           ADM_admission_reason = if_else(ADM_admission_reason == "UNKNOWN_VALUE", NA_character_, ADM_admission_reason),
           ADM_admission_referral = if_else(ADM_admission_referral == "UNKNOWN_VALUE", NA_character_, ADM_admission_referral))
  
  # make the columns binary
  baseline_data <- baseline_data %>%
    make_col_binary_all(c("ADM_admission_source", "ADM_admission_type", "ADM_admission_reason", "ADM_admission_referral")) %>% 
    select(-c(ADM_admission_source, ADM_admission_type, ADM_admission_reason, ADM_admission_referral,
              ADM_admission_source_binary_all_Other, ADM_admission_type_binary_all_Other, ADM_admission_reason_binary_all_Other, 
              ADM_admission_referral_binary_all_Other))
  
  # Merge and calculate ADM_admissiontocatheter
  # -------------------------------------------
  
  LM_data <- LM_data %>% 
    left_join(baseline_data, by = "functioneelDossierNr") %>%  
    mutate(ADM_admission_to_catheter = as.numeric(difftime(CAT_start_time_episode, 
                                                           HospitalStartDate, units = "days"))) %>%
    select(-c("HospitalStartDate"))
  
  message(sprintf("Patient and admission baseline features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
}

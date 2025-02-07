# IMPUTATION CONFIG

# vars_to_keep: selected variables based on domain knowledge, literature and missingness (of Shan's first choice)


vars_to_keep <- c("functioneelDossierNr", "CAT_catheter_episode", "LM", 
                  "PAT_gender_M", "PAT_age", "CLABSI_history", "MB_other_infection_than_BSI_during_window", "MS_is_ICU_unit",
                  "CAT_catheter_type_binary_all_CVC", "CAT_catheter_type_binary_all_Port_a_cath",
                  "CAT_catheter_type_binary_all_Tunneled_CVC", "CAT_catheter_type_binary_all_PICC", "CAT_catheter_type_binary_all_Dialysis_CVC",
                  "CAT_catheter_location_binary_all_Collarbone", "CAT_catheter_location_binary_all_Neck", "CAT_lumens_flushed",
                  "CAT_bandage_observation_binary_all_Normal", "CAT_bandage_observation_binary_all_Red", 
                  "CAT_bandage_observation_binary_all_Bloody_or_Moist", "CAT_bandage_observation_binary_all_Other_Hema_Pus_Loose_Necro",
                  "CAT_bandage_type_binary_last_polyurethane", "CAT_bandage_type_binary_last_gauze",
                  "LAB_glucose_last", "LAB_glucose_arterial_last", "LAB_pH_last", "LAB_CRP_last", "LAB_Hemoglobine_last",
                  "LAB_WBC_Monocytes_last", "LAB_WBC_Neutrophils_last", "LAB_WBC_count_last", "LAB_Platelet_count_last",
                  "COM_lymphoma_before_LM", "COM_PATH_tumor_before_LM", "COM_PATH_transplant_before_LM",
                  "MED_TPN", "MED_L2_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE", "MED_L2_L01_ANTINEOPLASTIC_AGENTS",
                  "CARE_VS_respiratory_rate_last", "CARE_VS_oxygen_saturation_last",
                  "CARE_VS_SD_mean_BP_last", "CARE_VS_diastolic_BP_last", "CARE_VS_systolic_BP_last",
                  "CARE_VS_heart_rate_max", "CARE_VS_temperature_max", "CARE_VS_MV",
                  "CARE_WND_wound_type_binary_all_suture", "CARE_WND_wound_type_binary_all_open_wound"
)



vars_to_impute <- c(#demographics 
  
  "PAT_age", 
  
  # most influential predictor without missingness
  
  "MED_7d_TPN", "MS_is_ICU_unit", 
  
  "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS", "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",
  
  "CAT_catheter_type_binary_all_CVC", "CAT_catheter_type_binary_all_Tunneled_CVC", "CAT_catheter_type_binary_all_Dialysis_CVC",
  
  "CAT_catheter_type_binary_all_Port_a_cath", "CAT_catheter_type_binary_all_PICC", 
  
  # combine Port_a_cath & Tunneled (long-term catheter)
  
  
  
  # other influential predictor with missingness (based on LASSO)
  
  # various missing pattern shall be considered
  
  "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC", "CAT_lumens_Dialysis_CVC", "CAT_lumens_Port_a_cath", "CAT_lumens_PICC",
  
  # "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC" etc. should be combined
  
  "CARE_NEU_GCS_score_last",
  
  "CARE_VS_temperature_max",
  
  "LAB_is_neutropenia",
  
  "ADM_admission_source_binary_all_Home",
  
  "LAB_urea_last",
  
  "LAB_creatinine_last",
  
  "LAB_bilirubin_last",
  
  "LAB_ciclosporin_last",
  
  "LAB_pH_last",
  
  "MS_medical_specialty_bin_drop_Cardiac",
  
  "MS_medical_specialty_bin_drop_Traumatology",
  
  "MS_medical_specialty_bin_drop_Pediatrics",
  
  # "MS_medical_specialty_bin_drop_Internal_Medicine",
  
  # "MS_medical_specialty_bin_drop_Abdomen",
  
  # "MS_medical_specialty_bin_drop_Endocrinology",
  
  "MS_medical_specialty_bin_drop_Hematology"
  
)

vars_impute_selected <- c(#demographics 
  
  "PAT_age", 
  
  # most influential predictor without missingness
  
  "MED_7d_TPN", "MS_is_ICU_unit", 
  
  "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS", "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",
  
  "CAT_catheter_type_binary_all_CVC", "CAT_catheter_type_binary_all_Tunneled_CVC", "CAT_catheter_type_binary_all_Dialysis_CVC",
  
  "CAT_catheter_type_binary_all_Port_a_cath", "CAT_catheter_type_binary_all_PICC", 
  
  # combine Port_a_cath & Tunneled (long-term catheter)
  
  
  
  # other influential predictor with missingness (based on LASSO)
  
  # various missing pattern shall be considered
  
  "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC", "CAT_lumens_Dialysis_CVC", "CAT_lumens_Port_a_cath", "CAT_lumens_PICC",
  
  # "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC" etc. should be combined
  
  "CARE_NEU_GCS_score_last",
  
  "CARE_VS_temperature_max",
  
  "LAB_is_neutropenia",
  
  "ADM_admission_source_binary_all_Home",
  
  "LAB_urea_last",
  
  "LAB_creatinine_last",
  
  "LAB_bilirubin_last",
  
  "LAB_ciclosporin_last",
  
  "LAB_pH_last",
  
  "MS_medical_specialty"
)


# new list after adding fold_id


new_vars_to_impute <- c(#demographics 
  
  "PAT_age", 
  
  # most influential predictor without missingness
  
  "MED_7d_TPN", "MS_is_ICU_unit", 
  
  "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS", "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",
  
  "CAT_catheter_type_binary_all_CVC", "CAT_catheter_type_binary_all_Tunneled_CVC", "CAT_catheter_type_binary_all_Dialysis_CVC",
  
  "CAT_catheter_type_binary_all_Port_a_cath", "CAT_catheter_type_binary_all_PICC", 
  
  # combine Port_a_cath & Tunneled (long-term catheter)

  
  # other influential predictor with missingness (based on LASSO)
  
  # various missing pattern shall be considered
  
  "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC", "CAT_lumens_Dialysis_CVC", "CAT_lumens_Port_a_cath", "CAT_lumens_PICC",
  
  # "CAT_lumens_CVC", "CAT_lumens_Tunneled_CVC" etc. should be combined
  
  "CARE_NEU_GCS_score_last",
  "LAB_is_neutropenia",
  "LAB_urea_last",
  "LAB_bilirubin_last",
  "CARE_VS_temperature_max",
  "MS_medical_specialty_bin_drop_Cardiac",
  "LAB_creatinine_last",
  "MS_medical_specialty_bin_drop_Internal_Medicine",
  "MS_medical_specialty_bin_drop_Endocrinology",
  "MS_medical_specialty_bin_drop_Abdomen",
  "MS_medical_specialty_bin_drop_Hematology",
  "MS_medical_specialty_bin_drop_Oncology",
  "LAB_pH_last")
  





# functions to recall: classify_variables ......
# dropped level changes: Traumatology

classify_variables <- function(data) {
  vars_impute_mode <- character(0)  # Initialize vector for variables to impute using mode
  vars_impute_median <- character(0)  # Initialize vector for variables to impute using median
  
  for (col in names(data)) {
    unique_vals <- unique(data[[col]])
    
    if (any(is.na(unique_vals))) {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }
    
    if (is.integer(data[[col]]) || is.numeric(data[[col]])) {
      if (all(unique_vals == 0 | unique_vals == 1)) {
        vars_impute_mode <- c(vars_impute_mode, col)
      } else {
        vars_impute_median <- c(vars_impute_median, col)
      }
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      vars_impute_mode <- c(vars_impute_mode, col)
    }
  }
  
  list(
    vars_impute_mode = vars_impute_mode,
    vars_impute_median = vars_impute_median
  )
}

impute_median <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  ifelse(is.na(x), median_value, x)
}


# Function to impute missing values with mode for a single variable within each group
impute_mode <- function(x) {
  # Check the class of the input vector
  class_x <- class(x)
  
  if (all(is.na(x))) {
    # If all values are NA, return as is
    return(x)
  } else {
    # Compute the mode value
    mode_value <- names(sort(table(x), decreasing = TRUE))[1]
    
    # Impute missing values with mode value while preserving the class
    if (class_x == "integer") {
      return(ifelse(is.na(x), as.integer(mode_value), x))
    } else if (class_x == "numeric") {
      return(ifelse(is.na(x), as.numeric(mode_value), x))
    } else if (class_x == "character") {
      return(ifelse(is.na(x), as.character(mode_value), x))
    } else {
      return(x)  # Return as is for other classes
    }
  }
}


# Mode function
Mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  ux <- unique(x)
  if (length(ux) == 0) return(NA)  # Return NA if all values are NA
  ux[which.max(tabulate(match(x, ux)))]
}









# files location
# --------------

data_path_train_clean <- "data_for_models/2014_2017/"
data_path_train_dyn_miss <- "data_for_models/2014_2017/outer_missing/"
data_path_train_dyn_complete_MF <- "data_for_models/2014_2017/outer_imputed/MFP/"
data_path_train_dyn_complete_LOCF <- "data_for_models/2014_2017/outer_imputed/LOCF/"

data_path_test_clean <- "data_for_models/2018_2020/"
data_path_test_dyn_miss <- "data_for_models/2018_2020/outer_missing/"
data_path_test_dyn_complete_MF <- "data_for_models/2018_2020/outer_imputed/MFP/"
data_path_test_dyn_complete_LOCF <- "data_for_models/2018_2020/outer_imputed/LOCF/"

# predictions
preds_path_train_CV <- "predictions/train_CV/"
preds_path_test <- "predictions/test/"

# var imp
var_imp_path_train_CV <- "variable_importance/train_CV/"
var_imp_path_train <- "variable_importance/train/"

# models (hyperparam and all)
models_path_train <- "models_train/"

models_path_train_CV <- paste0(models_path_train, "train_CV/")
models_path_train <- paste0(models_path_train, "train/")

# subsamples dir for trees in random forests
inbag_subsamp_dir_train <- "data_for_models/2014_2017/subsamples_train/"
subsamp_file_name_prefix_train <- paste0(inbag_subsamp_dir_train, "inbag_subsamp_train")

inbag_subsamp_dir_train_CV <- "data_for_models/2014_2017/outer_imputed/subsamples_train_CV/"
subsamp_file_name_prefix_train_CV <- paste0(inbag_subsamp_dir_train_CV, "inbag_subsamp_train_CV")

# results path
results_path <- "results_train_test/"

# DYNAMIC config
# --------------

max_LM_eval_train <- 30

max_LM_train <- 298

# conf matrix at 0.5%, 1%, 2%, 3%, 4%, 5%
# conf matrix at 15%, 20%, 25%
thresholds_train <- c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.15, 0.2, 0.25)

# features included in models

# used for CS model
feature_set_1 <- c("LM",
                   "MED_L5_7d_B05BA10_combinations",
                   "CAT_consecutive_current_days_CVC",
                   "CAT_consecutive_current_days_Tunneled_CVC",
                   "CAT_consecutive_current_days_Port_a_cath",
                   "CAT_consecutive_current_days_PICC",
                   "CAT_consecutive_current_days_Dialysis_CVC", 
                   "MB_other_infection_than_BSI_during_window",
                   "CARE_NEU_GCS_score_last",
                   "LAB_RBC_count_last",
                   "ADM_admission_to_catheter",
                   "MS_medical_specialty_bin_drop_Burns",
                   "MS_medical_specialty_bin_drop_Cardiac",
                   "MS_medical_specialty_bin_drop_ICU",
                   "MS_medical_specialty_bin_drop_Pediatrics",
                   "MS_medical_specialty_bin_drop_Thoracic_Surgery",
                   "MS_medical_specialty_bin_drop_Traumatology",
                   "MS_net_OR_time_before_catheter",
                   "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS",
                   "MED_7d_number_of_IV_med",
                   "CAT_days_since_last_bandage_obs",
                   "LAB_CK_last",
                   "LAB_pH_last",
                   "LAB_WBC_count_last",
                   "MED_7d_number_of_ORAL_med",
                   "PAT_age",
                   "CAT_nr_bandage_obersations",
                   "CARE_ISO_binary_all_source_isolation",
                   "CARE_VS_heart_rate_max",
                   "LAB_Platelet_count_last",
                   "LAB_creatinine_clearance_last",
                   "CAT_lumens_flushed",
                   "LAB_vancomycine_last",
                   "CAT_days_since_last_tube_change",
                   "CAT_needle_length_max",
                   "LAB_pO2_last",
                   "LAB_ferritin_last",
                   "LAB_SPE_albumin_alpha_1_globulin_last",
                   "CARE_VS_temperature_max",
                   "LAB_aspergillus_ag_last",
                   "LAB_urea_last",
                   "LAB_WBC_Neutrophils_last",
                   "LAB_LDH_last",
                   "LAB_TSH_last",
                   "LAB_creatinine_last",
                   "CARE_VS_respiratory_rate_last",
                   "LAB_bilirubin_last",
                   "MED_L2_7d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",
                   "CARE_PHY_drain",
                   "LAB_D_dimer_last",
                   "ADM_nr_adm_past_180_days",
                   "LAB_natrium_last",
                   "LAB_glucose_last",
                   "LAB_glucose_arterial_last",
                   "MS_net_OR_time_before_LM",
                   "LAB_O2_saturation_last",
                   "LAB_CRP_last",
                   "MS_ICU_time_before_catheter")

# used for ML models (larger feature set) and for missing data imputation
feature_set_2 <- c("ADM_admission_to_catheter",
                   "ADM_admission_type_binary_all_Emergency",
                   "ADM_nr_adm_past_180_days",
                   "ADM_nr_emergency_adm_past_180_days",
                   "ADM_unplanned_readmission",
                   #"admission specialty / ward", # to be calculaed
                   "CARE_ICU_ECMO",
                   # "CARE_ICU_percent_body_burned_max", # only keep BURN unit
                   "CARE_ISO_binary_all_protective_isolation",
                   "CARE_ISO_binary_all_source_isolation",
                   "CARE_NEU_GCS_score_last",
                   "CARE_PHY_BMI",
                   "CARE_PHY_drain",
                   "CARE_SAF_freedom_restriction_categorical_last",
                   "CARE_SAF_mobility_assistance_binary_all_no_help",
                   "CARE_SYM_pruritus_binary_all_YES",
                   "CARE_SYM_RASS_max",
                   "CARE_VS_breathing_aid",
                   "CARE_VS_CVP_last",
                   "CARE_VS_CVP_measured",
                   "CARE_VS_diastolic_BP_last",
                   "CARE_VS_heart_rate_max",
                   "CARE_VS_MV",
                   "CARE_VS_oxygen_saturation_last",
                   "CARE_VS_respiratory_rate_last",
                   "CARE_VS_systolic_BP_last",
                   "CARE_VS_temperature_max",
                   "CARE_WND_wound_type_binary_all_closed_wound",
                   "CARE_WND_wound_type_binary_all_open_wound",
                   "CARE_WND_wound_type_binary_all_post_suture",
                   "CARE_WND_wound_type_binary_all_suture",
                   "CAT_bandage_change",
                   "CAT_bandage_observation_binary_all_Bloody_or_Moist",
                   "CAT_bandage_observation_binary_all_Normal",
                   "CAT_bandage_observation_binary_all_Other_Hema_Pus_Loose_Necro",
                   "CAT_bandage_observation_binary_all_Red",
                   "CAT_bandage_type_binary_last_gauze",
                   # "CAT_bandage_type_binary_last_polyurethane", # keep only gauze
                   "CAT_catheter_episode",
                   "CAT_catheter_location_binary_all_Arm",
                   "CAT_catheter_location_binary_all_Collarbone",
                   "CAT_catheter_location_binary_all_Groin",
                   "CAT_catheter_location_binary_all_Navel",
                   "CAT_catheter_location_binary_all_Neck",
                   "CAT_catheter_location_binary_all_Other",
                   "CAT_catheter_location_binary_all_dialysis_unknown",
                   "CAT_catheter_placement_binary_all_bedside",
                   "CAT_catheter_placement_binary_all_OR",
                   "CAT_consecutive_current_days_CVC",
                   "CAT_consecutive_current_days_Dialysis_CVC",
                   "CAT_consecutive_current_days_PICC",
                   "CAT_consecutive_current_days_Port_a_cath",
                   "CAT_consecutive_current_days_Tunneled_CVC",
                   "CAT_days_since_last_bandage_changed",
                   "CAT_days_since_last_bandage_obs",
                   "CAT_days_since_last_tube_change",
                   "CAT_lumens_flushed",
                   "CAT_lumens_total",
                   "CAT_needle_length_max",
                   "CAT_nr_bandage_obersations",
                   "CAT_number_central_lines",
                   "CAT_number_peripheral_catheters",
                   "CAT_tube_change",
                   "CLABSI_history",
                   "COM_arterial_occlusive_disease_before_LM",
                   "COM_COPD_before_LM",
                   "COM_heart_failure_before_LM",
                   "COM_HIV_before_LM",
                   "COM_lymphoma_before_LM",
                   "COM_ORG_heart_and_circulatory_system_before_LM",
                   "GEN_LM_month_categ",
                   "HC_101_Coronary_atherosclerosis_and_other_heart_disease",
                   "HC_106_Cardiac_dysrhythmias",
                   "HC_117_Other_circulatory_disease",
                   "HC_155_Other_gastrointestinal_disorders",
                   "HC_158_Chronic_kidney_disease",
                   "HC_238_Complications_of_surgical_procedures_or_medical_care",
                   "HC_253_Allergic_reactions",
                   "HC_2617_Adverse_effects_of_medical_drugs",
                   "HC_3_Bacterial_infection_unspecified_site",
                   "HC_42_Secondary_malignancies",
                   "HC_45_Maintenance_chemotherapy_radiotherapy",
                   "HC_53_Disorders_of_lipid_metabolism",
                   "HC_55_Fluid_and_electrolyte_disorders",
                   "HC_58_Other_nutritional_endocrine_and_metabolic_disorders",
                   "HC_59_Deficiency_and_other_anemia",
                   "HC_96_Heart_valve_disorders",
                   "HC_98_Essential_hypertension",
                   "LAB_ALT_last",
                   "LAB_APTT_last",
                   "LAB_aspergillus_ag_last",
                   "LAB_AST_last",
                   "LAB_bilirubin_last",
                   "LAB_CK_last",
                   "LAB_creatinine_clearance_last",
                   "LAB_creatinine_last",
                   "LAB_CRP_last",
                   "LAB_D_dimer_last",
                   "LAB_ferritin_last",
                   "LAB_fibrinogen_last",
                   "LAB_glucose_arterial_last",
                   "LAB_glucose_last",
                   "LAB_haematocrit_last",
                   "LAB_Hemoglobine_last",
                   "LAB_is_neutropenia",
                   "LAB_LDH_last",
                   "LAB_natrium_last",
                   "LAB_NT_proBNP_last",
                   "LAB_O2_saturation_last",
                   "LAB_pH_last",
                   "LAB_Platelet_count_last",
                   "LAB_pO2_last",
                   "LAB_potassium_last",
                   "LAB_PT_INR_last",
                   "LAB_PT_percent_last",
                   "LAB_PT_sec_last",
                   "LAB_RBC_count_last",
                   "LAB_SPE_albumin_alpha_1_globulin_last",
                   "LAB_troponine_T_last",
                   "LAB_TSH_last",
                   "LAB_urea_last",
                   "LAB_vancomycine_last",
                   "LAB_WBC_count_last",
                   "LAB_WBC_Monocytes_last",
                   "LAB_WBC_Neutrophils_last",
                   "LM",
                   "MB_infection_time_window_binary_all_blood",
                   "MB_infection_time_window_binary_all_catheter",
                   "MB_infection_time_window_binary_all_deep_tissue",
                   "MB_infection_time_window_binary_all_drain",
                   "MB_infection_time_window_binary_all_GI",
                   "MB_infection_time_window_binary_all_lung",
                   "MB_infection_time_window_binary_all_skin",
                   "MB_infection_time_window_binary_all_sputum",
                   "MB_infection_time_window_binary_all_urogen",
                   "MB_number_contaminants_in_blood",
                   "MB_other_infection_than_BSI_during_window",
                   "MED_7d_CITRA_LOCK",
                   "MED_7d_immunoglobulins",
                   "MED_7d_number_of_IV_med",
                   "MED_7d_number_of_ORAL_med",
                   "MED_7d_VANCO_CEFTA_LOCK",
                   "MED_L2_7d_H02_CORTICOSTEROIDS_FOR_SYSTEMIC_USE",
                   "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",
                   "MED_L2_7d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",
                   "MED_L2_7d_J04_ANTIMYCOBACTERIALS",
                   "MED_L2_7d_J05_ANTIVIRALS_FOR_SYSTEMIC_USE",
                   "MED_L2_7d_J06_IMMUNE_SERA_AND_IMMUNOGLOBULINS",
                   "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS",
                   "MED_L2_7d_L04_IMMUNOSUPPRESSANTS",
                   "MED_L5_7d_B05BA01_amino_acids",
                   "MED_L5_7d_B05BA02_fat_emulsions",
                   "MED_L5_7d_B05BA03_carbohydrates",
                   "MED_L5_7d_B05BA10_combinations",
                   "MS_alternative_flag",
                   "MS_ICU_time_before_catheter",
                   "MS_ICU_time_before_LM",
                   "MS_is_24h_net_OR_unit",
                   "MS_is_ICU_unit",
                   "MS_medical_specialty",
                   "MS_net_OR_time_before_catheter",
                   "MS_net_OR_time_before_LM",
                   "MS_physical_ward",
                   "PAT_age",
                   "PAT_gender_M",
                   "CARE_PHY_weight_mean", # for BMI
                   "CARE_PHY_length_mean",
                   "CAT_lumens_PICC", "CAT_lumens_CVC", # for total lumens
                   "CAT_lumens_Tunneled_CVC",
                   "CAT_lumens_Dialysis_CVC",
                   "CAT_lumens_Port_a_cath")

# missing data imputation config
# ------------------------------

# configure columns for which we don't allow missingess
cols_not_missing_train <- c("functioneelDossierNr",                                      
                            "CAT_catheter_episode",                                       
                            "LM",                                                         
                            "eventtime",                                                  
                            "type",                                                       
                            "ADM_admission_to_catheter",                                  
                            "ADM_nr_adm_past_180_days",                                   
                            "ADM_nr_emergency_adm_past_180_days",                         
                            "ADM_unplanned_readmission",                                  
                            "CARE_ICU_ECMO",                                              
                            "CARE_ISO_binary_all_protective_isolation",                   
                            "CARE_ISO_binary_all_source_isolation",                       
                            "CARE_PHY_drain",                                             
                            "CARE_SYM_pruritus_binary_all_YES",                           
                            "CARE_VS_breathing_aid",                                      
                            "CARE_VS_CVP_measured",                                       
                            "CARE_VS_MV",                                                 
                            "CAT_bandage_change",                                         
                            "CAT_catheter_location_binary_all_Arm",                       
                            "CAT_catheter_location_binary_all_Collarbone",                
                            "CAT_catheter_location_binary_all_Groin",                     
                            "CAT_catheter_location_binary_all_Navel",                     
                            "CAT_catheter_location_binary_all_Neck",                      
                            "CAT_catheter_location_binary_all_Other",                     
                            "CAT_catheter_placement_binary_all_bedside",                  
                            "CAT_catheter_placement_binary_all_OR",                       
                            "CAT_consecutive_current_days_CVC",                           
                            "CAT_consecutive_current_days_Dialysis_CVC",                  
                            "CAT_consecutive_current_days_PICC",                          
                            "CAT_consecutive_current_days_Port_a_cath",                   
                            "CAT_consecutive_current_days_Tunneled_CVC",                  
                            "CAT_days_since_last_bandage_changed",                        
                            "CAT_days_since_last_bandage_obs",                            
                            "CAT_days_since_last_tube_change",                            
                            "CAT_nr_bandage_obersations",                                 
                            "CAT_number_central_lines",                                   
                            "CAT_number_peripheral_catheters",                            
                            "CAT_tube_change",                                            
                            "CLABSI_history",                                             
                            "COM_arterial_occlusive_disease_before_LM",                   
                            "COM_COPD_before_LM",                                         
                            "COM_heart_failure_before_LM",                                
                            "COM_HIV_before_LM",                                          
                            "COM_lymphoma_before_LM",                                     
                            "COM_ORG_heart_and_circulatory_system_before_LM",             
                            "GEN_LM_month_categ",                                         
                            "HC_101_Coronary_atherosclerosis_and_other_heart_disease",    
                            "HC_106_Cardiac_dysrhythmias",                                
                            "HC_117_Other_circulatory_disease",                           
                            "HC_155_Other_gastrointestinal_disorders",                    
                            "HC_158_Chronic_kidney_disease",                              
                            "HC_238_Complications_of_surgical_procedures_or_medical_care",
                            "HC_253_Allergic_reactions",                                  
                            "HC_2617_Adverse_effects_of_medical_drugs",                   
                            "HC_3_Bacterial_infection_unspecified_site",                  
                            "HC_42_Secondary_malignancies",                               
                            "HC_45_Maintenance_chemotherapy_radiotherapy",                
                            "HC_53_Disorders_of_lipid_metabolism",                        
                            "HC_55_Fluid_and_electrolyte_disorders",                      
                            "HC_58_Other_nutritional_endocrine_and_metabolic_disorders",  
                            "HC_59_Deficiency_and_other_anemia",                          
                            "HC_96_Heart_valve_disorders",                                
                            "HC_98_Essential_hypertension",                               
                            "MB_infection_time_window_binary_all_blood",                  
                            "MB_infection_time_window_binary_all_catheter",               
                            "MB_infection_time_window_binary_all_deep_tissue",            
                            "MB_infection_time_window_binary_all_drain",                  
                            "MB_infection_time_window_binary_all_GI",                     
                            "MB_infection_time_window_binary_all_lung",                   
                            "MB_infection_time_window_binary_all_skin",                   
                            "MB_infection_time_window_binary_all_sputum",                 
                            "MB_infection_time_window_binary_all_urogen",                 
                            "MB_number_contaminants_in_blood",                            
                            "MB_other_infection_than_BSI_during_window",                  
                            "MED_7d_CITRA_LOCK",                                          
                            "MED_7d_immunoglobulins",                                     
                            "MED_7d_number_of_IV_med",                                    
                            "MED_7d_number_of_ORAL_med",                                  
                            "MED_7d_VANCO_CEFTA_LOCK",                                    
                            "MED_L2_7d_H02_CORTICOSTEROIDS_FOR_SYSTEMIC_USE",             
                            "MED_L2_7d_J01_ANTIBACTERIALS_FOR_SYSTEMIC_USE",              
                            "MED_L2_7d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",                
                            "MED_L2_7d_J04_ANTIMYCOBACTERIALS",                           
                            "MED_L2_7d_J05_ANTIVIRALS_FOR_SYSTEMIC_USE",                  
                            "MED_L2_7d_J06_IMMUNE_SERA_AND_IMMUNOGLOBULINS",              
                            "MED_L2_7d_L01_ANTINEOPLASTIC_AGENTS",                        
                            "MED_L2_7d_L04_IMMUNOSUPPRESSANTS",                           
                            "MED_L5_7d_B05BA01_amino_acids",                              
                            "MED_L5_7d_B05BA02_fat_emulsions",                            
                            "MED_L5_7d_B05BA03_carbohydrates",                            
                            "MED_L5_7d_B05BA10_combinations",                             
                            "MS_ICU_time_before_catheter",                                
                            "MS_is_24h_net_OR_unit",                                      
                            "MS_is_ICU_unit",                                             
                            "MS_net_OR_time_before_catheter",                             
                            "MS_net_OR_time_before_LM",                                   
                            "PAT_gender_M",                                               
                            "CAT_lumens_total",                                           
                            "MS_total_ICU_time_before_LM")





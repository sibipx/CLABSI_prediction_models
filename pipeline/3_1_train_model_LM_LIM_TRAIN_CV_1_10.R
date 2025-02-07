# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# config for this model
model <- "LM_LIM"
model_type <- "MF_CS"
horizon <- "7d"
cv <- "CV_1_10"
path_data_complete <- "data_for_models/2014_2017/outer_imputed/MFP/"
# selected variables
list_of_predictors <- c("LM_1",
                       "LM_2",
                       "MED_L5_7d_B05BA10_combinations",
                       "MED_L5_7d_B05BA10_combinations_LM",
                       "CAT_consecutive_current_days_CVC",
                       "CAT_consecutive_current_days_Tunneled_CVC",
                       "CAT_consecutive_current_days_Port_a_cath",
                       "CAT_consecutive_current_days_PICC",
                       "CAT_consecutive_current_days_Dialysis_CVC", 
                       "MB_other_infection_than_BSI_during_window",
                       "CARE_NEU_GCS_score_last",
                       "CARE_NEU_GCS_score_last_LM",
                       "LAB_RBC_count_last",
                       "LAB_RBC_count_last_LM",
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
                       "LAB_CK_last_log",
                       "LAB_pH_last",
                       "LAB_WBC_count_last_log",
                       "MED_7d_number_of_ORAL_med",
                       "PAT_age",
                       "CAT_nr_bandage_obersations",
                       "CARE_ISO_binary_all_source_isolation",
                       "CARE_VS_heart_rate_max",
                       "LAB_Platelet_count_last",
                       "LAB_creatinine_clearance_last",
                       "CAT_lumens_flushed",
                       "LAB_vancomycine_last_log",
                       "CAT_days_since_last_tube_change",
                       "CAT_needle_length_max",
                       "LAB_pO2_last",
                       "LAB_ferritin_last_log",
                       "LAB_SPE_albumin_alpha_1_globulin_last",
                       "CARE_VS_temperature_max",
                       "LAB_aspergillus_ag_last_log",
                       "LAB_urea_last_log",
                       "LAB_WBC_Neutrophils_last_log",
                       "LAB_LDH_last_log",
                       "LAB_TSH_last_log",
                       "LAB_creatinine_last_log",
                       "CARE_VS_respiratory_rate_last",
                       "LAB_bilirubin_last_log",
                       "MED_L2_7d_J02_ANTIMYCOTICS_FOR_SYSTEMIC_USE",
                       "CARE_PHY_drain",
                       "LAB_D_dimer_last_log",
                       "ADM_nr_adm_past_180_days",
                       "LAB_natrium_last",
                       "LAB_glucose_last",
                       "LAB_glucose_arterial_last",
                       "MS_net_OR_time_before_LM",
                       "LAB_O2_saturation_last_log",
                       "LAB_CRP_last_log",
                       "MS_ICU_time_before_catheter")


preds_name <- paste("preds", model, model_type, horizon, cv, sep = "_")
results_name <- paste("results", model, model_type, horizon, cv, sep = "_")
coefs_name <- paste("coefs", model, model_type, horizon, cv, sep = "_")

# get filenames for imputed datasets 
datasets_files <- list.files(path_data_complete, 
                             recursive = TRUE, full.names = TRUE)
train_files <- datasets_files[str_detect(datasets_files, "fold_in")]
test_files <- datasets_files[str_detect(datasets_files, "fold_out")]

# for time saving purpose, select only the first 10 list files
# train_files <- train_files[1:10]
# test_files <- test_files[1:10]

# keep results
predictions <- init_preds_TRAIN_TEST()
results <- init_results_TRAIN_TEST()
coefs <- init_coefs_TRAIN_TEST()

# build model for each df
for (f in train_files){
  
  print(f)
  start_time <- Sys.time()
  
  # load data
  load(f) # loads train data named data_train
  test_file <- str_replace(f, "fold_in", "fold_out") # corresponding test set file
  load(test_file) 
  
  # binarize
  
  cat_cols <- "MS_medical_specialty"
  
  bin_model <- data_fold_in_imp %>% 
    make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))
  
  data_fold_in_imp_bin <- bin_model$data
  
  
  bin_model <- data_fold_out_imp %>% 
    make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))
  
  data_fold_out_imp_bin <- bin_model$data
  
  
  
  # apply administrative censoring to our dataset due to the nature of sliding prediction window
  # assume the prediction time horizon is set as 7 days
  
  # outcome: Surv(eventtime, type)
  
  data_fold_in_imp_bin <- data_fold_in_imp_bin %>%
    mutate(type = if_else((type == "CLABSI"|type == "Death"|type == "Discharge") & eventtime > LM + 7, "Censored", type),
           id = paste(functioneelDossierNr, CAT_catheter_episode, sep = "_"),
           Tstart = LM,
           eventtime = ifelse(eventtime <= LM + 7, eventtime, LM + 7)) %>%
    filter(!eventtime <= LM) %>% 
    mutate(type = if_else(type == "Censored", 0, if_else(type == "CLABSI", 1, if_else(type == "Death", 2, 3))))
  
  
  
  data_fold_out_imp_bin <- data_fold_out_imp_bin %>%
    mutate(type = if_else((type == "CLABSI"|type == "Death"|type == "Discharge") & eventtime > LM + 7, "Censored", type),
           id = paste(functioneelDossierNr, CAT_catheter_episode, sep = "_"),
           Tstart = LM,
           eventtime = ifelse(eventtime <= LM + 7, eventtime, LM + 7)) %>%
    filter(!eventtime <= LM) %>% 
    mutate(type = if_else(type == "Censored", 0, if_else(type == "CLABSI", 1, if_else(type == "Death", 2, 3))))
  
  
  
  
  data_fold_in_imp_bin$LM_1 <- data_fold_in_imp_bin$LM/100
  data_fold_in_imp_bin$LM_2 <- (data_fold_in_imp_bin$LM/100)^2
  
  data_fold_out_imp_bin$LM_1 <- data_fold_out_imp_bin$LM/100
  data_fold_out_imp_bin$LM_2 <- (data_fold_out_imp_bin$LM/100)^2
  
  data_fold_in_imp_bin$MED_L5_7d_B05BA10_combinations_LM <- data_fold_in_imp_bin$MED_L5_7d_B05BA10_combinations * data_fold_in_imp_bin$LM
  
  data_fold_out_imp_bin$MED_L5_7d_B05BA10_combinations_LM <- data_fold_out_imp_bin$MED_L5_7d_B05BA10_combinations * data_fold_out_imp_bin$LM
  
  data_fold_in_imp_bin$CARE_NEU_GCS_score_last_LM <- data_fold_in_imp_bin$CARE_NEU_GCS_score_last * data_fold_in_imp_bin$LM
  
  data_fold_out_imp_bin$CARE_NEU_GCS_score_last_LM <- data_fold_out_imp_bin$CARE_NEU_GCS_score_last * data_fold_out_imp_bin$LM
  
  data_fold_in_imp_bin$LAB_RBC_count_last_LM <- data_fold_in_imp_bin$LAB_RBC_count_last * data_fold_in_imp_bin$LM
  
  data_fold_out_imp_bin$LAB_RBC_count_last_LM <- data_fold_out_imp_bin$LAB_RBC_count_last * data_fold_out_imp_bin$LM
  
  data_fold_in_imp_bin$LAB_D_dimer_last_log <- log(data_fold_in_imp_bin$LAB_D_dimer_last)
  
  data_fold_out_imp_bin$LAB_D_dimer_last_log <- log(data_fold_out_imp_bin$LAB_D_dimer_last)
  
  data_fold_in_imp_bin$LAB_CK_last_log <- log(data_fold_in_imp_bin$LAB_CK_last)
  
  data_fold_out_imp_bin$LAB_CK_last_log <- log(data_fold_out_imp_bin$LAB_CK_last)
  
  data_fold_in_imp_bin$LAB_WBC_count_last_log <- log(data_fold_in_imp_bin$LAB_WBC_count_last)
  
  data_fold_out_imp_bin$LAB_WBC_count_last_log <- log(data_fold_out_imp_bin$LAB_WBC_count_last)
  
  data_fold_in_imp_bin$LAB_ferritin_last_log <- log(data_fold_in_imp_bin$LAB_ferritin_last)
  
  data_fold_out_imp_bin$LAB_ferritin_last_log <- log(data_fold_out_imp_bin$LAB_ferritin_last)
  
  data_fold_in_imp_bin$LAB_urea_last_log <- log(data_fold_in_imp_bin$LAB_urea_last)
  
  data_fold_out_imp_bin$LAB_urea_last_log <- log(data_fold_out_imp_bin$LAB_urea_last)
  
  data_fold_in_imp_bin$LAB_LDH_last_log <- log(data_fold_in_imp_bin$LAB_LDH_last)
  
  data_fold_out_imp_bin$LAB_LDH_last_log <- log(data_fold_out_imp_bin$LAB_LDH_last)
  
  data_fold_in_imp_bin$LAB_TSH_last_log <- log(data_fold_in_imp_bin$LAB_TSH_last)
  
  data_fold_out_imp_bin$LAB_TSH_last_log <- log(data_fold_out_imp_bin$LAB_TSH_last)
  
  data_fold_in_imp_bin$LAB_CRP_last_log <- log(data_fold_in_imp_bin$LAB_CRP_last)
  
  data_fold_out_imp_bin$LAB_CRP_last_log <- log(data_fold_out_imp_bin$LAB_CRP_last)
  
  data_fold_in_imp_bin$LAB_O2_saturation_last_log <- log(data_fold_in_imp_bin$LAB_O2_saturation_last)
  
  data_fold_out_imp_bin$LAB_O2_saturation_last_log <- log(data_fold_out_imp_bin$LAB_O2_saturation_last)
  
  data_fold_in_imp_bin$LAB_bilirubin_last_log <- log(data_fold_in_imp_bin$LAB_bilirubin_last)
  
  data_fold_out_imp_bin$LAB_bilirubin_last_log <- log(data_fold_out_imp_bin$LAB_bilirubin_last)
  
  data_fold_in_imp_bin$LAB_creatinine_last_log <- log(data_fold_in_imp_bin$LAB_creatinine_last)
  
  data_fold_out_imp_bin$LAB_creatinine_last_log <- log(data_fold_out_imp_bin$LAB_creatinine_last)
  
  data_fold_in_imp_bin$LAB_vancomycine_last_log <- log(data_fold_in_imp_bin$LAB_vancomycine_last)
  
  data_fold_out_imp_bin$LAB_vancomycine_last_log <- log(data_fold_out_imp_bin$LAB_vancomycine_last)
  
  data_fold_in_imp_bin$LAB_WBC_Neutrophils_last_log <- log(data_fold_in_imp_bin$LAB_WBC_Neutrophils_last+0.5)
  
  data_fold_out_imp_bin$LAB_WBC_Neutrophils_last_log <- log(data_fold_out_imp_bin$LAB_WBC_Neutrophils_last+0.5)
  
  data_fold_in_imp_bin$LAB_aspergillus_ag_last_log <- log(data_fold_in_imp_bin$LAB_aspergillus_ag_last+0.02)
  
  data_fold_out_imp_bin$LAB_aspergillus_ag_last_log <- log(data_fold_out_imp_bin$LAB_aspergillus_ag_last+0.02)
  
  
  # specify the formula
  form <- paste0(" ~ cluster(id) + ",paste0(list_of_predictors, collapse = " + "))
  
  
  # fit landmark supermodel
  LMsupercs0 <- riskRegression::CSC(update.formula(prodlim::Hist(eventtime,type,Tstart)~., form), data_fold_in_imp_bin, method = "breslow")
  
  coef_cs_model <- coef(LMsupercs0$models$`Cause 1`)
  
  # predict on test set
  
  model_info <- list(
    "coefficients" = stats::coef(LMsupercs0),
    "baseline_hazards" = lapply(LMsupercs0$models, function(mod) survival::basehaz(mod, centered = FALSE)),
    "model_terms" = lapply(LMsupercs0$models, function(mod) mod[["terms"]])
  )
  
  
  data_fold_out_imp_bin$pred_test <- predictLM_CSC(model_info, list_of_predictors, newdata = data_fold_out_imp_bin, horizon = 7, primary_event = 1)
  
  # if pred_test <=0, then replace its with multinomial results
  ## lm multi
  outcome_col <- "type"   
  # specify the formula
  form <- paste0(outcome_col," ~ ",paste0(list_of_predictors, collapse = " + "))
  
  LMsupermulti0 <- nnet:::multinom(form, data_fold_in_imp_bin)
  
  data_fold_out_imp_bin$pred_test2 <- predict(LMsupermulti0, newdata = data_fold_out_imp_bin, type = "probs")[, 2]
  
  data_fold_out_imp_bin <- data_fold_out_imp_bin %>%
    mutate(pred_test = if_else(pred_test <= 0|is.na(pred_test)|pred_test > 1, pred_test2, pred_test))
  
  
  # observed risk within 7 days y_true_cat (categorical: "CLABSI", "no_CLABSI", "Discharge", "Death", "Censored")
  # observed risk within 7 days y_test (binary: 0/1)
  data_fold_out_imp_bin$y_true_cat <- if_else(data_fold_out_imp_bin$type == 1, "CLABSI", if_else(data_fold_out_imp_bin$type == 2, "Death", if_else(data_fold_out_imp_bin$type == 3, "Discharge", "Censored")))
  
  data_fold_out_imp_bin$y_test <- if_else(data_fold_out_imp_bin$type == 1, 1, 0)
  
  
  predictions <- predictions %>% 
    add_row(preds = data_fold_out_imp_bin$pred_test,
            y_true_cat = data_fold_out_imp_bin$y_true_cat,
            y_true_time = data_fold_out_imp_bin$eventtime,
            train_set = str_replace(f, path_data_complete, ""),
            test_set = str_replace(test_file, path_data_complete, ""),
            model = model,
            model_type = model_type,
            horizon = horizon,
            cv = cv,
            LM = data_fold_out_imp_bin$LM,
            functioneelDossierNr = data_fold_out_imp_bin$functioneelDossierNr,
            CAT_catheter_episode = data_fold_out_imp_bin$CAT_catheter_episode)
  
  # performance metrics of the model
  

  # Calculate per-LM metrics
  metrics_per_lm <- data_fold_out_imp_bin %>% 
    filter(LM <= 14) %>% 
    group_by(LM) %>% 
    summarise(
      AUROC = c(c_statistic(y_test, pred_test)),
      slope = calibration_slope(y_test, pred_test),
      intercept = calibration_large(y_test, pred_test),
      OE_ratio = oe_ratio(y_test, pred_test),
      ECE = ECE(y_test, pred_test),
      ECI = ECI(y_test, pred_test),
      BS = brier_score(y_test, pred_test),
      Scaled_BS = scaled_brier_score(y_test, pred_test)
    ) %>%
    pivot_longer(cols = AUROC:Scaled_BS, names_to = "metric", values_to = "value")
  
  # Calculate overall metrics across all LMs
  overall_metrics <- data_fold_out_imp_bin %>%
    summarise(
      LM = NA,
      AUROC = c(c_statistic(y_test, pred_test)),
      slope = calibration_slope(y_test, pred_test),
      intercept = calibration_large(y_test, pred_test),
      OE_ratio = oe_ratio(y_test, pred_test),
      ECE = ECE(y_test, pred_test),
      ECI = ECI(y_test, pred_test),
      BS = brier_score(y_test, pred_test),
      Scaled_BS = scaled_brier_score(y_test, pred_test)
    ) %>%
    pivot_longer(cols = AUROC:Scaled_BS, names_to = "metric", values_to = "value")
  
  # Combine per-LM metrics with overall metrics
  metrics <- bind_rows(metrics_per_lm, overall_metrics)
  
  
  results <- results %>%
    add_row(train_set = str_replace(f, path_data_complete, ""),
            test_set = str_replace(test_file, path_data_complete, ""),
            metric = metrics$metric,
            value = metrics$value,
            model = model,
            model_type = model_type,
            horizon = horizon,
            cv = cv,
            LM = metrics$LM)
  

  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
}

# save predictions 
save(predictions, file = paste0("predictions/train_CV/", preds_name))

# save(results, file = paste0("results_train_test/", results_name))
# save(results, file = paste0("playground/4_Comparing_Regression_and_Machine_Learning_Algorithms_for_Dynamic_CLABSI_Prediction/performances/", results_name))



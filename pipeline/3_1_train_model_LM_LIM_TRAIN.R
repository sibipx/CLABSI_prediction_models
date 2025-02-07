# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))


# config for this model
model <- "LM_LIM"
model_type <- "MF_CS"
horizon <- "7d"
cv <- ""
path_data_train <- paste0(data_path_train_clean, "data_train_imp")
path_data_test <- paste0(data_path_test_clean, "data_test_imp")

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

preds_name <- paste("preds", model, model_type, horizon, sep = "_")
results_name <- paste("results", model, model_type, horizon, sep = "_")
coefs_name <- paste("coefs", model, model_type, horizon, sep = "_")

load(path_data_train) # loads train data named data_train_imp
load(path_data_test) # loads test data named data_test_imp

# keep results
predictions <- init_preds_TRAIN_TEST()
results <- init_results_TRAIN_TEST()
coefs <- init_coefs_TRAIN_TEST()

# also create s, s^2, ICU*s, ICU*s^2

data_train_imp$LM_1 <- data_train_imp$LM/100
data_train_imp$LM_2 <- (data_train_imp$LM/100)^2

data_test_imp$LM_1 <- data_test_imp$LM/100
data_test_imp$LM_2 <- (data_test_imp$LM/100)^2

data_train_imp$MED_L5_7d_B05BA10_combinations_LM <- data_train_imp$MED_L5_7d_B05BA10_combinations * data_train_imp$LM

data_test_imp$MED_L5_7d_B05BA10_combinations_LM <- data_test_imp$MED_L5_7d_B05BA10_combinations * data_test_imp$LM

data_train_imp$CARE_NEU_GCS_score_last_LM <- data_train_imp$CARE_NEU_GCS_score_last * data_train_imp$LM

data_test_imp$CARE_NEU_GCS_score_last_LM <- data_test_imp$CARE_NEU_GCS_score_last * data_test_imp$LM

data_train_imp$LAB_RBC_count_last_LM <- data_train_imp$LAB_RBC_count_last * data_train_imp$LM

data_test_imp$LAB_RBC_count_last_LM <- data_test_imp$LAB_RBC_count_last * data_test_imp$LM

data_train_imp$LAB_D_dimer_last_log <- log(data_train_imp$LAB_D_dimer_last)

data_test_imp$LAB_D_dimer_last_log <- log(data_test_imp$LAB_D_dimer_last)

data_train_imp$LAB_CK_last_log <- log(data_train_imp$LAB_CK_last)

data_test_imp$LAB_CK_last_log <- log(data_test_imp$LAB_CK_last)

data_train_imp$LAB_WBC_count_last_log <- log(data_train_imp$LAB_WBC_count_last)

data_test_imp$LAB_WBC_count_last_log <- log(data_test_imp$LAB_WBC_count_last)

data_train_imp$LAB_ferritin_last_log <- log(data_train_imp$LAB_ferritin_last)

data_test_imp$LAB_ferritin_last_log <- log(data_test_imp$LAB_ferritin_last)

data_train_imp$LAB_urea_last_log <- log(data_train_imp$LAB_urea_last)

data_test_imp$LAB_urea_last_log <- log(data_test_imp$LAB_urea_last)

data_train_imp$LAB_LDH_last_log <- log(data_train_imp$LAB_LDH_last)

data_test_imp$LAB_LDH_last_log <- log(data_test_imp$LAB_LDH_last)

data_train_imp$LAB_TSH_last_log <- log(data_train_imp$LAB_TSH_last)

data_test_imp$LAB_TSH_last_log <- log(data_test_imp$LAB_TSH_last)

data_train_imp$LAB_CRP_last_log <- log(data_train_imp$LAB_CRP_last)

data_test_imp$LAB_CRP_last_log <- log(data_test_imp$LAB_CRP_last)

data_train_imp$LAB_O2_saturation_last_log <- log(data_train_imp$LAB_O2_saturation_last)

data_test_imp$LAB_O2_saturation_last_log <- log(data_test_imp$LAB_O2_saturation_last)

data_train_imp$LAB_bilirubin_last_log <- log(data_train_imp$LAB_bilirubin_last)

data_test_imp$LAB_bilirubin_last_log <- log(data_test_imp$LAB_bilirubin_last)

data_train_imp$LAB_creatinine_last_log <- log(data_train_imp$LAB_creatinine_last)

data_test_imp$LAB_creatinine_last_log <- log(data_test_imp$LAB_creatinine_last)

data_train_imp$LAB_vancomycine_last_log <- log(data_train_imp$LAB_vancomycine_last)

data_test_imp$LAB_vancomycine_last_log <- log(data_test_imp$LAB_vancomycine_last)

data_train_imp$LAB_WBC_Neutrophils_last_log <- log(data_train_imp$LAB_WBC_Neutrophils_last+0.5)

data_test_imp$LAB_WBC_Neutrophils_last_log <- log(data_test_imp$LAB_WBC_Neutrophils_last+0.5)

data_train_imp$LAB_aspergillus_ag_last_log <- log(data_train_imp$LAB_aspergillus_ag_last+0.02)

data_test_imp$LAB_aspergillus_ag_last_log <- log(data_test_imp$LAB_aspergillus_ag_last+0.02)



cat_cols <- "MS_medical_specialty"

bin_model <- data_train_imp %>% 
  make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))

data_train_bin <- bin_model$data


bin_model <- data_test_imp %>% 
  make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))

data_test_bin <- bin_model$data

# specify the formula
form <- paste0(" ~ cluster(id) + ",paste0(list_of_predictors, collapse = " + "))


# fit landmark supermodel

data_train_bin <- data_train_bin %>%
  mutate(type = if_else((type == "CLABSI"|type == "Death"|type == "Discharge") & eventtime > LM + 7, "Censored", type),
         id = paste(functioneelDossierNr, CAT_catheter_episode, sep = "_"),
         Tstart = LM,
         eventtime = ifelse(eventtime <= LM + 7, eventtime, LM + 7)) %>%
  filter(!eventtime <= LM) %>% 
  mutate(type = if_else(type == "Censored", 0, if_else(type == "CLABSI", 1, if_else(type == "Death", 2, 3))))



data_test_bin <- data_test_bin %>%
  mutate(type = if_else((type == "CLABSI"|type == "Death"|type == "Discharge") & eventtime > LM + 7, "Censored", type),
         id = paste(functioneelDossierNr, CAT_catheter_episode, sep = "_"),
         Tstart = LM,
         eventtime = ifelse(eventtime <= LM + 7, eventtime, LM + 7)) %>%
  filter(!eventtime <= LM) %>% 
  mutate(type = if_else(type == "Censored", 0, if_else(type == "CLABSI", 1, if_else(type == "Death", 2, 3))))


# simple
# use LMsupercs0

LMsupercs0 <- riskRegression::CSC(update.formula(prodlim::Hist(eventtime,type,Tstart)~., form), data_train_bin, method = "breslow")

coef_cs_model <- coef(LMsupercs0$models$`Cause 1`)

# predict on test set

model_info <- list(
  "coefficients" = stats::coef(LMsupercs0),
  "baseline_hazards" = lapply(LMsupercs0$models, function(mod) survival::basehaz(mod, centered = FALSE)),
  "model_terms" = lapply(LMsupercs0$models, function(mod) mod[["terms"]])
)



data_test_bin$pred_test <- predictLM_CSC(model_info, list_of_predictors,newdata = data_test_bin, horizon = 7, primary_event = 1)
# data_test_bin$pred_test_death <- predictLM_CSC(LMsupercs0, model_info, newdata = data_test_bin, horizon = 7, primary_event = 2)
# data_test_bin$pred_test_discharge <- predictLM_CSC(LMsupercs0, model_info, newdata = data_test_bin, horizon = 7, primary_event = 3)


# if pred_test <=0, then replace its with multinomial results
## lm multi
outcome_col <- "type"   
# specify the formula
form <- paste0(outcome_col," ~ ",paste0(list_of_predictors, collapse = " + "))

LMsupermulti0 <- nnet:::multinom(form, data_train_bin)

data_test_bin$pred_test2 <- predict(LMsupermulti0, newdata = data_test_bin, type = "probs")[, 2]


data_test_bin <- data_test_bin %>%
  mutate(pred_test = if_else(pred_test == 0, pred_test2, pred_test))

data_test_bin$y_true_cat <- if_else(data_test_bin$type == 1, "CLABSI", if_else(data_test_bin$type == 2, "Death", if_else(data_test_bin$type == 3, "Discharge", "Censored")))

data_test_bin$y_test <- if_else(data_test_bin$type == 1, 1, 0)


predictions <- predictions %>% 
  add_row(preds = data_test_bin$pred_test,
          y_true_cat = data_test_bin$y_true_cat,
          y_true_time = data_test_bin$eventtime,
          train_set = paste0("data_train_imp"),
          test_set = paste0("data_test_imp"),
          model = model,
          model_type = model_type,
          horizon = horizon,
          cv = cv,
          LM = data_test_bin$LM,
          functioneelDossierNr = data_test_bin$functioneelDossierNr,
          CAT_catheter_episode = data_test_bin$CAT_catheter_episode)

# performance metrics of the model


# Calculate per-LM metrics
metrics_per_lm <- data_test_bin %>% 
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
overall_metrics <- data_test_bin %>%
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
  add_row(train_set = paste0("data_train_imp"),
          test_set = paste0("data_test_imp"),
          metric = metrics$metric,
          value = metrics$value,
          model = model,
          model_type = model_type,
          horizon = horizon,
          cv = cv,
          LM = metrics$LM)

save(predictions, file = paste0("predictions/test/", preds_name))

# save(results, file = paste0("results_train_test/", results_name))
# save(results, file = paste0("playground/4_Comparing_Regression_and_Machine_Learning_Algorithms_for_Dynamic_CLABSI_Prediction/performances/", results_name))

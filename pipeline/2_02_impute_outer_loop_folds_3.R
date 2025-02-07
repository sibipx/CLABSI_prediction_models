# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

set.seed(2024)

MF_errors <- tibble(train_set = character(),
                    iteration = integer(), 
                    variable = character(),  
                    MSE = double(),        
                    NMSE = double(),       
                    MER = double(),       
                    macro_F1 = double(),  
                    F1_score = double())

# load folds
load(paste0(data_path_train_dyn_miss, "folds_in_adm_ids"))
load(paste0(data_path_train_dyn_miss, "folds_out_adm_ids"))

# load train data
load(paste0(data_path_train_clean, "data_train_clean"))

for (i in 5:6){
  
  print(sprintf("fold: %s", i))
  start_time <- Sys.time()
  
  # impute fold in
  data_fold_in <- data_train_clean %>% 
    filter(functioneelDossierNr %in% folds_in_adm_ids[[i]])
  
  imp_obj <- fit_impute_dynamic_MF_train(data_fold_in, 
                                         cols_not_missing = cols_not_missing_train,
                                         verbose = FALSE)
  
  data_fold_in_imp <- imp_obj$data
  
  # keep MF errors for later exploration
  MF_err <- imp_obj$imp_obj$imp_obj_MF$OOB_err
  MF_err$train_set <- sprintf("fold_in_%s", str_pad(i, 2, side = "left", pad = "0"))
  MF_errors <- MF_errors %>% add_row(MF_err)
  
  # impute fold out
  data_fold_out <- data_train_clean %>% 
    filter(functioneelDossierNr %in% folds_out_adm_ids[[i]])
  
  data_fold_out_imp <- predict_impute_dynamic_MF_train(imp_obj$imp_obj, data_fold_out,
                                                       verbose = FALSE)
  
  # remove BMI from train and test (problematic due to recording errors)
  data_fold_in_imp$CARE_PHY_BMI <- NULL
  data_fold_out_imp$CARE_PHY_BMI <- NULL
  
  # save on disk
  save(data_fold_in_imp, 
       file = sprintf("%sdata_fold_in_%s", data_path_train_dyn_complete_MF,
                      str_pad(i, 2, side = "left", pad = "0")))
  
  save(data_fold_out_imp, 
       file = sprintf("%sdata_fold_out_%s", data_path_train_dyn_complete_MF,
                      str_pad(i, 2, side = "left", pad = "0")))
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
}

MF_errors_5_6 <- MF_errors

save(MF_errors_5_6, file = paste0(models_path_train, "MF_errors_DYN_folds_5_6"))


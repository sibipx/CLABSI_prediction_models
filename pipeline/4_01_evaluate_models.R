# libraries and functions
# -----------------------

# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# load LM data for TEMPORAL evaluation per year and per ward
load("data_for_models/2018_2020/LM_data.RData")
LM_data_18_20 <- LM_data
load("data_for_models/2014_2017/LM_data.RData")
LM_data_14_17 <- LM_data
# LM_data <- LM_data_18_20 %>% 
#   add_row(LM_data_14_17)

# get all models to evaluate
pred_files <- list.files(preds_path_test, full.names=TRUE)

pred_files_OOB <- pred_files[str_detect(pred_files, "OOB_")]
pred_files <- pred_files[!pred_files %in% pred_files_OOB]

pred_files_OOB <- pred_files_OOB[str_detect(pred_files_OOB, "preds_")]
pred_files <- pred_files[str_detect(pred_files, "preds_")]

model_names <- str_replace(pred_files, preds_path_test, "")
model_names <- str_replace(model_names, "preds_", "")

# get CV files for all models
pred_files_CV <- list.files(preds_path_train_CV, full.names=TRUE)
pred_files_CV_OOB <- pred_files_CV[str_detect(pred_files_CV, "OOB_")]
pred_files_CV <- pred_files_CV[!pred_files_CV %in% pred_files_CV_OOB]

# TO RUN A SINGLE MODEL, overwrite here the model name
# model_names <- c("SL_NNLS")

model_names <- model_names[!model_names %in% c("LM_LIM_MF_CS_no_dimer_7d")]

for (mod in model_names){
  
  start_time <- Sys.time()
  print(mod)
  print(sprintf("Started at: %s", start_time))
  
  if (!str_detect(mod, "SL_")){ # only temporal evaluation for SL
    
    # INTERNAL evaluation
    # -------------------
    pred_files_CV_mod <- pred_files_CV[str_detect(pred_files_CV, mod)]
    
    # predictions on folds
    all_preds <- list()
    for (preds_f in pred_files_CV_mod){
      load(preds_f)
      all_preds <- c(all_preds, list(predictions))
    }
    predictions <- do.call("rbind", all_preds)
    
    results_CV <- evaluate_mod(predictions, thresholds = thresholds_train)
    
    rm(predictions, all_preds)
    
    # predictions OOB (only for RF models)
    if (str_detect(mod, "RF_")){
      pred_files_CV_OOB_mod <- pred_files_CV_OOB[str_detect(pred_files_CV_OOB, mod)]
      all_preds <- list()
      for (preds_f in pred_files_CV_OOB_mod){
        load(preds_f)
        all_preds <- c(all_preds, list(OOB_predictions))
      }
      predictions_OOB <- do.call("rbind", all_preds)
      
      results_CV_OOB <- evaluate_mod(predictions_OOB,
                                     thresholds = thresholds_train,
                                     only_stacked = TRUE,
                                     full_calib = FALSE)
    }
  }
  
  # TEMPORAL evaluation
  # -------------------
  
  # get model files
  preds_name <- paste("preds", mod, sep = "_")
  preds_file <- paste0(preds_path_test, preds_name)
  
  load(preds_file)
  
  results_TEMP <- evaluate_mod(predictions, thresholds = thresholds_train, eval_per_ward = TRUE)
  
  # predictions OOB (only for RF models)
  if (str_detect(mod, "RF_")){
    pred_files_TEMP_OOB_mod <- pred_files_OOB[str_detect(pred_files_OOB, mod)]
    load(pred_files_TEMP_OOB_mod)
    
    results_TEMP_OOB <- evaluate_mod(OOB_predictions,
                                     thresholds = thresholds_train,
                                     only_stacked = TRUE,
                                     full_calib = FALSE)
  }
  
  # save results
  results <- results_TEMP
  if (!str_detect(mod, "SL_")){ # only temporal evaluation for SL
    results <- results %>% 
      add_row(results_CV)
  }
  
  results_name <- paste("results", mod, sep = "_")
  results_file <- paste0(results_path, results_name)
  
  save(results, file = results_file)
  
  # save results OOB (only for RF models)
  if (str_detect(mod, "RF_")){
    
    results_OOB <- results_CV_OOB %>%
      add_row(results_TEMP_OOB)
    
    results_name_OOB <- paste("results_OOB", mod, sep = "_")
    results_file_OOB <- paste0(results_path, results_name_OOB)
    
    save(results_OOB, file = results_file_OOB)
  }
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
}


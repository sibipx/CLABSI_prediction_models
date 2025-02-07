# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

library(ranger)
library(tuneRanger)
library(randomForestSRC)

# run separate scripts per fold - just to get the results faster than in a loop
fold_in <- "data_fold_in_06"

# config for this model
# model name should be unique across files!!!
model <- "RF_LIM_MF_multinom_7d"
print(model)

# model config
model_type <- "multinomial"
var_sel <- "LIM"
horizon <- 7
iters_design <- 30
iters_optim <- 70
tuning_metric <- "logloss"
importance_type <- "permutation"

# build file names
preds_name <- paste("preds", model, "CV", sep = "_")
results_name <- paste("results", model, "CV", sep = "_")
OOB_preds_name <- paste("OOB_preds", model, "CV", sep = "_")
OOB_results_name <- paste("OOB_results", model, "CV", sep = "_")
var_imp_name <- paste("var_imp", model, "CV", sep = "_")
hyperparams_name <- paste("hyperparams", model, "CV", sep = "_")
timings_name <- paste("timings", model, "CV", sep = "_")

preds_file <- paste0(preds_path_train_CV, preds_name)
results_file <- paste0(preds_path_train_CV, results_name)
OOB_results_file <- paste0(preds_path_train_CV, OOB_results_name)
OOB_preds_file <- paste0(preds_path_train_CV, OOB_preds_name)

var_imp_file <- paste0(var_imp_path_train_CV, var_imp_name)

hyperparams_file <- paste0(models_path_train_CV, hyperparams_name)
timings_file <- paste0(models_path_train_CV, timings_name)


# get filenames for imputed datasets
datasets_files <- list.files(data_path_train_dyn_complete_MF,
                             recursive = TRUE, full.names = TRUE)
train_files <- datasets_files[str_detect(datasets_files, "data_fold_in_")]
test_files <- datasets_files[str_detect(datasets_files, "data_fold_out_")]

inbags_prefix <- subsamp_file_name_prefix_train_CV

# keep results
predictions <- init_preds()
var_imp <- init_var_imp()
hyperparams <- init_hyperparams()
timings <- init_timings()
OOB_predictions <- init_preds()

# load data
f <- train_files[str_detect(train_files, fold_in)]

load(f) # loads train data named data_fold_in_imp
test_file <- str_replace(f, "fold_in", "fold_out") # corresponding test set file
load(test_file) 

fold <- str_sub(f, -2, -1)

name_train_set <- str_replace(f, data_path_train_dyn_complete_MF, "")
name_test_set <- str_replace(test_file, data_path_train_dyn_complete_MF, "")

set.seed(2024)

results <- build_RF_model(data_fold_in_imp, data_fold_out_imp, # train, test
                          model,
                          model_type = model_type,
                          var_sel = var_sel,
                          horizon = horizon,
                          iters_design = iters_design,
                          iters_optim = iters_optim,
                          tuning_metric = tuning_metric, 
                          importance_type = importance_type, 
                          inbags_prefix = paste(inbags_prefix, "foldin", fold, sep = "_"),
                          name_train_set = name_train_set,
                          name_test_set = name_test_set)

predictions <- predictions %>% add_row(results$predictions)
var_imp <- var_imp %>% add_row(results$var_imp)
hyperparams <- hyperparams %>% add_row(results$hyperparams)
timings <- timings %>% add_row(results$timings)
OOB_predictions <- OOB_predictions %>% add_row(results$OOB_predictions)

# save results on disk
preds_file <- paste(preds_file, fold, sep = "_")
var_imp_file <- paste(var_imp_file, fold, sep = "_")
hyperparams_file <- paste(hyperparams_file, fold, sep = "_")
timings_file <- paste(timings_file, fold, sep = "_")
OOB_preds_file <- paste(OOB_preds_file, fold, sep = "_")

save(predictions, file = preds_file)
save(var_imp, file = var_imp_file)
save(hyperparams, file = hyperparams_file)
save(timings, file = timings_file)
save(OOB_predictions, file = OOB_preds_file)


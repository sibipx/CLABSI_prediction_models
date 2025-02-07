# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

library(ranger)
library(tuneRanger)
library(randomForestSRC)

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
preds_name <- paste("preds", model, sep = "_")
results_name <- paste("results", model, sep = "_")
OOB_preds_name <- paste("OOB_preds", model, sep = "_")
OOB_results_name <- paste("OOB_results", model, sep = "_")
var_imp_name <- paste("var_imp", model, sep = "_")
hyperparams_name <- paste("hyperparams", model, sep = "_")
timings_name <- paste("timings", model, sep = "_")

preds_file <- paste0(preds_path_test, preds_name)
results_file <- paste0(preds_path_test, results_name)
OOB_results_file <- paste0(preds_path_test, OOB_results_name)
OOB_preds_file <- paste0(preds_path_test, OOB_preds_name)

var_imp_file <- paste0(var_imp_path_train, var_imp_name)

hyperparams_file <- paste0(models_path_train, hyperparams_name)
timings_file <- paste0(models_path_train, timings_name)

# get training and test sets
path_data_train <- paste0(data_path_train_clean, "data_train_imp")
path_data_test <- paste0(data_path_test_clean, "data_test_imp")

load(path_data_train) # loads train data named data_train_imp
load(path_data_test) # loads test data named data_test_imp

inbags_prefix <- subsamp_file_name_prefix_train

name_train_set <- "train"
name_test_set <- "test"

set.seed(2024)

results <- build_RF_model(data_train_imp, data_test_imp,
                          model,
                          model_type = model_type,
                          var_sel = var_sel,
                          horizon = horizon,
                          iters_design = iters_design,
                          iters_optim = iters_optim,
                          tuning_metric = tuning_metric, 
                          importance_type = importance_type, 
                          inbags_prefix = inbags_prefix,
                          name_train_set = name_train_set,
                          name_test_set = name_test_set,
                          save_model = TRUE)

# save results on disk
predictions <- results$predictions
save(predictions, file = preds_file)
var_imp <- results$var_imp
save(var_imp, file = var_imp_file)
hyperparams <- results$hyperparams
save(hyperparams, file = hyperparams_file)
timings <- results$timings
save(timings, file = timings_file)
OOB_predictions <- results$OOB_predictions
save(OOB_predictions, file = OOB_preds_file)
# saved model
model_res <- results$model
model_file <- paste0(models_path_train, "model_", model)
save(model_res, file = model_file)


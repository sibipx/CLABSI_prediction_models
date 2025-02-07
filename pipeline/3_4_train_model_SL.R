# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# models to include (these should each identify a unique model)
model_names <- c("preds_LM_LIM_MF_CS_7d", 
                 "preds_RF_LIM_MF_multinom_7d",
                 "preds_RF_VST_MF_multinom_7d",
                 "preds_XGB_ALL_MF_multinom_7d",
                 "preds_XGB_LIM_MF_multinom_7")

# load individual model predictions CV
pred_files_CV <- list.files("predictions/train_CV/",
                            recursive = TRUE, full.names = TRUE)

OOB_pred_files <- pred_files_CV[str_detect(pred_files_CV, "OOB_")]
pred_files_CV <- pred_files_CV[!pred_files_CV %in% OOB_pred_files]

pred_files_ids <- sapply(model_names, function(x) str_detect(pred_files_CV, x))
pred_files_ids <- as.logical(rowSums(pred_files_ids))
pred_files_CV <- pred_files_CV[pred_files_ids]

# load CV predictions
all_CV_preds <- init_preds()
all_CV_preds$y_true_time <- NULL 
all_CV_preds$train_set <- NULL

for (f in pred_files_CV){
  load(f)
  
  preds_temp <- predictions %>% 
    select(preds, y_true_cat, test_set, model, LM, functioneelDossierNr,
           CAT_catheter_episode) %>% 
    mutate(test_set = str_replace(test_set, "/", "")) %>% 
    mutate(y_true_cat = if_else(y_true_cat == "CLABSI", "CLABSI", "no_CLABSI"))
  
  all_CV_preds <- all_CV_preds %>% add_row(preds_temp)
}

# check
all_CV_preds %>% distinct(model)
all_CV_preds %>% distinct(test_set)
all_CV_preds %>% distinct(y_true_cat)

# loads individual model predictions on TEST set
pred_files_test <- list.files("predictions/test/",
                              recursive = TRUE, full.names = TRUE)
OOB_pred_files <- pred_files_test[str_detect(pred_files_test, "OOB_")]
pred_files_test <- pred_files_test[!pred_files_test %in% OOB_pred_files]

pred_files_ids <- sapply(model_names, function(x) str_detect(pred_files_test, x))
pred_files_ids <- as.logical(rowSums(pred_files_ids))
pred_files_test <- pred_files_test[pred_files_ids]

# load TEST predictions
all_test_preds <- init_preds()
all_test_preds$y_true_time <- NULL 
all_test_preds$train_set <- NULL

for (f in pred_files_test){
  load(f)
  
  preds_temp <- predictions %>% 
    select(preds, y_true_cat, test_set, model, LM, functioneelDossierNr,
           CAT_catheter_episode) %>% 
    mutate(test_set = str_replace(test_set, "/", ""),
           test_set = if_else(test_set == "data_test_imp", "test", test_set)) %>% 
    mutate(y_true_cat = if_else(y_true_cat == "CLABSI", "CLABSI", "no_CLABSI"))
  
  all_test_preds <- all_test_preds %>% add_row(preds_temp)
}

# check
all_test_preds %>% distinct(model)
all_test_preds %>% distinct(test_set)
all_test_preds %>% distinct(y_true_cat)

# calculate individual performance of each model (predictions "pooled together")
logloss_models <- all_CV_preds %>% 
  mutate(y_true_0_1 = if_else(y_true_cat == "CLABSI", 1, 0)) %>% 
  group_by(model) %>% 
  summarise(log_loss = log_loss(y_true_0_1, preds)) %>% 
  ungroup()

logloss_models

models <- unique(all_CV_preds$model)

all_preds_spread <- all_CV_preds %>%
  mutate(y_true_cat = if_else(y_true_cat == "CLABSI", 1, 0)) %>%
  select(preds, y_true_cat, test_set,
         model, functioneelDossierNr,
         CAT_catheter_episode, LM) %>%
  pivot_wider(id_cols = c(functioneelDossierNr, CAT_catheter_episode, LM, y_true_cat, test_set),
              names_from = model, values_from = preds)  

# this should not return anything!
all_preds_spread %>%
  filter(if_any(all_of(models), is.na))

# NNLS
# ----

model <- "SL_NNLS"
print(model)

library(nnls)

y <- all_preds_spread$y_true_cat
x <- all_preds_spread %>% select(all_of(models)) %>% as.matrix()
mod_nnls <- nnls(x, y)
coefs_nnls <- mod_nnls$x 
coefs_nnls <- coefs_nnls /(sum(coefs_nnls))
coefs_nnls <- coefs_nnls %>% as.matrix() %>% t()

# calculate weighted predictions
calculate_weighted_preds <- function(weights_matrix, data, model_preds) {
  preds_matrix <- as.matrix(data[model_preds])
  weighted_preds <- weights_matrix %*% t(preds_matrix)
  return(t(weighted_preds))
}

all_weighted_preds_nnls <- calculate_weighted_preds(coefs_nnls, 
                                                    all_preds_spread, models)

log_loss_nnls <- apply(all_weighted_preds_nnls, 2, 
                       function(preds) log_loss(all_preds_spread$y_true_cat, preds))

(logloss_models <- logloss_models %>% 
    add_row(model = "nnlsSL",
            log_loss = log_loss_nnls))

# predict on test set
all_preds_test_spread <- all_test_preds %>%
  pivot_wider(id_cols = c(functioneelDossierNr, CAT_catheter_episode, LM, y_true_cat, test_set),
              names_from = model, values_from = preds)  

# these should not return anything
all_preds_test_spread %>% filter(is.na(LM_LIM ))
all_preds_test_spread %>% filter(is.na(RF_LIM_MF_multinom_7d))
all_preds_test_spread %>% filter(is.na(RF_VST_MF_multinom_7d))

preds <- calculate_weighted_preds(coefs_nnls, all_preds_test_spread, models)

predictions <- all_preds_test_spread %>% 
  select(functioneelDossierNr, CAT_catheter_episode, LM, y_true_cat, test_set) %>% 
  cbind(preds) %>% 
  mutate(model = model)

save(predictions, file = paste0(preds_path_test, "preds_", model))

# save weights
colnames(coefs_nnls) <- models

save(coefs_nnls, file = paste0(models_path_train, "coefs_", model))


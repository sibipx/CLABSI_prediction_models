
# libraries and functions
# -----------------------

# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# split training data in 10 folds
# -------------------------------

load(paste0(data_path_train_clean, "data_train_clean"))

set.seed(2024)
n_folds <- 10

folds_in <- splitTools::create_folds(data_train_clean$functioneelDossierNr, 
                                     k = n_folds, type = "grouped")
folds_in_adm_ids <- lapply(folds_in, function(x)
  unique(data_train_clean[x,"functioneelDossierNr", drop = TRUE]))

folds_out_adm_ids <- lapply(folds_in_adm_ids, function(x)
  unique(data_train_clean$functioneelDossierNr[!data_train_clean$functioneelDossierNr %in% 
                                                 x]))

# save outer folds in/out on disk
save(folds_in_adm_ids, file = paste0(data_path_train_dyn_miss, "folds_in_adm_ids"))
save(folds_out_adm_ids, file = paste0(data_path_train_dyn_miss, "folds_out_adm_ids"))


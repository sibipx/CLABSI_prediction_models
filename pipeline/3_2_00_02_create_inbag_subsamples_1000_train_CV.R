# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# get train_data
path_data_train <- paste0(data_path_train_clean, "data_train_imp")

num_trees <- 1000

sample_adm_ids <- function(adm_ids, adm_ids_unique, n){
  s <- sample(adm_ids_unique, n, replace = FALSE)
  return(as.integer(adm_ids %in% s))
}

create_inbag <- function(adm_ids, sample_fraction, num_trees){
  
  adm_ids_unique <- unique(adm_ids)
  n_id <- length(adm_ids_unique)
  
  inbags <- replicate(num_trees, 
                      sample_adm_ids(adm_ids, adm_ids_unique, round(n_id * sample_fraction)))
  
  return(inbags)
}

set.seed(2024)

# data path
path_data_complete <- data_path_train_dyn_complete_MF

# get filenames for imputed datasets 
datasets_files <- list.files(path_data_complete, 
                             recursive = TRUE, full.names = TRUE)
train_files <- datasets_files[str_detect(datasets_files, "data_fold_in")]

# create subsamples for each df and percentage (10 to 90)
for (f in train_files){
  
  print(f)
  start_time <- Sys.time()
  
  # load data
  load(f) # loads train data named data_fold_in_imp
  
  # create subsamples for 30% to 80%
  adm_ids <- data_fold_in_imp$functioneelDossierNr
  
  for (sample_fraction in 30:80){
    inbags <- create_inbag(adm_ids, sample_fraction/100, num_trees)
    
    fold <- str_sub(f, -2, -1)
    
    subsamp_file_name <- sprintf("%s_foldin_%s_%s", subsamp_file_name_prefix_train_CV,
                                 fold, sample_fraction)
    
    save(inbags, file = subsamp_file_name)
  }
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
}

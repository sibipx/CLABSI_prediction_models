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

# subsample_inbag <- function(inbag, subsample_size, len_inbags) {
#   ids <- which(inbag == 1)
#   ids_selected <- sort(sample(ids, subsample_size, replace = FALSE))
#   inbag_selected <- rep(0, len_inbags)
#   inbag_selected[ids_selected] <- 1
#   
#   return(inbag_selected)
# }

set.seed(2024)

# load train data
load(paste0(data_path_train_clean, "data_train_imp"))

# create subsamples for 30% to 80%
adm_ids <- data_train_imp$functioneelDossierNr

for (sample_fraction in 30:80){
  inbags <- create_inbag(adm_ids, sample_fraction/100, num_trees)
  
  subsamp_file_name <- paste0(subsamp_file_name_prefix_train, "_", sample_fraction)
  
  save(inbags, file = subsamp_file_name)
}

#' Adds features related to blood transfer to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_blood_transfer <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^BLD_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding BLOOD TRANSFER lab results features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get blood transfusion data
  # --------------------------
  
  message("Reading blood transfer data from the database...")
  
  query <- "SELECT functioneelDossierNr, typeBloedProduct, uitvoerDatum 
           FROM bloedTransfusions_tb WHERE 1 = 1"
  blood_data <- get_data(con, query, admission_ids)
  
  message(sprintf("Blood transfer data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # Extract features 
  # ----------------
  
  # map names
  blood_data <- blood_data %>% 
    mutate(typeBloedProduct = map_names(typeBloedProduct, dict_blood_transfer))
  
  # keep administration in the LM timeframe
  blood_data_during_LM <- blood_data %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(uitvoerDatum >= LM_start_time, 
           uitvoerDatum <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, typeBloedProduct) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(BLD_admin = list(typeBloedProduct)) %>% 
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, BLD_admin)
  
  # make binary variables
  blood_data_during_LM <- blood_data_during_LM %>% 
    make_col_binary_all(cols = c("BLD_admin")) %>% 
    select(-BLD_admin)
  
  # merge in LM data
  LM_data <- LM_data %>% 
    left_join(blood_data_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("BLD_admin_")), ~ if_else(is.na(.), 0, .)) # make 0 when not recorded
  
  message(sprintf("Blood transfer features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}





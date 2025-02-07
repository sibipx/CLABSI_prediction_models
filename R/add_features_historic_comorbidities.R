#' Adds features related to patient historic comorbidities to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_historic_comorbidities <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^HC_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding patient historic comorbidities...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Add feature - historic comorbidities
  # ------------------------------------
  
 
  his_com_str <- paste(dict_historic_comorbidities$his_com_orig, collapse = "', '")
  
  query <- sprintf("SELECT DISTINCT functioneelDossierNr, CCS_Categorie, CCS_Categorie_Omsch
           FROM historicComorbidities_revision2024_tb WHERE CCS_Categorie_Omsch in ('%s')", his_com_str)
  historic_comorbidities_tb <- get_data(con, query, admission_ids)
  # this is baseline variable
  
  historic_comorbidities_tb <- historic_comorbidities_tb %>% 
    mutate(name = map_names(CCS_Categorie_Omsch, dict_historic_comorbidities),
           value = 1) %>% 
      select(functioneelDossierNr, name, value) %>% 
    distinct() %>%
    pivot_wider(names_from = name, 
                names_prefix = "HC_",
                values_from = value,
                values_fill = 0, values_fn = function(x) 1) 
  
  # add columns if no values were found
  cols_features <- unique(dict_historic_comorbidities$his_com_new)
  cols_features <- paste0("HC_", cols_features)
  
  for (col in cols_features){
    if(!col %in% colnames(historic_comorbidities_tb)){
      historic_comorbidities_tb[,col] <- 0
    }
  }

  
  # add feature to LM data
  LM_data <- LM_data %>% 
    left_join(historic_comorbidities_tb, by =  "functioneelDossierNr") %>%
    mutate_at(vars(starts_with("HC_")), ~ if_else(is.na(.), 0, .))
  

  message(sprintf("historic comorbidity features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}


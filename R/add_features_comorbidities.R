#' Adds features related to historical patient conditions lab results to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_comorbidities <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^COM_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding comorbidities / patient conditions features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get care program data from the DB
  # ---------------------------------
  
  message("Reading care program data from the database...")
  
  # use sendTime (time recorded in the system) in the logic
  # it is safer than vanDatum which is the starts date (but there is lag sometimes)
  query <- "SELECT functioneelDossierNr, sendTime, descriptionNl
           FROM careProgram_tb WHERE 1 = 1"
  comorb_data <- get_data(con, query, admission_ids)
  
  message(sprintf("Care program data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  comorb_data <- comorb_data %>% 
    left_join(dict_pathology_organ, by = "descriptionNl")
  
  # Pathology features
  # ------------------
  
  message("Adding features - pathology...")
  
  comorb_data_pathology_before_LM <- comorb_data %>% 
    filter(!is.na(pathology)) %>% 
    mutate(pathology =  map_names(pathology, dict_pathology)) %>% 
    distinct(functioneelDossierNr, sendTime, pathology)
  
  comorb_data_pathology_before_LM <- comorb_data_pathology_before_LM %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(sendTime <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, pathology) %>% 
    pivot_wider(names_from = pathology, 
                names_prefix = "COM_PATH_",
                values_from = pathology, 
                values_fill = 0, values_fn = function(x) 1) %>% 
    rename_with(.cols = starts_with("COM_PATH"),
                .fn = function(x) paste0(x, "_before_LM"))
  
  # add columns that don't exist - this happens mostly when running on one (or few) admission IDs
  cols_COM <- paste("COM_PATH", dict_pathology$English, "before_LM", sep = "_")
  
  for (col in cols_COM){
    if(!col %in% colnames(comorb_data_pathology_before_LM)){
      comorb_data_pathology_before_LM[,col] <- 0
    }
  }
  
  # add feature to LM data; if NA make 0
  LM_data <- LM_data %>% 
    left_join(comorb_data_pathology_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("COM_PATH_")), ~ if_else(is.na(.), 0, .))
  
  # Organ features
  # --------------
  
  message("Adding features - organ...")
  
  comorb_data_organ_before_LM <- comorb_data %>% 
    filter(!is.na(organ)) %>% 
    mutate(organ =  map_names(organ, dict_organ)) %>% 
    distinct(functioneelDossierNr, sendTime, organ)
  
  comorb_data_organ_before_LM <- comorb_data_organ_before_LM %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(sendTime <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, organ) %>% 
    pivot_wider(names_from = organ, 
                names_prefix = "COM_ORG_",
                values_from = organ, 
                values_fill = 0, values_fn = function(x) 1) %>% 
    rename_with(.cols = starts_with("COM_ORG"),
                .fn = function(x) paste0(x, "_before_LM"))
  
  # add columns that don't exist - this happens mostly when running on one (or few) admission IDs
  cols_ORG <- paste("COM_ORG", dict_organ$English, "before_LM", sep = "_")
  
  for (col in cols_ORG){
    if(!col %in% colnames(comorb_data_organ_before_LM)){
      comorb_data_organ_before_LM[,col] <- 0
    }
  }
  
  # add feature to LM data; if NA make 0
  LM_data <- LM_data %>% 
    left_join(comorb_data_organ_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("COM_ORG_")), ~ if_else(is.na(.), 0, .))
  
  # Distinct comorbidites 
  # ---------------------
  
  # keep some distinct comorbidities (some come from literature review)
  comorb_data_before_LM <- comorb_data %>% 
    filter(descriptionNl %in% dict_care_program$Dutch) %>% 
    mutate(descriptionNl = map_names(descriptionNl, dict = dict_care_program))
  
  comorb_data_before_LM <- comorb_data_before_LM %>% 
    left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                 LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(sendTime <= LM_end_time) %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, descriptionNl) %>% 
    pivot_wider(names_from = descriptionNl, 
                names_prefix = "COM_",
                values_from = descriptionNl, 
                values_fill = 0, values_fn = function(x) 1) %>% 
    rename_with(.cols = starts_with("COM_"),
                .fn = function(x) paste0(x, "_before_LM"))
  
  # add columns that don't exist - this happens mostly when running on one (or few) admission IDs
  cols_COM <- paste("COM", dict_care_program$English, "before_LM", sep = "_")
  
  for (col in cols_COM){
    if(!col %in% colnames(comorb_data_before_LM)){
      comorb_data_before_LM[,col] <- 0
    }
  }
  
  # add feature to LM data 
  # if NA make 0 (if the medication is not administered before the LM)
  LM_data <- LM_data %>% 
    left_join(comorb_data_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate_at(vars(starts_with("COM_")), ~ if_else(is.na(.), 0, .))
  
  message(sprintf("Comorbidity features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}


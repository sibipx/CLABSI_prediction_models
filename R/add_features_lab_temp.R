#' Adds lab results to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_lab_temp <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^LAB_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding lab features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # Get lab data
  # ------------
  
  lab_tests <- dict_lab_tests$Dutch
  query_constraint <- sprintf(" AND test IN ('%s')", paste(lab_tests, collapse = "', '"))
  
  message("Reading lab data from the database...")
  
  # use validatietijdstip rather than order / collection time 
  # as it is sure that the result is available in the sytem at validation time
  query <- "SELECT functioneelDossierNr, validatietijdstip, test, resultaat, eenheid 
           FROM lwsData WHERE 1 = 1"
  query_complete <- paste0(query, query_constraint)
  lab_data <- get_data(con, query_complete, admission_ids)
  
  message(sprintf("Lab data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # Check and prepare lab data
  # --------------------------
  
  # check if more units per test, give warning
  unit_counts <- lab_data %>% 
    mutate(test = map_names(test, dict_lab_tests)) %>% 
    group_by(test, eenheid) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(test, eenheid) %>% 
    count() %>% 
    ungroup() %>% 
    filter(n > 1)
  
  if (dim(unit_counts)[[1]] > 0){
    message("Different units found for the following tests. Check and adapt data preparation!")
    print(unit_counts)
  }
  
  # make all lab results numeric
  lab_data <- lab_data %>% 
    mutate(resultaat = str_replace(resultaat, "^(<|>|<=|>=) *", "")) %>% 
    mutate(resultaat = as.numeric(resultaat)) %>% 
    mutate(validatietijdstip = as.POSIXct(validatietijdstip, tz = "UTC")) 
  
  # check how many are non-numeric
  lab_data_non_numeric <- lab_data %>% 
    filter(is.na(resultaat))
  
  if (nrow(lab_data_non_numeric) > 0){
    message(sprintf("There are %s results (%s) with non-numeric values for the below lab tests. These will be removed.", 
                    nrow(lab_data_non_numeric),
                    make_percent(nrow(lab_data_non_numeric)/nrow(lab_data))))
    
    print(lab_data_non_numeric %>% 
            count(test))
  }
  
  # remove the non-numeric values
  lab_data <- lab_data %>% 
    filter(!is.na(resultaat))
  
  # Feature - neutropenia
  # ---------------------
  
  # this follows the same log as in calculate_BSI.R
  lab_data_neutropenia <- lab_data %>% 
    filter(test %in% neutropenia_lab_tests) %>% 
    mutate(resultaat = resultaat * 1000) %>% 
    mutate(LAB_is_neutropenia = as.numeric(resultaat < 500)) %>% 
    distinct(functioneelDossierNr, validatietijdstip, LAB_is_neutropenia)
  
  neutropenia_during_LM <- lab_data_neutropenia %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, 
                       LM, LM_start_time, LM_end_time),
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(validatietijdstip >= LM_start_time, 
           validatietijdstip <= LM_end_time) %>% 
    select(-c(LM_start_time, LM_end_time, validatietijdstip)) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(LAB_is_neutropenia = as.numeric(any(LAB_is_neutropenia == 1))) %>% 
    ungroup() %>% 
    distinct()
  
  # merge in LM data 
  LM_data <- LM_data %>% 
    left_join(neutropenia_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  # Get lab features between 2 landmarks
  # ------------------------------------
  
  # process data in chunks and keep all chunks in a list (otherwise memory problems)
  data_chunks <- list()
  chunk_size <- 10000
  
  if (length(admission_ids) > chunk_size) {
    chunks <- split_vector(admission_ids, chunk_size)
  } else {
    chunks <- list(admission_ids)
  }
  
  message(sprintf("Processing lab data in %s chunks...", length(chunks)))
  
  # break in admission chunks
  for (i in 1:length(chunks)) {
    message(sprintf("Processing chunk %s...", i))
    
    admissions_chunk <- chunks[[i]]
    
    # MODIFIED TO TAKE LAST LAB RESULT regardless of time
    
    # join with LM_data 
    lab_data_temp <- lab_data %>% 
      filter(functioneelDossierNr %in% admissions_chunk) %>% 
      mutate(test = map_names(test, dict_lab_tests),
             test = paste0("LAB_", test)) %>% 
      left_join(LM_data %>% 
                  filter(functioneelDossierNr %in% admissions_chunk) %>% 
                  select(functioneelDossierNr, CAT_catheter_episode, 
                         LM, LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(# validatietijdstip >= LM_start_time, # modified!!!
             validatietijdstip <= LM_end_time) %>% 
      select(-c(LM_start_time, LM_end_time))
    
    # keep temporary df in list
    data_chunks[[i]] <- lab_data_temp
    
    # free up memory
    rm(lab_data_temp, admissions_chunk)
    gc()
    
  }
  
  # merge the data chunks together
  lab_data_during_LM <- data_chunks %>% reduce(rbind)
  rm(data_chunks)
  gc()
  
  # keep columns
  cols_lab <- unique(lab_data_during_LM$test)
  
  # keep values in a list
  lab_data_during_LM <- lab_data_during_LM %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM, test) %>% 
    arrange(validatietijdstip) %>% 
    mutate(value_list = list(resultaat)) %>% 
    ungroup() %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, test, value_list) %>% 
    distinct()
  
  # create columns for each feature
  lab_data_during_LM <- lab_data_during_LM %>% 
    pivot_wider(names_from = test, values_from = value_list) 
  
  # add columns if no values were found
  for (col in paste0("LAB_", dict_lab_tests$English, "_last")){
    if(!col %in% colnames(lab_data_during_LM)){
      lab_data_during_LM[,col] <- NA_real_
    }
  }
  
  # make features (last)
  lab_data_during_LM <- lab_data_during_LM %>%  
    make_col_numeric_last(cols_lab) %>% 
    select(-all_of(cols_lab))
  
  # merge in LM data 
  LM_data <- LM_data %>% 
    left_join(lab_data_during_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  
  message(sprintf("Lab features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
  
}
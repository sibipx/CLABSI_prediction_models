#' Adds features related to medication administered to the landmark dataframe 
#'
#' @param LM_data landamrk data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' @param ATC_L02_to_include a vector of ATC codes - level 2 - for which to extract features. If not specified, all ocdes are included.
#' 
#' @return LM_data - landmark dataframe with additional features

add_features_medication <- function(LM_data, con, dict_ATC, 
                                    ATC_L02_to_include, 
                                    ATC_codes_to_include){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^MED_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  message("Adding medication features...")
  
  # prepare dictionaries for diffierent levels
  dict_ATC_level_1 <- dict_ATC %>% 
    mutate(code_length = nchar(Code)) %>% 
    filter(code_length == 1) %>% 
    distinct(Code, Description)
  
  dict_ATC_level_2 <- dict_ATC %>% 
    mutate(code_length = nchar(Code)) %>% 
    filter(code_length == 3) %>% 
    distinct(Code, Description)
  
  dict_ATC_level_3 <- dict_ATC %>% 
    mutate(code_length = nchar(Code)) %>% 
    filter(code_length == 4) %>% 
    distinct(Code, Description)
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # check parameter values
  # ----------------------
  
  if(missing(ATC_L02_to_include)) {
    ATC_L02_to_include <- dict_ATC_level_2$Code
  } else if (!is.vector(ATC_L02_to_include)){
    stop("ATC_L02_to_include has to be a vector of ATC codes - level 2 - to include.")
  }
  
  # Get medication data
  # -------------------
  
  message("Reading medication data from the database...")
  
  query <- "SELECT DISTINCT atcCode, functioneelDossierNr, useDate, niveau2, 
           niveau3, TPN, toedienWijzeOmschrijving, artikelOmschrijving
           FROM farmaUse_tb WHERE 1 = 1"
  med_data <- get_data(con, query, admission_ids)
  
  message(sprintf("Medication data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # medication data only has date, no hour 
  # based on the exploration of a sample of EMV data for antibiotics, most medications are prescribed / administered at 8:00
  # next 2 peaks are at 20:00 and 16:00 (more conservative choices)
  # we move all medications at 8:00
  med_data <- med_data %>% 
    mutate(useDate = ymd_h(str_c(useDate, 8, sep=' ')))
  
  # add medication for different time winodws in the past
  
  for (tw in MED_time_window){
    
    message(sprintf("Adding medication features for time window: %s...", tw))
    
    # add TPN
    # -------
    
    message("Adding features - TPN...")
    
    MED_TPN <- paste0("MED_", tw, "d_TPN")
    
    med_data_TPN <- med_data %>% 
      mutate(TPN = if_else(TPN == "Y", 1, 0)) %>% 
      filter(TPN == 1) %>% 
      distinct(functioneelDossierNr, useDate, TPN) %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   CAT_start_time_episode, CAT_end_time_episode, LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% # use window - 1 to keep consistency with other functions on using LM_start_time for filtering
      mutate(!!MED_TPN := 1) %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, !!sym(MED_TPN))
    
    # add feature to LM data 
    # if NA make 0 (if the medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_TPN, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_TPN := if_else(is.na(!!sym(MED_TPN)), 0, !!sym(MED_TPN)))
    
    # add LEVEL 2 in ATC codes
    # ------------------------
    
    message("Adding features - ATC level 2...")
    
    MED_L2_prefix <- paste0("MED_L2_", tw, "d_")
    
    # filter level 2 and make friendly names 
    med_data_ATC_level_2 <- med_data %>% 
      distinct(functioneelDossierNr, useDate, niveau2) %>% 
      filter(niveau2 %in% ATC_L02_to_include) %>% 
      mutate(niveau2_friendly = map_names(niveau2, dict = dict_ATC_level_2, flag_unkown = FALSE),
             ATC_level_2 = paste(niveau2, clean_strings(niveau2_friendly), sep = "_")) %>% 
      distinct(functioneelDossierNr, useDate, ATC_level_2) 
    
    med_data_ATC_level_2 <- med_data_ATC_level_2 %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, ATC_level_2, CAT_catheter_episode, LM) %>% 
      pivot_wider(names_from = ATC_level_2, 
                  names_prefix = MED_L2_prefix,
                  values_from = ATC_level_2, 
                  values_fill = 0, values_fn = function(x) 1) %>% 
      rename_with(.cols = starts_with(MED_L2_prefix),
                  .fn = function(x) paste0(x, ""))
    
    # add columns that don't exist - this happens mostly when running on one (or few) admission IDs
    cols_MED_level_2 <- paste(sub("_$", "", MED_L2_prefix), 
                              dict_ATC_level_2$Code[dict_ATC_level_2$Code %in% ATC_L02_to_include],
                              clean_strings(dict_ATC_level_2$Description[dict_ATC_level_2$Code %in% ATC_L02_to_include]), 
                              sep = "_")
    
    for (col in cols_MED_level_2){
      if(!col %in% colnames(med_data_ATC_level_2)){
        med_data_ATC_level_2[,col] <- 0
      }
    }
    
    # add feature to LM data 
    # if NA make 0 (if the medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_ATC_level_2, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate_at(vars(starts_with(MED_L2_prefix)), ~ if_else(is.na(.), 0, .))
    
    # add individual ATC codes
    # ------------------------
    
    if (!missing(ATC_codes_to_include)) {
      
      message("Adding features - ATC codes...")
      
      # filter ATC codes and make friendly names 
      
      # there are around 1.8% of the lines with empty ATC code (""). These are filtered.
      # a check by Pieter on the database revealed that these are mostly preparation solutions
      # the top 5 according to the query were:
      # bereiding cyto infuus; water gedistil. 1000 ml; natr chloride 0.9% 1000 ml
      # sucrose 24% fl. drinkb. opl 2ml; bereiding capsule
      
      MED_L5_prefix <- paste0("MED_L5_", tw, "d_")
      
      med_data_ATC <- med_data %>% 
        filter(atcCode != "") %>% 
        distinct(functioneelDossierNr, useDate, atcCode) %>% 
        filter(atcCode %in% ATC_codes_to_include) %>% 
        mutate(ATC_code_friendly = map_names(atcCode, dict = dict_ATC, flag_unkown = FALSE),
               ATC_code = paste(atcCode, clean_strings(ATC_code_friendly), sep = "_")) %>% 
        distinct(functioneelDossierNr, useDate, ATC_code) 
      
      med_data_ATC <- med_data_ATC %>% 
        left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                     LM_start_time, LM_end_time),
                  by = "functioneelDossierNr", multiple = "all") %>% 
        filter(useDate <= LM_end_time,
               useDate >= LM_start_time - days(tw - 1)) %>% 
        distinct(functioneelDossierNr, ATC_code, CAT_catheter_episode, LM) %>% 
        pivot_wider(names_from = ATC_code, 
                    names_prefix = MED_L5_prefix,
                    values_from = ATC_code, 
                    values_fill = 0, values_fn = function(x) 1) %>% 
        rename_with(.cols = starts_with(MED_L5_prefix),
                    .fn = function(x) paste0(x, ""))
      
      # add columns that don't exist - this happens mostly when running on one (or few) admission IDs
      cols_MED_level_5 <- paste(sub("_$", "", MED_L5_prefix), 
                                dict_ATC$Code[dict_ATC$Code %in% ATC_codes_to_include],
                                clean_strings(dict_ATC$Description[dict_ATC$Code %in% ATC_codes_to_include]), 
                                sep = "_")
      
      for (col in cols_MED_level_5){
        if(!col %in% colnames(med_data_ATC)){
          med_data_ATC[,col] <- 0
        }
      }
      
      # add feature to LM data 
      # if NA make 0 (if the medication is not administered)
      LM_data <- LM_data %>% 
        left_join(med_data_ATC, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
        mutate_at(vars(starts_with(MED_L5_prefix)), ~ if_else(is.na(.), 0, .))
    }
    
    # add VANCOLOCK
    # -------------
    
    message("Adding features - VANCOLOCK...")
    
    MED_VANCO_CEFTA_LOCK <- paste0("MED_", tw, "d_VANCO_CEFTA_LOCK")
    
    med_data_VANCO <- med_data %>% 
      filter(artikelOmschrijving %in% vanco_cefta_lock_products) %>% 
      distinct(functioneelDossierNr, useDate) %>% 
      mutate(!!MED_VANCO_CEFTA_LOCK := 1)
    
    med_data_VANCO <- med_data_VANCO %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, 
               !!sym(MED_VANCO_CEFTA_LOCK))
    
    # add feature to LM data 
    # if NA make 0 (if the medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_VANCO, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_VANCO_CEFTA_LOCK := if_else(is.na(!!sym(MED_VANCO_CEFTA_LOCK)), 
                                               0, !!sym(MED_VANCO_CEFTA_LOCK)))
    
    # add CITRA LOCK
    # --------------
    
    message("Adding features - CITRA LOCK...")
    
    MED_CITRA_LOCK <- paste0("MED_", tw, "d_CITRA_LOCK")
    
    med_data_CITRA <- med_data %>% 
      filter(artikelOmschrijving %in% citra_lock_products) %>% 
      distinct(functioneelDossierNr, useDate) %>% 
      mutate(!!MED_CITRA_LOCK := 1)
    
    med_data_CITRA <- med_data_CITRA %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, !!sym(MED_CITRA_LOCK))
    
    # add feature to LM data 
    # if NA make 0 (if the medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_CITRA, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_CITRA_LOCK := if_else(is.na(!!sym(MED_CITRA_LOCK)), 
                                         0, !!sym(MED_CITRA_LOCK)))
    
    # add immunoglobulins
    # -------------------
    
    message("Adding features - immunoglobulins...")
    
    MED_immunoglobulins <- paste0("MED_", tw, "d_immunoglobulins")
    
    # get immunoglobulins from mapping at level 2, level 3 and ATC code
    med_data_IG <- med_data %>% 
      filter(niveau2 %in% immunoglobulins_level_2) %>% 
      rbind(
        med_data %>% 
          filter(niveau3 %in% immunoglobulins_level_3) 
      ) %>% 
      distinct(functioneelDossierNr, useDate) %>% 
      mutate(!!MED_immunoglobulins := 1)
    
    med_data_IG <- med_data_IG %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, CAT_catheter_episode, LM, 
               !!sym(MED_immunoglobulins))
    
    # add feature to LM data 
    # if NA make 0 (if the medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_IG, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_immunoglobulins := if_else(is.na(!!sym(MED_immunoglobulins)), 
                                              0, !!sym(MED_immunoglobulins)))
    
    # add IV drugs (presumably administered through the central line)
    # ---------------------------------------------------------------
    
    message("Adding features - IV medication...")
    
    MED_number_of_IV_meds <- paste0("MED_", tw, "d_number_of_IV_med")
    
    med_data_IV <- med_data %>% 
      filter(atcCode != "",
             toedienWijzeOmschrijving %in% dict_med_route$Dutch[dict_med_route$English == "IV"]) %>% 
      distinct(functioneelDossierNr, useDate, atcCode) 
    
    med_data_IV <- med_data_IV %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, atcCode, CAT_catheter_episode, LM) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
      count(name = MED_number_of_IV_meds) %>% 
      ungroup()
    
    # add feature to LM data 
    # if NA make 0 (if no IV medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_IV, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_number_of_IV_meds := if_else(is.na(!!sym(MED_number_of_IV_meds)), 
                                                0L, !!sym(MED_number_of_IV_meds)))
    
    # add ORAL drugs (feedback from lit. review - unsure why?)
    # --------------------------------------------------------
    
    message("Adding features - ORAL medication...")
    
    MED_number_of_ORAL_meds <- paste0("MED_", tw, "d_number_of_ORAL_med")
    
    med_data_PO <- med_data %>% 
      filter(atcCode != "",
             toedienWijzeOmschrijving %in% dict_med_route$Dutch[dict_med_route$English == "ORAL"]) %>% 
      distinct(functioneelDossierNr, useDate, atcCode) 
    
    med_data_PO <- med_data_PO %>% 
      left_join(LM_data %>% select(functioneelDossierNr, CAT_catheter_episode, LM, 
                                   LM_start_time, LM_end_time),
                by = "functioneelDossierNr", multiple = "all") %>% 
      filter(useDate <= LM_end_time,
             useDate >= LM_start_time - days(tw - 1)) %>% 
      distinct(functioneelDossierNr, atcCode, CAT_catheter_episode, LM) %>% 
      group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
      count(name = MED_number_of_ORAL_meds) %>% 
      ungroup()
    
    # add feature to LM data 
    # if NA make 0 (if no IV medication is not administered)
    LM_data <- LM_data %>% 
      left_join(med_data_PO, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
      mutate(!!MED_number_of_ORAL_meds := if_else(is.na(!!sym(MED_number_of_ORAL_meds)), 
                                                  0L, !!sym(MED_number_of_ORAL_meds)))
    
  }
  # return LM_data
  # -------------
  
  message(sprintf("Medication features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
}
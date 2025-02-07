#' Adds patient medical specialty features (also ICU & OR) to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features
#' 
#' to be changed later


add_features_medical_specialty <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^MS_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  
  message("Adding patient medical specialty and whereabouts features...")
  
  # keep only admission ids that are in LM_data (faster)
  # ----------------------------------------------------
  
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  # ----------------------------------------------------
  
  # Get PM_whereDetail #
  
  message("Reading Patient medical specialty and whereabouts information from the database...")
  query <- "SELECT functioneelDossierNr, AC_OMSCHR, AC_AARDOMSCH, periodeVan, uurMinuut, periodeNr, afdelingOmschrijving, alternatiefBedVlag, logischBedTraject, T_inZaal, T_uitZaal, netTimeInOR, omschrijving
  FROM PMwhereDetail_tb WHERE 1 = 1"
  PM_whereDetail_tb <- get_data(con, query, admission_ids)
  message(sprintf("Patients medical specialty and whereabouts data reading finished in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # periodeVan uurMinuut consist of the timestamps
  # periodeVan --> start time stamp
  # uurMinuut shares same date as periodeVan and is the end time stamp
  # periodeTot = periodeDate + uurMinuut

  PM_whereDetail_tb <- PM_whereDetail_tb %>% 
    mutate(periodeDate = strptime(periodeVan, format = "%Y-%m-%d")) %>%
    mutate(periodeTot = as.POSIXct(paste(periodeDate, uurMinuut),tz = 'UTC')) %>% 
    select(-c(uurMinuut, periodeDate))
  
  
  # ----------------------------------------------------
  
  # Extract the medical specialty information
  # use AC_AARDOMSCH afdelingSchrijving periodeNr periodeVan uurMinuut  
  
  message("Adding patients medical specialty information...")
  
  # afdelingOmschrijving: afdeling mapped code description at landmark time --> medical specialty
  # AC_OMSCHR: AC_code mapped description at landmark time --> physical ward
  # alternatiefBedVlag: flag patient presence whose medical specialty and physical ward do not match
  
  # select patients whose AC_AARDOMSCH == "Hospitalization"
  # or they could be counted once more for imaging/operation/radiology....
  PM_where_MS <-  PM_whereDetail_tb %>% 
    mutate(AC_AARDOMSCH = map_names(AC_AARDOMSCH, dict_AC_AARDOMSCH),
           AC_OMSCHR = map_names(AC_OMSCHR, dict_AC_OMSCHR),
           AC_afdelingOmschrijving = map_names(afdelingOmschrijving, dict_AC_afdelingOmschrijving)) %>% 
    filter(AC_AARDOMSCH == "Hospitalization",
           !AC_OMSCHR %in% c("Function Measurement", "Operation")) %>%
    arrange(functioneelDossierNr, periodeNr, periodeVan) %>%
    select(functioneelDossierNr, AC_OMSCHR, AC_afdelingOmschrijving, alternatiefBedVlag, periodeVan, periodeTot, logischBedTraject) 
  
  
  # MS_medical_specialty: the medical specialty that the patient's doctor registered at each landmark time point
  # MS_physical_ward: the physical ward where the patient stayed at each landmark time point
  # MS_alternative_flag: flag patient presence whose medical specialty and physical ward do not match
  
  PM_where_MS <-  LM_data %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time) %>% 
    left_join(PM_where_MS, by = "functioneelDossierNr", multiple = "all") %>%
    filter(periodeTot >= LM_start_time & periodeVan <= LM_end_time) %>%
    arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>%
    mutate(periodeTot_now = if_else(periodeTot > LM_end_time, LM_end_time, 
                                    periodeTot)) %>%
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>%
    arrange(periodeTot_now, periodeVan, logischBedTraject) %>% 
    slice_tail() %>%
    ungroup() %>%
    mutate(MS_medical_specialty = if_else(AC_afdelingOmschrijving == "", NA_character_, AC_afdelingOmschrijving)) %>%
    #  select rows with logischBedTraject = 1 for duplicate appearance
    mutate(MS_physical_ward = if_else(AC_OMSCHR == "", NA_character_, AC_OMSCHR)) %>%
    mutate(MS_alternative_flag = if_else(alternatiefBedVlag == "TRUE", 1, 0)) %>%  # make 0 when not recorded 
    select(functioneelDossierNr, CAT_catheter_episode, LM, MS_medical_specialty, MS_physical_ward, MS_alternative_flag)

  
  LM_data <- LM_data %>%
    left_join(PM_where_MS, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM"))
  

  # ----------------------------------------------------
  
  
  
  # MS_is_ICU_unit: Is the patient now (at the exact second of the current LM) in ICU?
  # ----------------------------------------------------
  
  message("Adding ICU presence information...")
  

  PM_where_ICU <- PM_whereDetail_tb %>%
    select(functioneelDossierNr, AC_AARDOMSCH, periodeVan, periodeTot, periodeNr) %>%
    arrange(functioneelDossierNr, periodeNr) %>%
    mutate(MS_is_ICU_unit = ifelse(AC_AARDOMSCH == AARDOMSCH_ICU, 1, 0)) %>%
    filter(MS_is_ICU_unit == 1) %>%
    select(-c("periodeNr", "AC_AARDOMSCH")) %>%
    distinct()

  
  PM_where_ICU_at_landmark <- PM_where_ICU %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(LM_end_time >= periodeVan, LM_end_time <= periodeTot) %>% 
    select(-c("periodeVan", "periodeTot")) %>% 
    distinct()
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_ICU_at_landmark, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    mutate(MS_is_ICU_unit = if_else(is.na(MS_is_ICU_unit), 0, MS_is_ICU_unit))
  
  # ----------------------------------------------------
  
  
  message("Calculating time spent in ICU before catheter placement...")
  # MS_ICU_time_before_catheter: total time spent in ICU between admission to catheter placement (per catheter episode)
  # ----------------------------------------------------
  PM_where_ICU_time_before_catheter <- PM_where_ICU %>% 
    # check and combine continuous ICU time blocks
    arrange(functioneelDossierNr, periodeVan) %>%
    group_by(functioneelDossierNr) %>%
    mutate(icu_group = group_overlap(periodeVan, periodeTot)) %>%
    ungroup() %>% 
    group_by(functioneelDossierNr, icu_group) %>% 
    mutate(start_time_icu = min_quiet(periodeVan),
           end_time_icu = max_quiet(periodeTot)) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, start_time_icu, end_time_icu) %>%
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode), 
              by = "functioneelDossierNr", multiple = "all") %>%
    filter(start_time_icu <= CAT_start_time_episode) %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_ICU_time_before_catheter = if_else(end_time_icu > CAT_start_time_episode, 
                                                 difftime(CAT_start_time_episode, start_time_icu, units = "days"), 
                                                 difftime(end_time_icu, start_time_icu, units = "days")),
           MS_ICU_time_before_catheter = as.numeric(MS_ICU_time_before_catheter)) %>%
    subset(MS_ICU_time_before_catheter > 0) %>%
    group_by(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_ICU_time_before_catheter = sum(MS_ICU_time_before_catheter)) %>%
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, MS_ICU_time_before_catheter)
  # unit is days
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_ICU_time_before_catheter, by = c("functioneelDossierNr", "CAT_catheter_episode")) %>% 
    mutate(MS_ICU_time_before_catheter = if_else(is.na(MS_ICU_time_before_catheter), 0, MS_ICU_time_before_catheter))
  
  # ----------------------------------------------------
  
  
  
  message("Calculating time spent in ICU during the time interval between 2 LMs...")
  # MS_ICU_time_before_LM: ICU during the time interval between 2 LMs" (has the patient been in ICU since last LM) 
  # ----------------------------------------------------
  PM_where_ICU_time_before_LM <- PM_where_ICU %>% 
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    # filter everything that is not between LM start and end; or does not intersect the interval LM_start_time - LM_end_time
    filter(periodeVan <= LM_end_time,
           periodeTot >= LM_start_time) %>% 
    # cut periodeVan and periodeTot so that we leave only the time in the interval LM_start_time - LM_end_time
    mutate(periodeVan = if_else(periodeVan < LM_start_time, LM_start_time, periodeVan),
           periodeTot = if_else(periodeTot > LM_end_time, LM_end_time, periodeTot),
           MS_ICU_time_before_LM = as.numeric(difftime(periodeTot, periodeVan, unit = "days"))) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MS_ICU_time_before_LM = sum(MS_ICU_time_before_LM)) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MS_ICU_time_before_LM)
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_ICU_time_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MS_ICU_time_before_LM = if_else(is.na(MS_ICU_time_before_LM), 0, MS_ICU_time_before_LM))
  
  rm(PM_where_ICU, PM_where_ICU_time_before_LM, PM_where_ICU_time_before_catheter)
  
  # ----------------------------------------------------
  
  
  
  message("Adding Operating Room presence information...")
  # MS_is_OR_unit: Is the patient now (at the exact second of the current LM) in OR?
  # ----------------------------------------------------
  PM_where_OR <- PM_whereDetail_tb %>%
    select(functioneelDossierNr, AC_AARDOMSCH, periodeVan, periodeTot, periodeNr) %>%
    arrange(functioneelDossierNr, periodeNr) %>%
    mutate(MS_is_OR_unit = ifelse(AC_AARDOMSCH == AARDOMSCH_OR, 1, 0)) %>%
    filter(MS_is_OR_unit == 1) %>%
    select(-c("periodeNr", "AC_AARDOMSCH")) %>%
    distinct()

  
  PM_where_OR_at_landmark <- PM_where_OR %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(LM_end_time >= periodeVan, LM_end_time <= periodeTot) %>% 
    select(-c("periodeVan", "periodeTot")) %>% 
    distinct()
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_OR_at_landmark, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    mutate(MS_is_OR_unit = if_else(is.na(MS_is_OR_unit), 0, MS_is_OR_unit))
  
  # ----------------------------------------------------
  
  
  message("Adding net OR presence at LM...")
  # MS_is_net_OR_unit: Is the patient now (at the exact second of the current LM) in net OR?
  # ----------------------------------------------------
  PM_where_net_OR <- PM_whereDetail_tb %>% 
    select(functioneelDossierNr, T_inZaal, T_uitZaal, netTimeInOR) %>%
    distinct(functioneelDossierNr, T_inZaal, T_uitZaal, netTimeInOR) %>%
    mutate(MS_is_net_OR_unit = ifelse(!is.na(netTimeInOR), 1, 0)) %>%
    filter(MS_is_net_OR_unit == 1) 
  
  
  PM_where_net_OR_at_landmark <- PM_where_net_OR %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(LM_end_time >= T_inZaal, LM_end_time <= T_uitZaal) %>% 
    select(-c("T_inZaal", "T_uitZaal", "netTimeInOR")) %>% 
    distinct()
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_net_OR_at_landmark, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    mutate(MS_is_net_OR_unit = if_else(is.na(MS_is_net_OR_unit), 0, MS_is_net_OR_unit))
  
  # ----------------------------------------------------
  
  
  message("Calculating time spent in OR before catheter placement...")
  # MS_OR_time_before_catheter: total time spent in OR between admission to catheter placement (per catheter episode)
  # OR: operating room; consists of procedure room, preparation room, real operating theatre where the surgery is done, post anesthesia care department and others
  # ----------------------------------------------------
  PM_where_OR_time_before_catheter <- PM_where_OR %>% 
    # check and combine continuous ICU time blocks
    arrange(functioneelDossierNr, periodeVan) %>%
    group_by(functioneelDossierNr) %>%
    mutate(or_group = group_overlap(periodeVan, periodeTot)) %>%
    ungroup() %>% 
    group_by(functioneelDossierNr, or_group) %>% 
    mutate(start_time_or = min_quiet(periodeVan),
           end_time_or = max_quiet(periodeTot)) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, start_time_or, end_time_or) %>%
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode), 
              by = "functioneelDossierNr", multiple = "all") %>%
    filter(start_time_or <= CAT_start_time_episode) %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_OR_time_before_catheter = if_else(end_time_or > CAT_start_time_episode, 
                                                difftime(CAT_start_time_episode, start_time_or, units = "days"), 
                                                difftime(end_time_or, start_time_or, units = "days")),
           MS_OR_time_before_catheter = as.numeric(MS_OR_time_before_catheter)) %>%
    subset(MS_OR_time_before_catheter > 0) %>%
    group_by(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_OR_time_before_catheter = sum(MS_OR_time_before_catheter)) %>%
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, MS_OR_time_before_catheter)
  # unit is days
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_OR_time_before_catheter, by = c("functioneelDossierNr", "CAT_catheter_episode")) %>% 
    mutate(MS_OR_time_before_catheter = if_else(is.na(MS_OR_time_before_catheter), 0, MS_OR_time_before_catheter))
  
  # ----------------------------------------------------
  
  
  message("Calculating net time spent in OR before catheter placement...")
  # MS_net_OR_time_before_catheter: total (net) time spent in OR between admission to catheter placement (per catheter episode)
  # net_OR: operating theatre where the exact surgery is done
  # ----------------------------------------------------
  PM_where_net_OR_time_before_catheter <- PM_where_net_OR %>% 
    # check and combine continuous ICU time blocks
    arrange(functioneelDossierNr, T_inZaal) %>%
    group_by(functioneelDossierNr) %>%
    mutate(or_group = group_overlap(T_inZaal, T_uitZaal)) %>%
    ungroup() %>% 
    group_by(functioneelDossierNr, or_group) %>% 
    mutate(start_time_or = min_quiet(T_inZaal),
           end_time_or = max_quiet(T_uitZaal)) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, start_time_or, end_time_or) %>%
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode), 
              by = "functioneelDossierNr", multiple = "all") %>%
    filter(start_time_or <= CAT_start_time_episode) %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_net_OR_time_before_catheter = if_else(end_time_or > CAT_start_time_episode, 
                                                    difftime(CAT_start_time_episode, start_time_or, units = "days"), 
                                                    difftime(end_time_or, start_time_or, units = "days")),
           MS_net_OR_time_before_catheter = as.numeric(MS_net_OR_time_before_catheter)) %>%
    subset(MS_net_OR_time_before_catheter > 0) %>%
    group_by(functioneelDossierNr, CAT_catheter_episode) %>%
    mutate(MS_net_OR_time_before_catheter = sum(MS_net_OR_time_before_catheter)) %>%
    ungroup() %>% 
    distinct(functioneelDossierNr, CAT_catheter_episode, MS_net_OR_time_before_catheter)
  # unit is days
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_net_OR_time_before_catheter, by = c("functioneelDossierNr", "CAT_catheter_episode")) %>% 
    mutate(MS_net_OR_time_before_catheter = if_else(is.na(MS_net_OR_time_before_catheter), 0, MS_net_OR_time_before_catheter))
  
  
  # ----------------------------------------------------

  
  message("Calculating time spent in OR during the time interval between 2 LMs...")
  # MS_OR_time_before_LM: OR during the time interval between 2 LMs" (has the patient been in OR since last LM) 
  # MS_is_24h_OR_unit: Is the patient in OR within the recent 24h (from last LM to the current LM)?
  # ----------------------------------------------------
  PM_where_OR_time_before_LM <- PM_where_OR %>% 
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    # filter everything that is not between LM start and end; or does not intersect the interval LM_start_time - LM_end_time
    filter(periodeVan <= LM_end_time,
           periodeTot >= LM_start_time) %>% 
    # cut periodeVan and periodeTot so that we leave only the time in the interval LM_start_time - LM_end_time
    mutate(periodeVan = if_else(periodeVan < LM_start_time, LM_start_time, periodeVan),
           periodeTot = if_else(periodeTot > LM_end_time, LM_end_time, periodeTot),
           MS_OR_time_before_LM = as.numeric(difftime(periodeTot, periodeVan, unit = "days"))) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MS_OR_time_before_LM = sum(MS_OR_time_before_LM),
           MS_is_24h_OR_unit = 1) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MS_OR_time_before_LM, MS_is_24h_OR_unit) 

  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_OR_time_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MS_OR_time_before_LM = if_else(is.na(MS_OR_time_before_LM), 0, MS_OR_time_before_LM),
           MS_is_24h_OR_unit = if_else(is.na(MS_is_24h_OR_unit), 0, MS_is_24h_OR_unit))
  
  rm(PM_where_OR_time_before_LM, PM_where_OR, 
     PM_where_net_OR_time_before_catheter, PM_where_OR_time_before_catheter)
  
  # ----------------------------------------------------
  
  
  message("Calculating net time spent in OR during the time interval between 2 LMs...")
  # MS_net_OR_time_before_LM: net OR during the time interval between 2 LMs" (has the patient been in OR since last LM) 
  # MS_is_24h_net_OR_unit: Is the patient in net OR within the recent 24h (from last LM to the current LM)?
  # ----------------------------------------------------
  PM_where_net_OR_time_before_LM <- PM_where_net_OR %>% 
    left_join(LM_data %>% 
                distinct(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    # filter everything that is not between LM start and end; or does not intersect the interval LM_start_time - LM_end_time
    filter(T_inZaal <= LM_end_time,
           T_uitZaal >= LM_start_time) %>% 
    # cut T_inZaal and T_uitZaal so that we leave only the time in the interval LM_start_time - LM_end_time
    mutate(T_inZaal = if_else(T_inZaal < LM_start_time, LM_start_time, T_inZaal),
           T_uitZaal = if_else(T_uitZaal > LM_end_time, LM_end_time, T_uitZaal),
           MS_net_OR_time_before_LM = as.numeric(difftime(T_uitZaal, T_inZaal, unit = "days"))) %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
    mutate(MS_net_OR_time_before_LM = sum(MS_net_OR_time_before_LM),
           MS_is_24h_net_OR_unit = 1) %>% 
    ungroup() %>%
    distinct(functioneelDossierNr, CAT_catheter_episode, LM, MS_net_OR_time_before_LM, MS_is_24h_net_OR_unit)
  
  
  LM_data <-  LM_data %>% 
    left_join(PM_where_net_OR_time_before_LM, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM")) %>% 
    mutate(MS_net_OR_time_before_LM = if_else(is.na(MS_net_OR_time_before_LM), 0, MS_net_OR_time_before_LM),
           MS_is_24h_net_OR_unit = if_else(is.na(MS_is_24h_net_OR_unit), 0, MS_is_24h_net_OR_unit))
  
  rm(PM_where_net_OR_time_before_LM)
  
  # ----------------------------------------------------
  
  
  
  # Extract the endoscopy information omschrijving 
  # MS_endoscopy: Has the patient undergone endoscopy since last LM (during the time interval between 2 LMs)
  message("Adding endoscopy information...")
  # ----------------------------------------------------

  PM_where_endoscopy <- PM_whereDetail_tb %>%
    select(functioneelDossierNr, omschrijving, periodeVan, periodeTot, periodeNr) %>%
    arrange(functioneelDossierNr, periodeNr) %>%
    mutate(MS_endoscopy = ifelse(omschrijving == omschrijving_endoscopy, 1, 0)) %>%
    filter(MS_endoscopy == 1) %>%
    select(-c("periodeNr", "omschrijving")) %>%
    distinct()
  
  
  PM_where_endoscopy <- PM_where_endoscopy %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(LM_end_time >= periodeVan, LM_start_time <= periodeVan) %>% 
    select(-c("periodeVan", "periodeTot")) %>% 
    distinct()
  
  
  LM_data <- LM_data %>%
    left_join(PM_where_endoscopy, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    mutate(MS_endoscopy = if_else(is.na(MS_endoscopy), 0, MS_endoscopy)) 
  
  rm(PM_where_endoscopy)

  
  # ----------------------------------------------------
  
  
  # Extract the IRCC information omschrijving 
  # MS_IRCC: Has the patient been in IRCC since last LM (during the time interval between 2 LMs)
  message("Adding IRCC information...")
  # ----------------------------------------------------
  
  PM_where_IRCC <- PM_whereDetail_tb %>%
    select(functioneelDossierNr, omschrijving, periodeVan, periodeTot, periodeNr) %>%
    arrange(functioneelDossierNr, periodeNr) %>%
    mutate(MS_IRCC = ifelse(omschrijving == omschrijving_IRCC, 1, 0)) %>%
    filter(MS_IRCC == 1) %>%
    select(-c("periodeNr", "omschrijving")) %>%
    distinct()
  
  
  PM_where_IRCC <- PM_where_IRCC %>% 
    left_join(LM_data %>% 
                select(functioneelDossierNr, CAT_catheter_episode, LM, LM_start_time, LM_end_time), 
              by = "functioneelDossierNr", multiple = "all") %>% 
    filter(LM_end_time >= periodeVan, LM_start_time <= periodeVan) %>% 
    select(-c("periodeVan", "periodeTot")) %>% 
    distinct()
  
  
  LM_data <- LM_data %>%
    left_join(PM_where_IRCC, by = c("functioneelDossierNr", "CAT_catheter_episode", "LM", "LM_start_time", "LM_end_time")) %>% 
    mutate(MS_IRCC = if_else(is.na(MS_IRCC), 0, MS_IRCC))
  
  rm(PM_where_IRCC)

  # ----------------------------------------------------
  
  
  message(sprintf("Patient medical specialty and whereabouts features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(LM_data)
}

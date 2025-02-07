#' Calculates BSIs of different type: CLABSI, secundary, POA... 
#'
#' @param con connection to the database created with DBI::dbConnect
#' @param admission_ids vector of admission IDs for which to do the calculation; if NULL all data in the database are used
#' 
#' @return dataframe in the same format as the table received from UZL calculation 
#' 

calculate_BSI <- function(con, samples_blood, samples_exclude, table_germs, 
                          admission_ids = NULL){
  
  # CLABSI is considered any laboratory-confirmed bloodstream infection for a patient with central line or 
  # within 48 hours after the central line removal
  # that is not detected in the first 48 hours from admission
  # (POA = present on admission), that is not a secondary infection (the same germ detected in another sample)
  # and is not a skin contamination (based on a list of skin contaminants).
  # The CLABSI definition is based on the Sciensano definition published in 2019. 
  # The symptoms criteria are though not checked, neither for adults (e.g.: fever, hypertension) nor for neonates (e.g.: fever, leukocytopenia).
  # It is considered that if cultures have been ordered that was on the base on these symptoms.
  
  # The planned collection time is used in the calculation (date_foreseen) 
  # because this is the closest proxy to the date of collection of specimen.
  # The exact collection date is not available. Collection time is the best choice because 
  # a germ was present in a blood sample at collection time, even if it is 
  # confirmed by the lab at a later date (validation time)
  
  start_time <- Sys.time()
  
  # get data
  # --------
  
  message("Reading Microbiology cultures from the database...")
  
  if (is.null(admission_ids)){
    query <- "SELECT functioneelDossierNr, tijdstip, staalsoort, extensie, resultaat FROM lwsPositieveKweken"
    rs <- DBI::dbSendQuery(con, query) 
    lab_MB <- DBI::dbFetch(rs) %>% as_tibble()
    dbClearResult(rs)
  } else {
    query <- "SELECT functioneelDossierNr, tijdstip, staalsoort,  extensie, resultaat FROM lwsPositieveKweken 
              WHERE 1 = 1"
    lab_MB <- get_data(con, query, admission_ids)
  }
  
  message("Reading patient admission data from the database...")
  
  if (is.null(admission_ids)){
    query <- "SELECT functioneelDossierNr, HospitalStartDate, HospitalEndDate, ageInWeeks 
              FROM PMbaseline_tb"
    rs <- DBI::dbSendQuery(con, query) 
    admission_data <- DBI::dbFetch(rs) %>% as_tibble()
    dbClearResult(rs)
  } else {
    query <- "SELECT functioneelDossierNr, HospitalStartDate, HospitalEndDate, ageInWeeks
              FROM PMbaseline_tb WHERE 1 = 1"
    admission_data <- get_data(con, query, admission_ids)
  }
  
  message("Reading lab data from the database (for neutropenia)...")
  
  if (is.null(admission_ids)){
    query <- sprintf("SELECT *
                     FROM  lwsData WHERE test IN ('%s')", 
                     paste(neutropenia_lab_tests, collapse = "', '"))
    rs <- DBI::dbSendQuery(con, query) 
    lab_data <- DBI::dbFetch(rs) %>% as_tibble()
    dbClearResult(rs)
  } else {
    query <- sprintf("SELECT *
                     FROM  lwsData WHERE test IN ('%s')", 
                     paste(neutropenia_lab_tests, collapse = "', '"))
    lab_data <- get_data(con, query, admission_ids)
  }
  
  # calculate BSI
  # -------------
  
  message("Calculating BSI ...")
  
  # rename columns to friendly names
  lab_MB <- lab_MB %>% 
    rename(date_foreseen = tijdstip,
           sample_type = staalsoort,
           germ = resultaat) %>% 
    select(functioneelDossierNr, date_foreseen, sample_type, extensie, germ)
  
  # fix some germ names 
  lab_MB <- lab_MB %>% 
    mutate(germ = if_else(germ == "Candida guilliermondii", "Candida guillermondii", germ))
  
  # identify bloodstream infections:
  # 1. It is a blood sample (not urine, wound, etc...) - samples_blood
  # 2. It is a germ in the list of valid germs from Veerle (table_germs) 
  # Otherwise germ includes values like: overbodig, resultaat gewist wegens foutieve patiÃ«ntidentificatie, etc...
  # Invalid germs (like: overbodig, ...) are also excluded for all samples (urine, wound, etc...)
  # These will be used for secondary BSI calculation.
  lab_MB <- lab_MB %>%
    filter(germ %in% table_germs$naam) %>% 
    mutate(is_BSI = str_trim(sample_type) %in% samples_blood)
  
  # some germs, like Enterobacter cloacae and Enterobacter cloacae complex are considered the same germ
  lab_MB <- lab_MB %>%
    mutate(germ_original = germ,
           germ_not_complex = str_replace(germ, 
                                          "-abcessus complex| complex| ss. xylosoxidan|/famata| var belfanti| var. grubii| var. neoformans|/gattii| O:157| var. Copenhagen| o3| o9", 
                                          "")) %>% 
    group_by(functioneelDossierNr, germ_not_complex) %>% 
    mutate(has_complex = any(germ != germ_not_complex),
           has_no_complex = any(germ == germ_not_complex)) %>% 
    ungroup() %>% 
    mutate(germ = if_else(has_complex & has_no_complex & germ_not_complex != germ, germ_not_complex, germ)) %>% 
    select(-c(germ_not_complex, has_complex))
  
  # POA
  # ---
  
  message("Calculating POA ...")
  
  lab_MB <- lab_MB %>% 
    left_join(admission_data, by = "functioneelDossierNr") %>% 
    # keep only samples after the admission date (over all years there are around 2.5% BSIs before admission)
    filter(date_foreseen >= HospitalStartDate,
           date_foreseen <= HospitalEndDate) %>% 
    mutate(hours_since_admission = as.numeric(difftime(date_foreseen, HospitalStartDate, units = "hours")),
           is_POA = is_BSI & hours_since_admission <= 48) %>% 
    select(-c(HospitalStartDate, HospitalEndDate, hours_since_admission))
  
  # secondary BSI
  # -------------
  
  message("Calculating secondary BSI ...")
  
  # keep BSI separated from non-BSI
  BSI_samples <- lab_MB %>% 
    select(functioneelDossierNr, date_foreseen, sample_type, germ, is_BSI) %>% 
    distinct() %>% 
    filter(is_BSI)
  
  # keep catheter wound location list (this list has been created manually by Elena and Veerle)
  catheter_wound_extension <- table_extensie_catheter %>% 
    filter(is_catheter_wound == "yes") %>% 
    pull(extensie)
  
  non_BSI_samples <- lab_MB %>% 
    select(functioneelDossierNr, date_foreseen, sample_type, extensie, germ, is_BSI) %>% 
    distinct() %>% 
    filter(!is_BSI,
           !sample_type %in% samples_exclude, # filter the catheter samples
           !(str_detect(tolower(extensie), extensie_exclude_moedermelk)), # exclude moedermelk (screening sample)
           !(extensie %in% catheter_wound_extension)) %>% # filter catheter extensions
    rename(date_foreseen_other = date_foreseen,
           sample_type_other = sample_type,
           germ_other = germ) %>% 
    select(-c(is_BSI, extensie))
  
  # merge BSI with non-BSI and filter only BSIs that have another sample for the same germ in +/- x days
  secondary_BSIs <- BSI_samples %>% 
    left_join(non_BSI_samples, by = "functioneelDossierNr", multiple = "all") %>% 
    filter(date_foreseen_other <= date_foreseen + days(time_window_secondary[1]), # keep germs in +/- x days window
           date_foreseen_other >= date_foreseen - days(time_window_secondary[2]),
           germ == germ_other) %>%  # only the same germ
    mutate(is_SECONDARY = TRUE) %>% 
    select(-c(is_BSI, date_foreseen_other, sample_type_other, germ_other)) %>% 
    distinct()
  
  # merge back the secondary BSIs
  lab_MB <- lab_MB %>% 
    left_join(secondary_BSIs,by = c("functioneelDossierNr", "date_foreseen", "sample_type", "germ")) %>% 
    mutate(is_SECONDARY = if_else(is.na(is_SECONDARY), FALSE, is_SECONDARY))
  
  # contaminant
  # -----------
  
  message("Calculating contaminant BSI ...")
  
  # the list of contaminants provided by Veerle
  list_germs_contaminants <- table_germs %>% 
    filter(`Huidcontaminant?` == "x") %>% 
    pull(naam)
  
  # mark contaminant germs
  lab_MB <- lab_MB %>% 
    mutate(is_germ_contaminant = is_BSI & germ %in% list_germs_contaminants)
  
  # keep contaminant samples apart
  contaminant_samples <- lab_MB %>% 
    filter(is_germ_contaminant, is_BSI) %>% 
    select(functioneelDossierNr, date_foreseen, germ, extensie) %>% 
    distinct()
  
  # filter same date and same extension
  # if blood through blue lumen and blood through red lumen is drawn at the same time
  # it will be considered as 2 samples in +/- 48h
  
  # is there another sample with the same contaminant germ in window +/- 2 days
  contaminant_samples <- contaminant_samples %>% 
    left_join(contaminant_samples %>% 
                rename(date_foreseen_other = date_foreseen,
                       extensie_other = extensie), 
              by = c("functioneelDossierNr", "germ"), multiple = "all") %>% 
    filter(date_foreseen_other <= date_foreseen + days(2), # keep germs in +/- 2d window
           date_foreseen_other >= date_foreseen - days(2),
           !(date_foreseen_other == date_foreseen & extensie == extensie_other)) %>% 
    mutate(is_contaminant_germ_twice = TRUE) %>% 
    select(-c(date_foreseen_other, extensie_other,extensie)) %>% 
    distinct()
  
  lab_MB <- lab_MB %>% 
    left_join(contaminant_samples, by = c("functioneelDossierNr", "date_foreseen", "germ")) %>% 
    mutate(is_CONTAMINANT = case_when(is_BSI & is_germ_contaminant & is_contaminant_germ_twice ~ FALSE, # we have 2 samples (= not contaminant)
                                      is_BSI & is_germ_contaminant & is.na(is_contaminant_germ_twice) ~ TRUE, # we don't have 2 samples' (= contaminant)
                                      TRUE ~ FALSE))
  
  # overwrite for NEONATES (only one sample with coag-neg staph is enough --> will be CLABSI not CONTAMINANT)
  lab_MB <- lab_MB %>% 
    mutate(is_neonate = !is.na(ageInWeeks) & ageInWeeks <= 4) %>% 
    mutate(is_CONTAMINANT = if_else(is_neonate & germ %in% coagulase_negatieve_stafylokok, FALSE, is_CONTAMINANT))
  
  # MBI (mucosal barrier injury)
  # ----------------------------
  
  message("Calculating MBI BSI ...")
  
  # mark MBI germs
  # map to SNOMED name
  list_germs_MBI_SNOMED <- unique(table_MBI_germs %>% pull(`SNOMED Preferred Term`))
  # or map to NHSN name
  list_germs_MBI_NHSN <- unique(table_MBI_germs %>% pull(`NHSN Display Name`))
  # or map to name mapping (manually done by Elena)
  list_germs_MBI_mapped <- table_MBI_germ_mapping %>% filter(!is.na(`UZL name`)) %>% 
    pull(`UZL name`) %>% unique()
  list_germs_MBI <- unique(c(list_germs_MBI_SNOMED, list_germs_MBI_NHSN, list_germs_MBI_mapped))
  
  # mark MBI germs
  lab_MB <- lab_MB %>% 
    mutate(is_germ_MBI_1_sample = is_BSI & germ %in% list_germs_MBI,
           is_germ_other = is_BSI & !germ %in% list_germs_MBI) %>% 
    group_by(functioneelDossierNr, date_foreseen, sample_type) %>% 
    mutate(is_germ_other = any(is_germ_other)) %>% 
    ungroup() %>% 
    mutate(is_germ_MBI_1_sample = is_germ_MBI_1_sample & 
             !is_germ_other)
  
  # mark MBI Streptococcus viridans (2 samples needed and no other MO) 
  lab_MB <- lab_MB %>% 
    mutate(is_germ_MBI_S_viridans_2_samples = is_BSI & germ == "Streptococcus viridans",
           is_germ_other_S_viridans = is_BSI & germ != "Streptococcus viridans") %>% 
    group_by(functioneelDossierNr, date_foreseen, germ) %>% 
    mutate(n_samples = n()) %>% 
    ungroup() %>% 
    group_by(functioneelDossierNr, date_foreseen, sample_type) %>% 
    mutate(is_germ_other_S_viridans = any(is_germ_other_S_viridans)) %>% 
    ungroup() %>% 
    mutate(is_germ_MBI_S_viridans_2_samples = is_germ_MBI_S_viridans_2_samples & 
             n_samples > 1 & 
             !is_germ_other_S_viridans)
  
  # is germ condition met for MBI?
  lab_MB <- lab_MB %>% 
    mutate(is_germ_MBI = is_germ_MBI_S_viridans_2_samples | is_germ_MBI_1_sample)
  
  # mark neutropenia (2 days with neutrophils or WBC <500/mm3 during the timewindow +/- 3 day from sample)
  
  # transform lab data
  # check units
  if (nrow(lab_data %>% filter(eenheid != "10**9/L")) > 0)
    message("There are other eenheid in lab data than 10**9/L. Check the logic of the function!")
  
  # clean and make lab results numeric
  lab_data <- lab_data %>% 
    mutate(resultaat = str_replace(resultaat, "^(<|>|<=|>=) *", "")) %>% 
    mutate(resultaat = as.numeric(resultaat)) %>% 
    mutate(validatietijdstip = as.POSIXct(validatietijdstip, tz = "UTC")) 
  
  # tranform in quantity / mm3
  # x * 10**9/L = x * 10**9/ 10**6 mm3 = x * 10**3 / mm3
  lab_data <- lab_data %>% 
    mutate(resultaat = resultaat * 1000) 
  
  # mark low WBC for neutropenia
  lab_data <- lab_data %>% 
    mutate(is_WBC_low = resultaat < 500) %>% 
    filter(is_WBC_low) %>% 
    select(functioneelDossierNr, validatietijdstip, is_WBC_low)
  
  # calculate +/- 3 days (on DAY level) for checking neutropenia
  lab_MB <- lab_MB %>% 
    mutate(window_MBI_start = as.Date(date_foreseen) - days(3),
           window_MBI_start = as.POSIXct(paste0(window_MBI_start, " 00:00:00"), format = "%Y-%m-%d %H:%M:%S"),
           window_MBI_end = as.Date(date_foreseen) + days(3),
           window_MBI_end = as.POSIXct(paste0(window_MBI_end, " 23:59:59"), format = "%Y-%m-%d %H:%M:%S"))
  
  neutropenia_samples <- lab_MB %>% 
    left_join(lab_data, by = "functioneelDossierNr", multiple = "all") %>% 
    filter(validatietijdstip >= window_MBI_start,
           validatietijdstip <= window_MBI_end) %>% 
    filter(is_germ_MBI) %>% 
    mutate(date_neutropenia = as.Date(validatietijdstip)) %>% 
    select(functioneelDossierNr, date_foreseen, sample_type, extensie, germ, is_germ_MBI, window_MBI_start,
           window_MBI_end, date_neutropenia) %>% 
    distinct() %>% 
    group_by(functioneelDossierNr, date_foreseen, sample_type, extensie, germ) %>% 
    mutate(n_neurtropenia = n()) %>% 
    ungroup() %>% 
    filter(n_neurtropenia > 1) %>% 
    mutate(is_neutropenia_in_window = TRUE) %>% 
    select(functioneelDossierNr, date_foreseen, sample_type, extensie, germ, is_neutropenia_in_window) %>% 
    distinct() 
  
  lab_MB <- lab_MB %>% 
    left_join(neutropenia_samples, by = c("functioneelDossierNr", "date_foreseen", "sample_type", "extensie",
                                          "germ")) %>% 
    mutate(is_MBI = is_germ_MBI & is_neutropenia_in_window)
  
  # CLABSI and final events
  # -----------------------
  
  message("Calculating CLABSI and final event type ...")
  
  # calculate event type and rename columns (same as in hospital extraction)
  event_data <- lab_MB %>% 
    filter(is_BSI) %>% 
    distinct() %>% 
    mutate(clabsiType = case_when(is_POA ~ 5,
                                  is_SECONDARY ~ 3,
                                  is_MBI ~ 6,
                                  is_CONTAMINANT ~ 4,
                                  TRUE ~ 1)) %>% 
    rename(datumAfname = date_foreseen,
           bacterie = germ) %>% 
    select(functioneelDossierNr, bacterie, clabsiType, datumAfname, germ_original) %>% 
    distinct()
  
  # calculate BSI episodes (as per Sciensano)
  # Alle gevallen van BSI die niet meer dan 14 dagen van elkaar gescheiden zijn, 
  # worden beschouwd als behorend tot dezelfde episode als ze door hetzelfde MO 
  # worden veroorzaakt
  # BUT not for contaminant episodes - these do not form episodes as per meeting with Veerle 15/06/2022
  # contamination with skin flora happens by accident and is left in isolation
  event_data_BSI <- event_data %>% 
    filter(clabsiType != 4) %>% 
    mutate(start_BSI_episode = datumAfname,
           end_BSI_epsiode = datumAfname + days(14)) %>% 
    arrange(functioneelDossierNr, bacterie, start_BSI_episode) %>% 
    # create BSI episodes from overlapping intervals
    group_by(functioneelDossierNr, bacterie) %>% 
    arrange(start_BSI_episode) %>% 
    mutate(BSI_episode = group_overlap(start_BSI_episode, end_BSI_epsiode)) %>% 
    ungroup() %>% 
    # make episode type the first BSI type and keep only this 
    group_by(functioneelDossierNr, bacterie, BSI_episode) %>% 
    arrange(start_BSI_episode) %>% 
    mutate(clabsiType = first(clabsiType),
           start_BSI_episode = first(start_BSI_episode),
           end_BSI_epsiode = last(end_BSI_epsiode)) %>% 
    slice(1) %>%
    ungroup()
  
  # for contaminant, create dummy episodes of 0 seconds (start = end)
  event_data_CONTAMINANT <- event_data %>% 
    filter(clabsiType == 4) %>% 
    mutate(start_BSI_episode = datumAfname,
           end_BSI_epsiode = datumAfname) %>% 
    group_by(functioneelDossierNr, bacterie) %>% 
    arrange(start_BSI_episode) %>% 
    mutate(BSI_episode = row_number()) %>% 
    ungroup()
  
  # merge back the BSI's with the CONATMINANT
  event_data <- event_data_BSI %>% 
    rbind(event_data_CONTAMINANT) %>% 
    arrange(functioneelDossierNr, datumAfname)
  
  # finally, for poly-bacterial infections, the grey zone is between CLASBI and SECONDARY
  
  # Een poly-microbiële BSI is een BSI waarvoor op dezelfde dag verschillende MO 
  # (in één of meerdere bloedstalen) zijn aangetroffen
  
  event_data <- event_data %>% 
    mutate(calendar_day = as.Date(datumAfname)) %>% 
    group_by(functioneelDossierNr, calendar_day) %>% 
    mutate(any_SECONDARY = any(clabsiType == 3),
           any_CLABSI = any(clabsiType == 1)) %>% 
    ungroup() %>% 
    mutate(is_poly_CLABSI_SEC = any_SECONDARY & any_CLABSI) %>% 
    select(-c(any_SECONDARY, any_CLABSI, calendar_day))
  
  # make poly-bacterial secondary
  if (is_poly_CLABSI){
    
    event_data <- event_data %>% 
      mutate(clabsiType = if_else(is_poly_CLABSI_SEC, 3, clabsiType))
  }
  
  # replace the original germ name (without termination eg: complex, removed)
  event_data <- event_data %>% 
    mutate(bacterie = germ_original) %>% 
    select(-germ_original)
  
  message(sprintf("BSIs calculated in in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(event_data)
}
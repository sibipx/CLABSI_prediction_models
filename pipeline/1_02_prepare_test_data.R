
# libraries and functions
# -----------------------
library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)

# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# load documentation
# ------------------

docu_path <- "docu/docu_df.xlsx"
docu_df <- docu_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = docu_path) %>% 
  `[[`("Landmark dataframe")

# configuration (train/test specific)
# -----------------------------------

# features not usable in playground years
columns_to_exclude_extra <- c("LAB_troponine_I_last")
columns_to_exclude <- unique(c(columns_to_exclude, columns_to_exclude_extra))

# prepare training data
# ---------------------

# connect to DB
con <- db_connect("R/db_config.R")

# get admission IDs
katheterPatients_tb <- DBI::dbReadTable(conn = con, "katheterPatients_tb") %>% as_tibble()
admission_ids <- katheterPatients_tb %>% 
  filter(admissionYear >= 2018) %>% 
  distinct(functioneelDossierNr) %>% 
  pull(functioneelDossierNr)


# init data
data <- init_data_catheters(con, admission_ids)

# add outcome
data <- add_features_outcome(data, con)

# add features
data <- add_features_catheters(data, con)
data <- add_features_medical_specialty(data, con)
data <- add_features_medication(data, con, dict_ATC, 
                                ATC_L02_to_include = ATC_L02_to_include,
                                ATC_codes_to_include = ATC_codes_to_include)
data <- add_features_admission(data, con)
data <- add_features_comorbidities(data, con)
data <- add_features_blood_transfer(data, con)
data <- add_features_microbiology(data, con)
data <- add_features_generic(data, con)
data <- add_features_historic_comorbidities(data, con)

# add lab features (large table) - do garbage collection first
gc()
data <- add_features_lab(data, con)

# CARE features
data <- add_features_care_vital_signs(data, con)
data <- add_features_care_neuro(data, con)
data <- add_features_care_safety(data, con)
data <- add_features_care_isolation(data, con)
data <- add_features_care_excretion(data, con)
data <- add_features_care_symptoms(data, con)
data <- add_features_care_physical_params(data, con)
data <- add_features_care_ICU(data, con)
data <- add_features_care_wound(data, con)
data <- add_features_care_dialysis(data, con)

# perform some basic checks
# -------------------------

# check if documentation is up to date :-)
check_columns(data, docu_df)

# there should be no duplicated LMs (every LM should appear only once in the df)
(check_duplicates <- data %>% 
    select(functioneelDossierNr, CAT_catheter_episode, CAT_start_time_episode, CAT_end_time_episode, LM) %>% 
    duplicated() %>% 
    sum())

if(check_duplicates > 0) {
  message(sprintf("There are %s duplicates", check_duplicates))
}

# summary missing values
map(data, ~sum(is.na(.))) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>% 
  mutate(number_present = nrow(data) - number_missing,
         percentage_missing = number_missing/dim(data)[[1]]) %>% 
  arrange(desc(number_missing)) %>% 
  filter(number_missing > 0) %>% 
  head(20)

# explore sparse columns
map(data %>% select(colnames(data)[colnames(data) %in% docu_df$Column[docu_df$type == "binary"]]), 
    ~sum(., na.rm = TRUE)) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_positive") %>% 
  mutate(percentage_positive = number_positive/dim(data)[[1]]) %>% 
  arrange(desc(number_positive)) %>% 
  tail(20)

# write data to the DB
# --------------------

write_to_DB(con, "LM_data_18_20", data, docu_df)

# read data & check if identical
# data_DB <- read_from_DB(con, "LM_data_18_20", docu_df) %>% as_tibble()
# 
# identical(data %>% 
#             mutate_if(is.integer, as.double) %>% 
#             arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
#             sapply(unname),
#           data_DB %>% 
#             mutate_if(is.integer, as.double) %>% 
#             arrange(functioneelDossierNr, CAT_catheter_episode, LM) %>% 
#             sapply(unname))


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

# clean training data
# ---------------------

# connect to DB
con <- db_connect("R/db_config.R")

data_test <- read_from_DB(con, "LM_data_18_20", docu_df) %>% as_tibble()

data_test_clean <- clean_dynamic_data_train(data_test)

save(data_test_clean, 
     file = paste0(data_path_test_clean, "data_test_clean"))

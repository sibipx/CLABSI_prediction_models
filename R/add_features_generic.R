#' Adds generic features related to patient (month, day, patient/nurse ratio ...) to the landmark dataframe 
#'
#' @param LM_data landmark data (already initialized with the init_data_catheters function)
#' @param con connection to the database created with DBI::dbConnect
#' 
#' @return LM_data - landmark dataframe with additional features
#' 
#' to be changed later

add_features_generic <- function(LM_data, con){
  
  start_time <- Sys.time()
  
  # delete the features if they exist (to avoid adding the features twice with .x, .y prefix)
  col_names <- colnames(LM_data)
  col_names <- col_names[grep("^GEN_.*", col_names)]
  if (length(col_names) > 0){
    LM_data <- LM_data %>% 
      select(-all_of(col_names))
  }
  
  
  message("Adding generic features...")
  
  
  # keep only admission ids that are in LM_data (faster)
  admission_ids <- unique(LM_data$functioneelDossierNr)
  
  
  # month of the year as proxy for periods with student nurses/doctors
  # day & season are also extracted, in case needed
  
  # days
  LM_data <- LM_data %>%
    mutate(GEN_LM_day_num = wday(LM_end_time, week_start = 1)) %>%
    mutate(GEN_LM_day_categ = case_when(GEN_LM_day_num == 1 ~ "Monday",
                                       GEN_LM_day_num == 2 ~ "Tuesday",
                                       GEN_LM_day_num == 3 ~ "Wednesday",
                                       GEN_LM_day_num == 4 ~ "Thursday",
                                       GEN_LM_day_num == 5 ~ "Friday",
                                       GEN_LM_day_num == 6 ~ "Saturday",
                                       GEN_LM_day_num == 7 ~ "Sunday",
                                       TRUE ~ NA_character_)) %>% 
    mutate(GEN_LM_day_sin = sin(GEN_LM_day_num * (2*pi/7)),
           GEN_LM_day_cos = cos(GEN_LM_day_num * (2*pi/7)))
  
  
  # months
  LM_data <- LM_data %>%
    mutate(GEN_LM_month_num = month(LM_end_time)) %>%
    mutate(GEN_LM_month_categ = case_when(GEN_LM_month_num == 1 ~ "January",
                                         GEN_LM_month_num == 2 ~ "February",
                                         GEN_LM_month_num == 3 ~ "March",
                                         GEN_LM_month_num == 4 ~ "April",
                                         GEN_LM_month_num == 5 ~ "May",
                                         GEN_LM_month_num == 6 ~ "June",
                                         GEN_LM_month_num == 7 ~ "July",
                                         GEN_LM_month_num == 8 ~ "August",
                                         GEN_LM_month_num == 9 ~ "September",
                                         GEN_LM_month_num == 10 ~ "October",
                                         GEN_LM_month_num == 11 ~ "November",
                                         GEN_LM_month_num == 12 ~ "December",
                                         TRUE ~ NA_character_)) %>% 
    mutate(GEN_LM_month_sin = sin(GEN_LM_month_num * (2*pi/12)),
           GEN_LM_month_cos = cos(GEN_LM_month_num * (2*pi/12)))
  
  
  # seasons
  # Meteorological Seasons
  # spring: March 1 to May 31
  # summer: June 1 to August 31
  # autumn: September 1 to November 30
  # winter: December 1 to February 28 (February 29 in a leap year)
  
  LM_data <- LM_data %>%
    mutate(GEN_LM_season_num = quarter(LM_end_time, fiscal_start = 3)) %>%
    mutate(GEN_LM_season_categ = case_when(GEN_LM_season_num == 1 ~ "Spring",
                                          GEN_LM_season_num == 2 ~ "Summer",
                                          GEN_LM_season_num == 3 ~ "Autumn",
                                          GEN_LM_season_num == 4 ~ "Winter",
                                          TRUE ~ NA_character_)) %>% 
    mutate(GEN_LM_season_sin = sin(GEN_LM_season_num * (2*pi/4)),
           GEN_LM_season_cos = cos(GEN_LM_season_num * (2*pi/4)))

  
  # for the sin / cos features, fix the 24 in the documentation too
  
  message(sprintf("Generic features added in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  
  return(LM_data)
}

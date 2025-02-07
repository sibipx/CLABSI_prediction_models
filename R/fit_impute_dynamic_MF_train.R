

fit_impute_dynamic_MF_train <- function(data, cont_fun = mean, cols_not_missing,
                                        verbose = TRUE){
  
  start_time <- Sys.time()
  
  # mean/mode + LOCF imputation
  # ---------------------------
  
  # data_miss <- map(data, ~sum(is.na(.))) %>% 
  #   as_tibble() %>% 
  #   pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>% 
  #   mutate(number_present = nrow(data) - number_missing,
  #          percentage_missing = number_missing/dim(data)[[1]]) %>% 
  #   arrange(desc(number_missing)) %>% 
  #   filter(number_missing > 0)
  
  is_binary_col <- sapply(data, function(x) all(unique(x[!is.na(x)]) %in% c(0, 1)))
  binary_cols <- names(is_binary_col)[is_binary_col]
  
  is_factor_col <- sapply(data, function(x) is.factor(x) | is.character(x))
  categorical_cols <- names(is_factor_col)[is_factor_col]
  
  continuous_cols <- colnames(data)[!colnames(data) %in% c(binary_cols, categorical_cols)]
  
  # exclude the columns for which we don't allow missingness
  binary_cols <- binary_cols[!binary_cols %in% cols_not_missing] 
  categorical_cols <- categorical_cols[!categorical_cols %in% cols_not_missing] 
  continuous_cols <- continuous_cols[!continuous_cols %in% cols_not_missing] 
  
  cols_impute <- c(categorical_cols, binary_cols, continuous_cols)
  
  # make integer column double
  data <- data %>% mutate_if(is.integer, as.double)
  
  # initialize table with mean (median) / mode
  table_cols_impute <- tibble(col_name = c(categorical_cols, binary_cols, continuous_cols)) %>% 
    mutate(mode_binary = NA_integer_,
           mode_categ = NA_character_,
           mean_cont = NA_real_)
  
  data_base <- data %>% filter(LM == 0)
  
  # mode categorical 
  for (col in categorical_cols) {
    
    col_values <- data_base[,col][[1]]
    col_values <- col_values[!is.na(col_values)]
    
    mode_col <- unique(col_values)[col_values %>%
                                     match(unique(col_values)) %>% 
                                     tabulate() %>%
                                     which.max()]
    
    # keep mode value for test 
    table_cols_impute <- table_cols_impute %>% 
      mutate(mode_categ = if_else(col_name == col, as.character(mode_col), mode_categ))
    
  }
  
  # mode binary
  for (col in binary_cols) {
    
    col_values <- data_base[,col][[1]]
    col_values <- col_values[!is.na(col_values)]
    
    mode_col <- unique(col_values)[col_values %>%
                                     match(unique(col_values)) %>% 
                                     tabulate() %>%
                                     which.max()]
    
    # keep mode value for test 
    table_cols_impute <- table_cols_impute %>% 
      mutate(mode_binary = if_else(col_name == col, as.integer(mode_col), mode_binary))
  }
  
  # mean (median) continuous
  for (col in continuous_cols) {
    
    col_values <- data_base[,col][[1]]
    col_values <- col_values[!is.na(col_values)]
    
    mean_col <- as.double(cont_fun(col_values))
    
    # keep mean / median value for test 
    table_cols_impute <- table_cols_impute %>% 
      mutate(mean_cont = if_else(col_name == col, mean_col, mean_cont))
  }
  
  # impute data at baseline with mean/mode
  data_LOCF <- data
  
  for (col in table_cols_impute$col_name){
    
    mode_binary_value <- table_cols_impute[table_cols_impute$col_name == col,"mode_binary"][[1]]
    mode_categ_value <- table_cols_impute[table_cols_impute$col_name == col,"mode_categ"][[1]]
    mean_value <- table_cols_impute[table_cols_impute$col_name == col,"mean_cont"][[1]]
    if (!is.na(mode_binary_value)) {
      value <- mode_binary_value 
    } else if (!is.na(mode_categ_value)){
      value <- mode_categ_value 
      if (is.integer(data[,col, drop = TRUE])) value <- as.integer(value)
    } else if (!is.na(mean_value)){
      value <- mean_value 
    } else {
      value <- NA
    }
    
    # initialize LOCF imputed data for x_init
    data_LOCF[is.na(data_LOCF[,col]) & data_LOCF$LM == 0, col, drop = TRUE] <- value

  }
  
  # do LOCF imputation as x_init for missForest
  data_LOCF <- data_LOCF %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    arrange(LM) %>% 
    fill(all_of(cols_impute), .direction = "down") %>% 
    ungroup() %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode, LM)
  
  if (is.na(data_LOCF) %>% rowSums() %>% sum() > 0) stop("There are missing values after LOCF imputation. Something went wrong.")
  
  # missForest
  # ----------
  
  # make binary columns factor
  is_binary_col <- sapply(data, function(x) all(unique(x[!is.na(x)]) %in% c(0, 1)))
  binary_vars <- names(is_binary_col)[is_binary_col]
  
  data <- data %>% 
    mutate_at(all_of(binary_vars), as.factor)
  data_LOCF <- data_LOCF %>% 
    mutate_at(all_of(binary_vars), as.factor)
  
  # make character columns factor
  is_char_col <- sapply(data, function(x) is.character(x))
  char_cols <- names(is_char_col)[is_char_col]
  char_cols <- char_cols[!char_cols %in% c("type")] # leave type (outcome) as is
  
  data <- data %>% 
    mutate_at(all_of(char_cols), as.factor)
  data_LOCF <- data_LOCF %>% 
    mutate_at(all_of(char_cols), as.factor)
  
  predictor_matrix <- missForestPredict::create_predictor_matrix(data)
  predictor_matrix[cols_not_missing,] <- 0
  # make sure outcome is not used
  predictor_matrix[c("eventtime", "type", "functioneelDossierNr"), ] <- 0
  predictor_matrix[,c("eventtime", "type", "functioneelDossierNr")] <- 0
  # do not impute LAB_is_neutropenia and do not use in imputation
  predictor_matrix["LAB_is_neutropenia", ] <- 0
  predictor_matrix[,"LAB_is_neutropenia"] <- 0
  
  imp_obj <- missForestPredict::missForest(data, verbose = verbose,
                                           initialization = "custom",
                                           return_integer_as_integer = FALSE,
                                           save_models = TRUE,
                                           predictor_matrix = predictor_matrix,
                                           proportion_usable_cases = c(0.97, 0.03),
                                           x_init = data_LOCF,
                                           respect.unordered.factors = "order",
                                           max.depth = 10)
  
  # make binary variables double
  data <- imp_obj$ximp
  data <- data %>% 
    mutate_at(all_of(binary_vars), function(x) as.numeric(as.character(x)))
  
  # re-calculate neutropenia based on the imputed variable 
  # (last value of WBC count or WBC Neutrophils)
  # following the same log as in calculate_BSI.R
  data <- data %>% 
    mutate(LAB_is_neutropenia = as.numeric(LAB_WBC_count_last * 1000 < 500 |
                                             LAB_WBC_Neutrophils_last * 1000 < 500)) 
  
  # calculate BMI after imputation
  data <- data %>% 
    mutate(CARE_PHY_BMI = CARE_PHY_weight_mean / CARE_PHY_length_mean^2) %>% 
    select(-c(CARE_PHY_length_mean, CARE_PHY_weight_mean))
  
  if (is.na(data) %>% rowSums() %>% sum() > 0) stop("There are missing values after missForest imputation. Something went wrong.")
  
  if (verbose) message(sprintf("Dynamic train imputation done in %s minutes.", 
                               difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(list(imp_obj = list(table_cols_impute = table_cols_impute,
                             imp_obj_MF = imp_obj),
              data = data))
}

predict_impute_dynamic_MF_train <- function(imp_obj, data, 
                                            verbose = TRUE){
  
  start_time <- Sys.time()
  
  # mean/mode + LOCF imputation
  # ---------------------------
  
  table_cols_impute <- imp_obj$table_cols_impute
  
  # make integer column double
  data <- data %>% mutate_if(is.integer, as.double)
  
  data_LOCF <- data
  
  for (col in table_cols_impute$col_name){
    
    mode_binary_value <- table_cols_impute[table_cols_impute$col_name == col,"mode_binary"][[1]]
    mode_categ_value <- table_cols_impute[table_cols_impute$col_name == col,"mode_categ"][[1]]
    mean_value <- table_cols_impute[table_cols_impute$col_name == col,"mean_cont"][[1]]
    if (!is.na(mode_binary_value)) {
      value <- mode_binary_value 
    } else if (!is.na(mode_categ_value)){
      value <- mode_categ_value 
      if (is.integer(data_LOCF[,col, drop = TRUE])) value <- as.integer(value)
    } else if (!is.na(mean_value)){
      value <- mean_value 
    } else {
      value <- NA
    }
    
    # initialize LOCF imputed data for x_init
    data_LOCF[is.na(data_LOCF[,col]) & data_LOCF$LM == 0, col, drop = TRUE] <- value
    
  }
  
  # do LOCF imputation as x_init for missForest
  data_LOCF <- data_LOCF %>% 
    group_by(functioneelDossierNr, CAT_catheter_episode) %>% 
    arrange(LM) %>% 
    fill(all_of(table_cols_impute$col_name), .direction = "down") %>% 
    ungroup() %>% 
    arrange(functioneelDossierNr, CAT_catheter_episode, LM)
  
  if (is.na(data_LOCF) %>% rowSums() %>% sum() > 0) stop("There are missing values after LOCF imputation. Something went wrong.")
  
  # missForest
  # ----------
  
  # make binary columns factor
  is_binary_col <- sapply(data, function(x) all(unique(x[!is.na(x)]) %in% c(0, 1)))
  binary_vars <- names(is_binary_col)[is_binary_col]
  
  data <- data %>% 
    mutate_at(all_of(binary_vars), as.factor)
  data_LOCF <- data_LOCF %>% 
    mutate_at(all_of(binary_vars), as.factor)
  
  # make character columns factor
  is_char_col <- sapply(data, function(x) is.character(x))
  char_cols <- names(is_char_col)[is_char_col]
  char_cols <- char_cols[!char_cols %in% c("type")] # leave type (outcome) as is
  
  data <- data %>% 
    mutate_at(all_of(char_cols), as.factor)
  data_LOCF <- data_LOCF %>% 
    mutate_at(all_of(char_cols), as.factor)
  
  # impute
  data <- missForestPredict::missForestPredict(imp_obj$imp_obj_MF, newdata = data,
                                               x_init = data_LOCF)
  
  # make binary variables double
  data <- data %>% 
    mutate_at(all_of(binary_vars), function(x) as.numeric(as.character(x)))
  
  # re-calculate neutropenia based on the imputed variable 
  # (last value of WBC count or WBC Neutrophils)
  # following the same log as in calculate_BSI.R
  data <- data %>% 
    mutate(LAB_is_neutropenia = as.numeric(LAB_WBC_count_last * 1000 < 500 |
                                             LAB_WBC_Neutrophils_last * 1000 < 500)) 
  
  # calculate BMI after imputation
  data <- data %>% 
    mutate(CARE_PHY_BMI = CARE_PHY_weight_mean / CARE_PHY_length_mean^2) %>% 
    select(-c(CARE_PHY_length_mean, CARE_PHY_weight_mean))
  
  if (is.na(data) %>% rowSums() %>% sum() > 0) warning("There are missing values after missForest imputation. Something went wrong.")
  
  if (verbose) message(sprintf("Baseline test imputation done in %s minutes.", 
                               difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  return(data)
}

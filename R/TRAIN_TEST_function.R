# keep results
init_preds_TRAIN_TEST <- function(){
  
  results <- tibble(preds = double(),
                    y_true_cat = character(),
                    y_true_time = double(),
                    train_set = character(),
                    test_set = character(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    cv = character(),
                    LM = double(),
                    functioneelDossierNr = double(),
                    CAT_catheter_episode = double())
  
  return(results) 
}

init_results_TRAIN_TEST <- function(){
  
  results <- tibble(train_set = character(),
                    test_set = character(),
                    metric = character(),
                    value = numeric(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    cv = character(),
                    LM = double())
  
  return(results)
}

init_coefs_TRAIN_TEST <- function(){
  
  results <- tibble(variable = character(),
                    value = numeric(),
                    train_set = character(),
                    model = character(),
                    model_type = character(),
                    horizon = character(),
                    cv = character(),
                    LM = double())
  
  
  return(results)
}

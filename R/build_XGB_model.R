
build_XGB_model <- function(data_train, data_test,
                            model, 
                            model_type = NULL,
                            var_sel = NULL,
                            horizon = 7,
                            iters_design = 30,
                            iters_optim = 70,
                            tuning_metric = "logloss", 
                            #importance_type = "permutation",
                            name_train_set = NULL,
                            name_test_set = NULL,
                            use_gpu = TRUE,
                            save_model = FALSE,
                            ...) {
  
  # fixed XGB configuration
  n_cpu <- detectCores()
  n_rounds <- 50000
  early_stopping_rounds <- 30
  n_folds <- 5
  
  print(sprintf("n_cpu is %s", n_cpu))
  
  # keep results
  predictions <- init_preds()
  var_imp <- init_var_imp()
  hyperparams <- init_hyperparams()
  timings <- init_timings()
  
  positive_class <- "CLABSI"
  negative_class <- "no_CLABSI"
  
  do_bayes <- function(n_design = NULL, opt_steps = NULL, of = obj_fun, plot = FALSE) {
    des <- generateDesign(n = n_design,
                          par.set = getParamSet(of),
                          fun = lhs::randomLHS)
    control <- makeMBOControl() %>%
      setMBOControlTermination(., iters = opt_steps)
    ## kriging with a matern(3,2) covariance function is the default surrogate model for numerical domains
    ## but if you wanted to override this you could modify the makeLearner() call below to define your own
    ## GP surrogate model with more or less smoothness, or use an entirely different method
    run <- mbo(fun = of,
               design = des,
               learner = makeLearner("regr.km", predict.type = "se", 
                                     covtype = "matern3_2", 
                                     nugget.stability = 1e-8,
                                     control = list(trace = FALSE)),
               control = control,
               show.info = TRUE)
    if (plot){
      opt_plot <- run$opt.path$env$path %>%
        mutate(Round = row_number()) %>%
        mutate(type = case_when(Round <= n_design ~ "Design",
                                TRUE ~ "mlrMBO optimization")) %>%
        ggplot(aes(x= Round, y= y, color= type)) +
        geom_point() +
        labs(title = "mlrMBO optimization") +
        ylab("logloss")
    } else {
      opt_plot <- NULL
    }
    #print(run$x)
    return(list(run = run, plot = opt_plot))
  }
  
  # build model 
  # -----------
  
  start_time <- Sys.time()
  
  if (var_sel %in% c("LIM")){
    
    # binarize medical department, using the same code as for the CS model
    cat_cols <- "MS_medical_specialty"
    
    bin_model <- data_train %>% 
      make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))
    data_train <- bin_model$data
    
    bin_model <- data_test %>% 
      make_col_binary_drop(cat_cols, dropped_levels = list(MS_medical_specialty = "Other"))
    data_test <- bin_model$data
    
    # check cols not in train data - stop if any
    cols_not_in_train <- feature_set_1[!feature_set_1 %in% colnames(data_train)]
    cols_not_in_test <- feature_set_1[!feature_set_1 %in% colnames(data_test)]
    
    if (length(cols_not_in_train) > 0) stop("Columns not found in train data")
    if (length(cols_not_in_test) > 0) stop("Columns not found in test data")
    
    cols_keep <- feature_set_1[feature_set_1 %in% colnames(data_train)]
    cols_keep <- cols_keep[!cols_keep %in% c("functioneelDossierNr", "type", 
                                             "eventtime")]
    
  } else if (var_sel %in% c("ALL")) {
    cols_keep <- colnames(data_train)
    cols_keep <- cols_keep[!cols_keep %in% c("functioneelDossierNr", "type", 
                                             "eventtime")]
    
  } else {
    stop("var_sel value not supported yet")
  }
  
  # keep X and Y as matrices
  train_X <- data_train %>% 
    select(all_of(cols_keep)) %>% 
    mutate_if(is.character, as.factor)
  
  test_X <- data_test %>% 
    select(all_of(cols_keep)) %>% 
    mutate_if(is.character, as.factor)
  
  # outcome with horizon x days
  if (model_type == "multinomial") {
    train_y <- data_train %>% 
      select(c("type", "eventtime", "LM")) %>% 
      mutate(CLABSI = case_when(type == "CLABSI" & eventtime - LM <= horizon ~ "CLABSI", 
                                type == "Death" & eventtime - LM <= horizon ~ "Death",
                                type == "Discharge" & eventtime - LM <= horizon ~ "Discharge",
                                TRUE ~ "Censored")) %>% 
      pull(CLABSI) %>% as.factor()
    
  } else {
    stop("model type not supported")
  }
  
  if (model_type %in% c("multinomial")){
    y_true_0_1 <- ifelse(train_y == positive_class, 1, 0)
  } else {
    stop("model type not supported")
  }
  
  y_true_matrix <- matrix(cbind(y_true_0_1, 1 - y_true_0_1), ncol = 2)
  colnames(y_true_matrix) <- c(positive_class, negative_class)
  
  train_y_bin <- data_train %>% 
    select(c("type", "eventtime", "LM")) %>% 
    mutate(CLABSI = if_else(type == "CLABSI" & eventtime - LM <= horizon, "CLABSI", "no_CLABSI")) %>% 
    pull(CLABSI) %>% as.factor()
  
  test_y <- data_test %>% 
    select(c("type", "eventtime", "LM")) %>% 
    mutate(CLABSI = if_else(type == "CLABSI" & eventtime - LM <= horizon, "CLABSI", "no_CLABSI")) %>% 
    pull(CLABSI) %>% as.factor()
  
  param_set = makeParamSet(
    makeNumericParam("eta",                    lower = 0.001,   upper = 0.01), 
    makeNumericParam("gamma",                  lower = 0.5,     upper = 5), # forced a little bit higher
    makeIntegerParam("max_depth",              lower = 1L,      upper = 18L), 
    makeIntegerParam("min_child_weight",       lower = 60L,     upper = 200L), # forced a little bit higher
    makeNumericParam("subsample",              lower = 0.3,     upper = 1),
    makeNumericParam("colsample_bytree",       lower = 0.2,     upper = 1), 
    makeNumericParam("alpha",                  lower = 0.5,     upper = 10) 
  )
  
  # create binary variables
  categ_cols <- c("MS_medical_specialty", 
                  "MS_physical_ward_base",
                  "CARE_SAF_freedom_restriction_categorical_last",
                  "GEN_LM_month_categ") 
  
  categ_cols <- categ_cols[categ_cols %in% colnames(train_X)]
  
  train_X_bin <- train_X
  test_X_bin <- test_X
  
  if (length(categ_cols) > 0){
    
    train_X_bin <- make_col_binary_all(train_X_bin, categ_cols)
    train_X_bin[,categ_cols] <- NULL
    test_X_bin <- make_col_binary_all(test_X_bin, categ_cols)
    test_X_bin[,categ_cols] <- NULL
  }
  
  # if there are columns in train that are not in test, add them with 0 
  cols_not_in_test <- colnames(train_X_bin)[!colnames(train_X_bin) %in% colnames(test_X_bin)]
  if (length(cols_not_in_test) > 0) test_X_bin[,cols_not_in_test] <- 0
  # keep cols in same order
  test_X_bin <- test_X_bin[,colnames(train_X_bin)]
  
  folds_in <- splitTools::create_folds(data_train$functioneelDossierNr,
                                       k = n_folds, type = "grouped")
  folds_out <- lapply(folds_in, function(x) which(!1:nrow(data_train) %in% x))
  
  # objective <- "binary:logistic"
  objective <- "multi:softprob"
  cv_metric <- "mlogloss"
  
  train_y_label <- as.integer(train_y) - 1
  # CLABSI = 1, Censored = 0, Death = 2, Discharge = 3
  
  data_train_xgb <- xgb.DMatrix(data = as.matrix(train_X_bin),
                                label = train_y_label)
  
  n_classes <- length(levels(train_y))
  
  logloss_bin_obj <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    labels_CLABSI <- ifelse(labels == 1, 1, 0)
    
    # preds should be a matrix with 4 columns, but is not
    preds <- matrix(preds, ncol = n_classes, byrow = TRUE)
    preds_CLABSI <- preds[,2]     
    # Censored = 0, CLABSI = 1, Death = 2, Discharge = 3 ==>
    # Censored = 1, CLABSI = 2, Death = 3, Discharge = 4
    
    binary_logloss <- log_loss(labels_CLABSI, preds_CLABSI)
    
    return(list(metric = "binary_logloss", value = binary_logloss))
  }
  
  logloss_m_obj <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    labels <- factor(labels)
    #labels_CLABSI <- ifelse(labels == 1, 1, 0)
    
    # preds should be a matrix with 4 columns, but is not
    preds <- matrix(preds, ncol = n_classes, byrow = TRUE)
    #preds_CLABSI <- preds[,2]     # Censored = 0, CLABSI = 1, Death = 2, Discharge = 3 ==>
    # Censored = 1, CLABSI = 2, Death = 3, Discharge = 4
    
    #binary_logloss <- log_loss(labels_CLABSI, preds_CLABSI)
    m_logloss <- ModelMetrics::mlogLoss(labels, preds)
    
    return(list(metric = "m_logloss", value = m_logloss))
  }
  
  obj_fun <- makeSingleObjectiveFunction(
    name = "xgb_cv_bayes",
    fn =   function(x){
      
      params <- list(booster          = "gbtree",
                     eta              = x["eta"],
                     max_depth        = x["max_depth"],
                     min_child_weight = x["min_child_weight"],
                     gamma            = x["gamma"],
                     subsample        = x["subsample"],
                     colsample_bytree = x["colsample_bytree"],
                     alpha            = x["alpha"],
                     objective        = objective,
                     eval_metric      = cv_metric,
                     num_class        = n_classes)
      
      if (use_gpu) {
        params$device <- "cuda:0"
        params$tree_method <- "hist"
      }
      
      cv <- xgb.cv(params = params, 
                   data = data_train_xgb, 
                   nround = n_rounds, # Set this large and use early stopping
                   nthread = n_cpu, # parallel
                   folds = folds_out,
                   showsd = FALSE,
                   early_stopping_rounds = early_stopping_rounds, # If evaluation metric does not improve on out-of-fold sample for x rounds, stop
                   prediction = FALSE,
                   verbose = TRUE,
                   print_every_n = 1000)
      
      cv_eval <- cv$evaluation_log %>% pull(test_mlogloss_mean) %>% min()
      gc()  
      
      cv_eval
      
    },
    par.set = param_set,
    minimize = TRUE
  )
  
  # tune model 
  start_time_tuning <- Sys.time()
  runs <- do_bayes(n_design = iters_design, of = obj_fun, opt_steps = iters_optim)
  time_tuning <- as.numeric(difftime(Sys.time(), start_time_tuning, units = "secs"))
  
  best_fit <- runs$run$x
  print(best_fit)
  
  best_fit$num_class <- n_classes
  
  if (use_gpu){
    best_fit$device <- "cuda:0"
    best_fit$tree_method <- "hist"
  } 
  
  # fit final model
  start_time_final_model <- Sys.time()
  optimal_cv <- xgb.cv(params = best_fit,
                       objective = objective,
                       booster = "gbtree",
                       data = data_train_xgb,
                       nrounds = n_rounds,
                       nthread = n_cpu,
                       folds = folds_out,
                       prediction = FALSE,
                       showsd = FALSE,
                       early_stopping_rounds = early_stopping_rounds,
                       metrics = cv_metric, # use same objective as CV metric
                       verbose = 0)
  
  print(sprintf("optimal_cv$best_iteration is: %s", optimal_cv$best_iteration))
  
  XGB_model <- xgboost(params = best_fit,
                       objective = objective,
                       booster = "gbtree",
                       data = data_train_xgb,
                       nrounds = optimal_cv$best_iteration,
                       verbose = 0)
  time_final_model <- as.numeric(difftime(Sys.time(), start_time_final_model, units = "secs"))
  
  best_fit$best_iteration <- optimal_cv$best_iteration
  
  # save hyperparams
  if (use_gpu){
    best_fit$device <- NULL
  }
  best_fit$num_class <- NULL
  best_results <- best_fit %>% unlist()
  
  hyperparams <- hyperparams %>%
    add_row(hyperparameter = best_results %>% names(),
            value = best_results %>% as.character(),
            train_set = name_train_set,
            model = model,
            LM = NA_real_)
  
  # predict on test set
  start_time_predict <- Sys.time()
  test_preds <- predict(XGB_model, xgb.DMatrix(as.matrix(test_X_bin)))
  test_preds <- matrix(test_preds, ncol = n_classes, byrow = TRUE)
  test_preds <- test_preds[,2]
  time_predict <- as.numeric(difftime(Sys.time(), start_time_predict, units = "secs"))
  
  predictions <- predictions %>% 
    add_row(preds = test_preds,
            y_true_cat = test_y,
            y_true_time = NA_real_,
            train_set = name_train_set,
            test_set = name_test_set,
            model = model,
            LM = data_test$LM,
            functioneelDossierNr = data_test$functioneelDossierNr,
            CAT_catheter_episode = data_test$CAT_catheter_episode)
  
  # variable importance
  importance_matrix <- xgb.importance(colnames(data_train_xgb), 
                                      model = XGB_model)
  
  var_imp <- var_imp %>% 
    add_row(variable = importance_matrix$Feature,
            value = importance_matrix$Gain,
            train_set = name_train_set,
            model = model,
            LM = NA_real_,
            var_imp_type = "gain")
  
  # save timings 
  timings <- timings %>% 
    add_row(type = c("tuning", "build final model", "predict"),
            value = c(time_tuning, time_final_model, time_predict),
            train_set = name_train_set,
            model = model,
            LM = NA_real_) 
  
  # saved model
  if (!save_model){
    XGB_model <- NULL
  }
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # return predictions, variable importance, ...
  return(list(predictions = predictions,
              var_imp = var_imp,
              hyperparams = hyperparams,
              timings = timings,
              model = XGB_model))
  
}

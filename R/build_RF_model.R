
build_RF_model <- function(data_train, data_test,
                           model, 
                           model_type = NULL,
                           var_sel = NULL,
                           horizon = 7,
                           iters_design = 30,
                           iters_optim = 70,
                           tuning_metric = "logloss", 
                           importance_type = "permutation",
                           inbags_prefix = NULL,
                           name_train_set = NULL,
                           name_test_set = NULL,
                           save_model = FALSE,
                           ...) {
  
  # fixed RF configuration
  num_trees <- 1000
  respect_unordered_factors <- "order"
  replace <- FALSE
  probability <- TRUE
  
  # sample fraction to train the randomForestSRC model for getting minimal depth (out of 100)
  sample_fraction <- 60
  
  # keep results
  predictions <- init_preds()
  var_imp <- init_var_imp()
  hyperparams <- init_hyperparams()
  timings <- init_timings()
  OOB_predictions <- init_preds()
  
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
        ylab("Brier")
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
    
  } else if (var_sel %in% c("VST")) {
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
  
  if (var_sel %in% c("VST")){
    
    # data fit
    if (model_type %in% c("multinomial")){
      data_fit <- cbind(train_X, train_y)
    } 
    if (model_type %in% c("multinomial")){
      form <- as.formula("train_y ~ .")
    } 
    
    # load inbags
    # load inbag samples for all trees using sampling by adm id
    inbag_file <- paste0(inbags_prefix, "_", sample_fraction)
    load(inbag_file)
    
    # adjust inbag to min size of all bags
    n_inbag <- colSums(inbags)
    min_n_inbag <- min(n_inbag)
    len_inbags <- dim(inbags)[[1]]
    
    inbags <- apply(inbags, 2, function(x) subsample_inbag(x, min_n_inbag, len_inbags))
    
    RF_model_vars <- rfsrc(form, data_fit,
                           nodesize = 2,
                           mtry = ceiling(ncol(train_X)/3),
                           ntree = num_trees,
                           # specify subsamples
                           bootstrap = "by.user",
                           samp = inbags,
                           save.memory = TRUE, 
                           do.trace = FALSE,
                           split.depth = "all.trees",
                           importance="none")
    
    # get top vars acc. to minimal depth
    md_obj <- max.subtree(RF_model_vars, conservative = TRUE)
    depth <- md_obj$order[, 1]
    
    print(sort(depth))
    
    upper_n_feat <- ceiling(ncol(train_X) * 0.75)
    
    # make param set using number of features & mtry as fraction from it
    param_set = makeParamSet(
      makeIntegerParam("min.node.size", lower = 10L, upper = 4000L), 
      makeIntegerParam("n_feat", lower = 15L, upper = upper_n_feat), 
      makeIntegerParam("mtry_percent", lower = 2L, upper = 90L),
      makeIntegerParam("sample.fraction", lower = 30L, upper = 80L))
    
    
  } else { # LIM
    upper_n_feat <- ceiling(ncol(train_X) * 0.75)
    
    param_set = makeParamSet(
      makeIntegerParam("min.node.size", lower = 10L, upper = 4000L), 
      makeIntegerParam("mtry", lower = 2L, upper = upper_n_feat),
      makeIntegerParam("sample.fraction", lower = 30L, upper = 80L))
  }
  
  obj_fun <- makeSingleObjectiveFunction(
    name = "RF_ranger",
    fn =   function(x){
      
      # load inbag samples for all trees using sampling by adm id
      inbag_file <- paste0(inbags_prefix, "_", x[["sample.fraction"]])
      load(inbag_file)
      # transform to list
      inbags <- lapply(seq_len(ncol(inbags)), function(i) inbags[,i])
      
      if (var_sel %in% c("VST")){
        # select top n_feat and use only these
        depth <- sort(depth)
        feats_ordered <- names(depth)
        feats_use <- feats_ordered[1:x[["n_feat"]]]
        
        # make mtry integer from fraction of features
        mtry_int <- ceiling(x[["n_feat"]] * x[["mtry_percent"]] / 100)
        
        RF_obj <- ranger(x = train_X[,feats_use],
                         y = train_y,
                         # fixed params
                         num.trees = num_trees,
                         respect.unordered.factors = respect_unordered_factors,
                         replace = replace,
                         probability = probability,
                         # tuned params
                         mtry = mtry_int,
                         min.node.size = x[["min.node.size"]],
                         inbag = inbags, # by adm id
                         # get importance
                         importance = "none",
                         verbose = FALSE)
      } else { # LIM
        RF_obj <- ranger(x = train_X,
                         y = train_y,
                         # fixed params
                         num.trees = num_trees,
                         respect.unordered.factors = respect_unordered_factors,
                         replace = replace,
                         probability = probability,
                         # tuned params
                         mtry = x[["mtry"]],
                         min.node.size = x[["min.node.size"]],
                         inbag = inbags, # by adm id
                         # get importance
                         importance = "none",
                         verbose = FALSE)
      }
      
      # take binary OOB predictions for all model types
      if (model_type == "multinomial"){
        OOB_preds <- RF_obj$predictions %>% as.data.frame()
        cols_neg <- colnames(OOB_preds)[!colnames(OOB_preds) %in% positive_class]
        OOB_preds[,negative_class] <- rowSums(OOB_preds[,cols_neg])
        OOB_preds <- OOB_preds[,c(positive_class, negative_class)]
      } else {
        stop("model type not supported")
      }
      
      if (tuning_metric == "BS"){
        tuning_metric_value <- BS(OOB_preds[,c(positive_class, negative_class)], 
                                  y_true_matrix)
      } else if (tuning_metric == "AUROC"){
        tuning_metric_value <- ModelMetrics::auc(y_true_matrix[,positive_class], 
                                                 OOB_preds[,positive_class])
        tuning_metric_value <- - tuning_metric_value # to minimize
      } else if (tuning_metric == "BSS"){
        tuning_metric_value <- 1 - BSnorm(OOB_preds[,c(positive_class, negative_class)], 
                                          y_true_matrix)
        tuning_metric_value <- - tuning_metric_value # to minimize
      } else if (tuning_metric == "logloss"){
        obs <- y_true_matrix[,positive_class]
        pred <- OOB_preds[,positive_class]
        pred <- pmax(pmin(pred, 1-10^-15), 10^-15)
        tuning_metric_value <- -mean(obs * log(pred) + (1 - obs) * log(1 - pred))
      } else {
        stop ("tuning_metric not supported")
      }
      
      tuning_metric_value
      
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
  
  # get subsample file based on subsample percentage
  subsample_size <- best_fit[["sample.fraction"]]
  inbag_file <- paste0(inbags_prefix, "_", subsample_size)
  load(inbag_file)
  # transform to list
  inbags <- lapply(seq_len(ncol(inbags)), function(i) inbags[,i])
  
  # build final model 
  if (var_sel %in% c("VST")){
    
    # select top n_feat and use only these
    depth <- sort(depth)
    feats_ordered <- names(depth)
    feats_use <- feats_ordered[1:best_fit[["n_feat"]]]
    
    # data_fit <- cbind(train_X[,feats_use], train_y)
    
    # make mtry integer from fraction of features
    mtry_int <- ceiling(best_fit[["n_feat"]] * best_fit[["mtry_percent"]] / 100)
    
    start_time_final_model <- Sys.time()
    RF_model <- ranger(x = train_X[,feats_use],
                       y = train_y,
                       # fixed params
                       num.trees = num_trees,
                       respect.unordered.factors = respect_unordered_factors,
                       replace = replace,
                       probability = probability,
                       # tuned params
                       mtry = mtry_int,
                       min.node.size = best_fit[["min.node.size"]],
                       inbag = inbags, # corresponds to best subsample size
                       # get importance
                       importance = "permutation",
                       scale.permutation.importance = TRUE,
                       verbose = FALSE)
    time_final_model <- as.numeric(difftime(Sys.time(), start_time_final_model, units = "secs"))
  } else { # ALL, LIM
    
    start_time_final_model <- Sys.time()
    RF_model <- ranger(x = train_X,
                       y = train_y,
                       # fixed params
                       num.trees = num_trees,
                       respect.unordered.factors = respect_unordered_factors,
                       replace = replace,
                       probability = probability,
                       # tuned params
                       mtry = best_fit[["mtry"]],
                       min.node.size = best_fit[["min.node.size"]],
                       inbag = inbags, # corresponds to best subsample size
                       # get importance
                       importance = "permutation",
                       scale.permutation.importance = TRUE,
                       verbose = FALSE)
    time_final_model <- as.numeric(difftime(Sys.time(), start_time_final_model, units = "secs"))
  }
  
  # save hyperparams
  best_results <- best_fit %>% unlist()
  
  hyperparams <- hyperparams %>%
    add_row(hyperparameter = best_results %>% names(),
            value = best_results %>% as.character(),
            train_set = name_train_set,
            model = model,
            LM = NA_real_)
  
  # save OOB predictions 
  OOB_preds <- RF_model$predictions[,positive_class]
  OOB_predictions <- OOB_predictions %>% 
    add_row(preds = OOB_preds,
            y_true_cat = train_y,
            y_true_time = NA_real_,
            train_set = name_train_set,
            test_set = name_test_set,
            model = model,
            LM = data_train$LM,
            functioneelDossierNr = data_train$functioneelDossierNr,
            CAT_catheter_episode = data_train$CAT_catheter_episode)
  
  # predict on test set
  start_time_predict <- Sys.time()
  test_preds <- predict(RF_model, test_X)$predictions[,positive_class]
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
  var_imp <- var_imp %>% 
    add_row(variable = names(RF_model$variable.importance),
            value = RF_model$variable.importance,
            train_set = name_train_set,
            model = model,
            LM = NA_real_,
            var_imp_type = "permutation")
  
  # save timings 
  timings <- timings %>% 
    add_row(type = c("tuning", "build final model", "predict"),
            value = c(time_tuning, time_final_model, time_predict),
            train_set = name_train_set,
            model = model,
            LM = NA_real_) 
  
  message(sprintf("DONE in %s minutes.", 
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
  # saved model
  if (!save_model){
    RF_model <- NULL
  }
  
  # return predictions, variable importance, ...
  return(list(predictions = predictions,
              var_imp = var_imp,
              hyperparams = hyperparams,
              timings = timings,
              OOB_predictions = OOB_predictions,
              model = RF_model))
  
}

# function to reduce the inbags to min inbag size for RFSRC (limitation of the package)
subsample_inbag <- function(inbag, subsample_size, len_inbags) {
  ids <- which(inbag == 1)
  ids_selected <- sort(sample(ids, subsample_size, replace = FALSE))
  inbag_selected <- rep(0, len_inbags)
  inbag_selected[ids_selected] <- 1
  
  return(inbag_selected)
}

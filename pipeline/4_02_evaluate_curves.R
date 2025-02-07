# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

library(ROCR)

pred_mat_x_y <- function(preds){
  
  test_sets <- unique(preds$test_set)
  pred_mat_list <- list()
  
  for (i in 1:length(test_sets)){
    preds_ts <- preds %>% filter(test_set == test_sets[i]) %>% 
      mutate(y_true_0_1 = if_else(y_true_cat == positive_class, 1, 0)) %>% 
      select(y_true_0_1, preds)
    
    pred_mat <- prediction(preds_ts$preds, labels = preds_ts$y_true_0_1)
    pred_mat_list[[test_sets[[i]]]] <- pred_mat
  }
  
  return(pred_mat_list)
}

lift_table <- function(y_true, preds, bin_number = 10) {
  cbind(y_true, preds) %>% as.data.frame() %>% 
    dplyr::mutate(yhat_bin = ggplot2::cut_number(preds, bin_number)) %>%
    dplyr::group_by(yhat_bin) %>%
    dplyr::summarise(mean_y = mean(y_true), mean_yhat = mean(preds)) %>% 
    dplyr::select(mean_y, mean_yhat)
}

# function to subsample only n x points from a curve 
# AUROC & AUPRC have points = number of predictions - too high - inefficient for storing and plotting
subsample_x <- function(df, n = 2000) {
  ideal_spacing <- seq(min(df$x), max(df$x), length.out=n)
  ids <- sapply(ideal_spacing, function(x) which.min(abs(df$x-x)) )
  df <- df[ids,]
  return(df)
}

calib_rcs_CI <- function(y, preds){
  df_result <- NULL
  
  do_it <- function(y,preds){
    calib_curves_rcs_splines <- rcspline_plot(preds, y, model="logistic", nk=3, show="prob", plot = FALSE)
    calib_curves_rcs_splines$knots <- NULL  
    names(calib_curves_rcs_splines)[names(calib_curves_rcs_splines) == "xbeta"] <- "y"
    calib_curves_rcs_splines <- data.frame(calib_curves_rcs_splines)
    
    return(calib_curves_rcs_splines)
  }
  
  # return NULL on a dataset on which the curve fails completely
  df_result <- tryCatch({
    do_it(y,preds)}, 
    error = function(e){
      NULL
    })
  
  return(df_result)
}



# load LM data for TEMPORAL evaluation per year and per ward
load("data_for_models/2018_2020/LM_data.RData")
LM_data_18_20 <- LM_data
load("data_for_models/2014_2017/LM_data.RData")
LM_data_14_17 <- LM_data

# prediction files
preds <- init_preds()

pred_files <- list.files(preds_path_test, full.names=TRUE)

pred_files_OOB <- pred_files[str_detect(pred_files, "OOB_")]
pred_files <- pred_files[!pred_files %in% pred_files_OOB]

positive_class <- "CLABSI"

print(pred_files)

# TO RUN ONLY SELECTED MODELS
# pred_files <- pred_files[str_detect(pred_files, "preds_SL_")]


for (pf in pred_files){
  
  # prepare LM_data for year evaluation
  LM_data_test_year <- LM_data_18_20 %>% 
    mutate(year = lubridate::year(CAT_end_time_episode)) %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, year) %>%
    mutate(year = if_else(year > 2020, 2020, year))
  
  LM_data_train_year <- LM_data_14_17 %>% 
    mutate(year = lubridate::year(CAT_end_time_episode)) %>% 
    select(functioneelDossierNr, CAT_catheter_episode, LM, year) %>%
    mutate(year = if_else(year > 2017, 2017, year))
  
  LM_data_year <- LM_data_test_year %>% 
    add_row(LM_data_train_year)
  
  print(pf)
  start_time <- Sys.time()
  
  load(pf)
  
  model <- predictions %>% distinct(model) %>% pull(model)
  if (length(model) > 1)  stop("More than 1 model in a file")
  
  pred_mat <- pred_mat_x_y(predictions)
  
  # ROC curve
  pred_mat_obj <- lapply(pred_mat, function(x) ROCR::performance(x, "tpr", "fpr"))
  pred_mat_obj <- lapply(pred_mat_obj, function(obj) tibble(x = obj@x.values[[1]],
                                                            y = obj@y.values[[1]]))
  
  # sample only 200 points from the x of the curve (equally spaced) - more efficient
  pred_mat_obj <- lapply(pred_mat_obj, subsample_x)
  
  names(pred_mat_obj) <- if (is.null(names(pred_mat_obj))) seq_len(length(pred_mat_obj)) else names(pred_mat_obj)
  
  pred_mat_df <- do.call("rbind", lapply(names(pred_mat_obj), function(x) cbind(test_set = x, pred_mat_obj[[x]])))
  
  pred_mat_df_AUROC <- pred_mat_df %>% 
    add_column(model = model,
               curve_type = "AUROC",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               year = "all",
               upper = NA_real_,
               lower = NA_real_) 
  
  rm(pred_mat_obj, pred_mat_df)
  
  # PR curve
  pred_mat_obj <- lapply(pred_mat, function(x) ROCR::performance(x, "prec", "rec"))
  pred_mat_obj <- lapply(pred_mat_obj, function(obj) tibble(x = obj@x.values[[1]],
                                                            y = obj@y.values[[1]]))
  
  # sample only 200 points from the x of the curve (equally spaced) - more efficient
  pred_mat_obj <- lapply(pred_mat_obj, subsample_x)
  
  names(pred_mat_obj) <- if (is.null(names(pred_mat_obj))) seq_len(length(pred_mat_obj)) else names(pred_mat_obj)
  
  pred_mat_df <- do.call("rbind", lapply(names(pred_mat_obj), function(x) cbind(test_set = x, pred_mat_obj[[x]])))
  
  pred_mat_df_AUPRC <- pred_mat_df %>% 
    add_column(model = model,
               curve_type = "AUPRC",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               year = "all",
               upper = NA_real_,
               lower = NA_real_) 
  
  rm(pred_mat_obj, pred_mat_df)
  
  # calibration curves - deciles
  calib_curves_deciles <- predictions %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    select(preds, y_true, test_set) %>% 
    group_by(test_set) %>% 
    summarise(calibration_values = list(lift_table(y_true, preds))) %>%
    ungroup() %>% 
    unnest(calibration_values) %>% 
    rename(x = mean_yhat, y = mean_y) 
  
  calib_curves_deciles <- calib_curves_deciles %>% 
    add_column(model = model,
               curve_type = "calib_deciles",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               year = "all",
               upper = NA_real_,
               lower = NA_real_) 
  
  # rcs_spline version to add lower and upper confidence intervals 
  calib_curves_rcs_splines <-  predictions %>%
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set) %>%
    summarise(calibration_values = list(calib_rcs_CI(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_rcs_splines <- calib_curves_rcs_splines %>%
    add_column(model = model,
               curve_type = "calib_rcs",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               year = "all")
  
  # rcs_spline - PER YEAR
  calib_curves_rcs_splines_year <-  predictions %>%
    left_join(LM_data_year,
              by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, year) %>%
    summarise(calibration_values = list(calib_rcs_CI(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_rcs_splines_year <- calib_curves_rcs_splines_year %>% 
    add_column(model = model,
               curve_type = "calib_rcs",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all") %>% 
    mutate(year = as.character(year))
  
  # rcs_spline - per LM
  calib_curves_rcs_splines_LM <-  predictions %>%
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, LM) %>%
    summarise(calibration_values = list(calib_rcs_CI(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_rcs_splines_LM <- calib_curves_rcs_splines_LM %>%
    add_column(model = model,
               curve_type = "calib_rcs",
               y_all = NA_real_,
               y_none = NA_real_,
               year = "all") %>% 
    mutate(LM = as.character(LM))
    
  # rcs_spline - PER YEAR and LM
  calib_curves_rcs_splines_year_LM <-  predictions %>%
    left_join(LM_data_year,
              by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, year, LM) %>%
    summarise(calibration_values = list(calib_rcs_CI(y_true, preds))) %>%
    ungroup() %>%
    #distinct(train_set, test_set, calibration_values) %>%
    unnest(calibration_values) 
  
  calib_curves_rcs_splines_year_LM <- calib_curves_rcs_splines_year_LM %>% 
    add_column(model = model,
               curve_type = "calib_rcs",
               y_all = NA_real_,
               y_none = NA_real_) %>% 
    mutate(LM = as.character(LM),
           year = as.character(year))
  
  # calibration curves - splines
  calib_curves_splines <- predictions %>%
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set) %>%
    summarise(calibration_values = list(calib_curves_x_y(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_splines <- calib_curves_splines %>% 
    add_column(model = model,
               curve_type = "calib_splines",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               year = "all",
               upper = NA_real_,
               lower = NA_real_)
  
  # calibration curves - splines - PER YEAR
  calib_curves_splines_year <- predictions %>%
    left_join(LM_data_year,
              by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
    mutate(year = as.character(year)) %>% 
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, year) %>%
    summarise(calibration_values = list(calib_curves_x_y(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_splines_year <- calib_curves_splines_year %>% 
    add_column(model = model,
               curve_type = "calib_splines",
               y_all = NA_real_,
               y_none = NA_real_,
               LM = "all",
               upper = NA_real_,
               lower = NA_real_)
  
  # calibration curves - splines - PER LM
  calib_curves_splines_LM <- predictions %>% 
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, LM) %>%
    summarise(calibration_values = list(calib_curves_x_y(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_splines_LM <- calib_curves_splines_LM %>% 
    add_column(model = model,
               curve_type = "calib_splines",
               y_all = NA_real_,
               y_none = NA_real_,
               year = "all",
               upper = NA_real_,
               lower = NA_real_) %>% 
    mutate(LM = as.character(LM))
  
  # calibration curves - splines - PER YEAR AND LM
  calib_curves_splines_year_LM <- predictions %>%
    left_join(LM_data_year,
              by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
    mutate(year = as.character(year)) %>% 
    mutate(preds = if_else(preds == 0, 0.0000000001, preds),
           preds = if_else(preds == 1, 0.9999999999, preds)) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    group_by(test_set, year, LM) %>%
    summarise(calibration_values = list(calib_curves_x_y(y_true, preds))) %>%
    ungroup() %>%
    unnest(calibration_values) 
  
  calib_curves_splines_year_LM <- calib_curves_splines_year_LM %>% 
    add_column(model = model,
               curve_type = "calib_splines",
               y_all = NA_real_,
               y_none = NA_real_,
               upper = NA_real_,
               lower = NA_real_) %>% 
    mutate(LM = as.character(LM))
  
  # decision curves
  dca_df <- predictions %>% 
    select(preds, y_true_cat, test_set) %>% 
    mutate(y_true = if_else(y_true_cat == positive_class, 1, 0)) %>% 
    as.data.frame()
  
  all_test_sets <- unique(dca_df$test_set)
  
  dca_df_1 <- dca_df %>% 
    filter(test_set == all_test_sets[[1]]) %>% 
    as.data.frame()
  
  dca_results_1 <- dca(outcome = dca_df_1$y_true,
                       preds = dca_df_1$preds,
                       xstart = 0.001,
                       xstop = 0.25,
                       xby = 0.0001)
  
  dca_results <- data.frame(matrix(ncol = ncol(dca_results_1) + 1, nrow = 0))
  colnames(dca_results) <- c(colnames(dca_results_1), "test_set")
  dca_results$test_set <- as.character(dca_results$test_set)
  
  for (ts in all_test_sets) {
    dca_df_this <- dca_df %>%
      filter(test_set == ts) %>%
      as.data.frame()
    
    dca_results_this <- dca(outcome = dca_df_this$y_true,
                            preds = dca_df_this$preds,
                            xstart = 0.001,
                            xstop = 0.22,
                            xby = 0.0006)
    dca_results_this$test_set <- ts
    
    dca_results <- dca_results %>%
      add_row(dca_results_this)
  }
  
  dca_results <- dca_results %>% 
    rename(x = threshold,
           y = pred,
           y_all = all,
           y_none = none) %>% 
    add_column(model = model,
               curve_type = "DCA",
               year = "all",
               LM = "all",
               upper = NA_real_,
               lower = NA_real_)
  
  # write to disk
  model_curves <- pred_mat_df_AUROC %>% 
    add_row(pred_mat_df_AUPRC) %>% 
    add_row(calib_curves_deciles) %>% 
    add_row(calib_curves_splines) %>% 
    add_row(dca_results) %>% 
    add_row(calib_curves_rcs_splines) %>% 
    add_row(calib_curves_rcs_splines_LM) %>% 
    add_row(calib_curves_rcs_splines_year) %>% 
    add_row(calib_curves_rcs_splines_year_LM)
  
  save(model_curves, file = paste0("results_train_test/curves_", model, ".Rdata"))
  
  rm(model_curves, pred_mat_df_AUROC, pred_mat_df_AUPRC, 
     calib_curves_deciles, calib_curves_splines, dca_results)
  
  message(sprintf("DONE in %s minutes.", difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))
  
}


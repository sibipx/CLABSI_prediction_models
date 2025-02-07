#library(precrec)

init_preds <- function(){
  
  results <- tibble(preds = double(),
                    y_true_cat = character(),
                    y_true_time = double(),
                    train_set = character(),
                    test_set = character(),
                    model = character(),
                    LM = double(),
                    functioneelDossierNr = double(),
                    CAT_catheter_episode = double())
  
  return(results) 
}

init_var_imp <- function(){
  
  results <- tibble(variable = character(),
                    value = numeric(),
                    train_set = character(),
                    model = character(),
                    LM = double(),
                    var_imp_type = character())
  
  return(results)
}

init_results <- function(){
  
  results <- tibble(model = character(),
                    train_set = character(),
                    test_set = character(),
                    metric = character(),
                    value = numeric(),
                    type = character(),
                    subgroup = character(),
                    LM = double())
  
  return(results)
}

init_hyperparams <- function(){
  
  results <- tibble(hyperparameter = character(),
                    value = character(),
                    train_set = character(),
                    model = character(),
                    LM = double())
  
  return(results)
}

init_timings <- function(){
  
  results <- tibble(type = character(),
                    value = double(),
                    train_set = character(),
                    model = character(),
                    LM = double())
  
  return(results)
}

logit <- function (p) log(p/(1 - p))
expit <- function (x) exp(x)/(1 + exp(x))

cox_first_degree <- function(y, prob){
  dat <- data.frame(e = prob, o = y)
  dat$e[dat$e == 0] = 0.0000000001
  dat$e[dat$e == 1] = 0.9999999999
  dat$logite <- logit(dat$e)
  
  mfit1 = glm(formula = o~I(logite),
              family = binomial(link = "logit"), dat)
  
  slope = mfit1$coefficients[2]
  
  mfit2 = glm(formula = o~offset(logite),
              family = binomial(link = "logit"), dat)
  
  intercept = mfit2$coefficients[1]
  
  return(list(slope = slope, intercept = intercept))
}

# stolen and adapted from library gbm to return x and y values iso plotting
calib_curves_x_y <- function(y,p,
                             distribution="bernoulli",
                             replace=TRUE,
                             line.par=list(col="black"),
                             shade.col="lightyellow",
                             shade.density=NULL,
                             rug.par=list(side=1),
                             xlab="Predicted value",
                             ylab="Observed average",
                             xlim=NULL,ylim=NULL,
                             knots=NULL,df=6,
                             ...)
{
  
  df_result <- NULL
  
  do_it <- function(y,p,
                    distribution="bernoulli",
                    replace=TRUE,
                    line.par=list(col="black"),
                    shade.col="lightyellow",
                    shade.density=NULL,
                    rug.par=list(side=1),
                    xlab="Predicted value",
                    ylab="Observed average",
                    xlim=NULL,ylim=NULL,
                    knots=NULL,df=6,
                    ...){
    p <- logit(p)
    data <- data.frame(y=y,p=p)
    
    if(is.null(knots) && is.null(df))
      stop("Either knots or df must be specified")
    if((df != round(df)) || (df<1))
      stop("df must be a positive integer")
    
    if(distribution=="bernoulli")
    {
      family1 = binomial
    } else if(distribution=="poisson")
    {
      family1 = poisson
    } else
    {
      family1 = gaussian
    }
    gam1 <- glm(y~splines::ns(p,df=df,knots=knots),data=data,family=family1)
    
    x <- seq(min(p),max(p),length=200)
    yy <- predict(gam1,newdata=data.frame(p=x),se.fit=TRUE,type="response")
    
    x <- x[!is.na(yy$fit)]
    yy$se.fit <- yy$se.fit[!is.na(yy$fit)]
    yy$fit <- yy$fit[!is.na(yy$fit)]
    
    if(!is.na(shade.col))
    {
      se.lower <- yy$fit-2*yy$se.fit
      se.upper <- yy$fit+2*yy$se.fit
      if(distribution=="bernoulli")
      {
        se.lower[se.lower < 0] <- 0
        se.upper[se.upper > 1] <- 1
      }
      if(distribution=="poisson")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(distribution=="gamma")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(distribution=="tweedie")
      {
        se.lower[se.lower < 0] <- 0
      }
      if(is.null(xlim)) xlim <- range(se.lower,se.upper,x)
      if(is.null(ylim)) ylim <- range(se.lower,se.upper,x)
    }
    else
    {
      if(is.null(xlim)) xlim <- range(yy$fit,x)
      if(is.null(ylim)) ylim <- range(yy$fit,x)
    }
    
    x <- expit(x)
    
    df_result <- data.frame(x = x, y = yy$fit)
    return(df_result)
  }
  
  # return NULL on a dataset on which the curve fails completely
  df_result <- tryCatch({
    do_it(y,p,
          distribution="bernoulli",
          replace=TRUE,
          line.par=list(col="black"),
          shade.col="lightyellow",
          shade.density=NULL,
          rug.par=list(side=1),
          xlab="Predicted value",
          ylab="Observed average",
          xlim=NULL,ylim=NULL,
          knots=NULL,df=6,
          ...)}, 
    error = function(e){
      NULL
    })
  
  return(df_result)
}

AUPRC <- function(preds, y_true, positive_class){
  curves <- precrec::evalmod(scores = preds, 
                             labels = y_true, 
                             posclass = positive_class)
  aucs <- precrec::auc(curves)
  AUPRC <- aucs[aucs$curvetypes == "PRC", "aucs"]
  
  return(AUPRC)
}

# decision curve analysis
# shamelessly stolen from: https://www.mskcc.org/departments/epidemiology-biostatistics/biostatistics/decision-curve-analysis

#dca <- function(data, outcome, preds, xstart=0.01, xstop=0.99, xby=0.01, 
#                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
#                interventionper=100, smooth=FALSE,loess.span=0.10) {
dca <- function(outcome, preds, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  harm <- 0
  
  #########  CALCULATING NET BENEFIT   #########
  N <- length(outcome)
  event_rate <- mean(outcome)
  
  thresholds <- seq(from = xstart, to = xstop, by = xby)
  
  nb_all <- event_rate - (1 - event_rate) * thresholds/(1 - thresholds)
  nb_none <- 0
  
  # # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  nb_preds <- sapply(thresholds, function(t) {
    TP <- sum((preds >= t) & (outcome == 1))
    FP <- sum((preds >= t) & (outcome == 0))
    
    nb <- TP/N - FP/N * (t/(1 - t)) - harm
    return(nb)
  })
  
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  if (smooth==TRUE){
    
    lws=loess(data.matrix(nb_preds[!is.na(nb_preds)]) ~ 
                data.matrix(thresholds[!is.na(nb_preds)]),
              span=loess.span)
    nb_preds_sm <- nb_preds
    nb_preds_sm[!is.na(nb_preds)] <- lws$fitted
    
  }
  
  return(data.frame(threshold = thresholds,
                    pred = nb_preds,
                    all = nb_all,
                    none = nb_none))
  
}  

log_loss <- function(obs, pred){
  
  pred <- pmax(pmin(pred, 1-10^-15), 10^-15)
  
  logloss <- -mean(obs * log(pred) + (1 - obs) * log(1 - pred))
  
  return(logloss)
}

BS <- function (probabilities, y) {
  mean(rowSums((probabilities - y)^2))
}

#' Normalized Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Normalized Brier Score
#' @keywords internal
#' @noRd

BSnorm <- function (probabilities, y) {
  # reference is a "no skill learner" that predicts the class prevalence
  BS_reference <- BS(matrix(rep(colMeans(y), nrow(y)), nrow = nrow(y), byrow = TRUE), y)
  if (BS_reference == 0){ # avoid division by 0
    return(0)
  } else {
    return(BS(probabilities, y) / BS_reference)
  }
}

# evaluate metrics for a model (given predictions and true outcome)
evaluate_metrics <- function(preds, y_true, positive_class = "CLABSI", 
                             thresholds = NULL,
                             full_calib = TRUE,
                             adm_id = NULL, # calculates new alerts
                             CAT_ep = NULL,
                             LM_preds = NULL){
  
  negative_class <- "no_CLABSI"
  
  # make categorical if multiclass
  y_true <- ifelse(y_true == positive_class, positive_class, negative_class)
  
  y_true_0_1 <- ifelse(y_true == positive_class, 1, 0)
  
  # if only 1 class is present in test data return all NA
  if (length(unique(y_true_0_1)) == 1 | sum(y_true_0_1) < 5){
    metrics <- c("AUROC" = NA_real_)
    metrics["mean_calibration"] <- NA_real_
    metrics["BS"] <- NA_real_
    metrics["BSS"] <- NA_real_
    metrics["logloss"] <- NA_real_
    
    if (full_calib){
      metrics["slope"] <- NA_real_
      metrics["intercept"] <- NA_real_
    }
    
    metrics["ECE"] <- NA_real_
    metrics["ECI"] <- NA_real_
    
    metrics["AUPRC"] <- NA_real_
    
    return(metrics)
    
  }
  
  # make y matrixs
  y_test_matrix <- matrix(cbind(y_true_0_1, 1 - y_true_0_1), ncol = 2)
  colnames(y_test_matrix) <- c(positive_class, negative_class)
  
  # make preds matrix
  preds_matrix <- matrix(cbind(preds, 1 - preds), ncol = 2)
  colnames(preds_matrix) <- c(positive_class, negative_class)
  
  # ROC + CI using pROC, which calculates DeLong CI
  roc_curve <- pROC::roc(y_true_0_1, preds)
  auc_value <- pROC::auc(roc_curve)
  ci_delong <- pROC::ci.auc(roc_curve, method = "delong")
  
  metrics <- c("AUROC" = auc_value)
  metrics["AUROC_lower"] <- ci_delong[1]
  metrics["AUROC_upper"] <- ci_delong[3]
  
  # E:O + CI
  estimated <- mean(preds)
  observed <- mean(y_true_0_1)
  EO <- estimated / observed
  metrics["mean_calibration"] <- EO  # estimated over observed
  
  # SE log EO
  n <- length(y_true_0_1)
  log_EO <- log(EO)
  SE_log_EO <- sqrt((1-observed)/sum(y_true_0_1)) # assumes E fixed (zero variance)
  
  # SE_E <- sqrt(estimated * (1 - estimated) / n)
  # SE_O <- sqrt(observed * (1 - observed) / n)
  # SE_log_EO <- sqrt((SE_E / estimated)^2 + (SE_O / observed)^2) # uses var(log(x)) ~ var(x) / x^2
  
  # CI log Eo
  log_EO_lower <- log_EO - 1.96 * SE_log_EO
  log_EO_upper <- log_EO + 1.96 * SE_log_EO
  EO_lower <- exp(log_EO_lower) # backtransform
  EO_upper <- exp(log_EO_upper) # backtransform
  
  metrics["mean_calibration_lower"] <- EO_lower
  metrics["mean_calibration_upper"] <- EO_upper
  
  # other metrics (no CI)
  metrics["BS"] <- BS(preds_matrix, y_test_matrix[,colnames(y_test_matrix)])
  metrics["BSS"] <- 1 - BSnorm(preds_matrix, y_test_matrix[,colnames(y_test_matrix)])
  metrics["logloss"] <- log_loss(y_true_0_1, preds)
  
  if (full_calib){
    slope_intercept <- cox_first_degree(y_true_0_1, preds)
    metrics["slope"] <- slope_intercept$slope
    metrics["intercept"] <- slope_intercept$intercept
    
    metrics["ECE"] <- CalibratR::getECE(y_true_0_1, preds)
    
    get_ECI_loess <- function(y_true_0_1, preds){
      pmcalib_curve <- ECI(y_true_0_1, preds)
      metrics["ECI"] <- pmcalib_curve
    }
    metrics["ECI"] <- NA_real_
    try(
      metrics["ECI"] <- get_ECI_loess(y_true_0_1, preds)
    )
  }
  
  # AUPRC
  curves <- precrec::evalmod(scores = preds, labels = y_true, posclass = positive_class)
  aucs <- precrec::auc(curves)
  AUPRC <- aucs[aucs$curvetypes == "PRC", "aucs"]
  metrics["AUPRC"] <- AUPRC
  
  # Metrics at fixed thresholds
  for (t in thresholds){
    
    N <- length(y_true)
    N_pos <- sum(y_true_0_1)
    N_neg <- N - N_pos 
    
    # Confusion matrix metrics t
    class_preds_t <- ifelse(preds >= t, 1, 0) 
    nr_alerts_t <- sum(class_preds_t)
    nr_predicted_negative_t <- N - nr_alerts_t
    TP_t <- sum((preds >= t) & (y_true_0_1 == 1))
    FP_t <- sum((preds >= t) & (y_true_0_1 == 0))
    TN_t <- sum((preds < t) & (y_true_0_1 == 0))
    
    # Sensitivity at t
    Sensitvity_at_t <- TP_t/N_pos
    metrics[paste0("Sensitiviy_at_thresh_", t)] <- Sensitvity_at_t
    
    # Specificity at t
    Specificity_at_t <- TN_t/N_neg
    metrics[paste0("Specificity_at_thresh_", t)] <- Specificity_at_t
    
    # Alert rate at t
    Alert_rate_at_t <- nr_alerts_t/N
    metrics[paste0("Alert_rate_at_thresh_", t)] <- Alert_rate_at_t
    
    # PPV at t
    PPV_at_t <- TP_t/nr_alerts_t
    metrics[paste0("PPV_at_thresh_", t)] <- PPV_at_t
    
    # NPV at t
    NPV_at_t <- TN_t/nr_predicted_negative_t
    metrics[paste0("NPV_at_thresh_", t)] <- NPV_at_t
    
    # NB at t
    NB_at_t <- TP_t/N - (FP_t/N) * (t/(1-t))
    metrics[paste0("NB_at_thresh_", t)] <- NB_at_t
    
    # NB treat all at t
    event_rate <- N_pos / N
    nb_all <- event_rate - (1 - event_rate) * t/(1 - t)
    metrics[paste0("NB_all_at_thresh_", t)] <- nb_all
    
    # Alert rate for first alert raised in the system
    # assumes a new alert is created when the patient is transferred when evaluation is done per ward 
    # (gaps in catheter episode, without consecutive LMs)
    if (is.null(adm_id) | is.null(CAT_ep) | is.null(LM_preds)){
      metrics[paste0("New_alerts_at_thresh_", t)] <- NA_real_
      metrics[paste0("New_alerts_rate_at_thresh_", t)] <- NA_real_
    } else {
      new_alerts <- tibble(adm_id = adm_id, 
                           CAT_ep = CAT_ep, 
                           LM_preds = LM_preds, 
                           preds = preds) %>%
        mutate(alert = if_else(preds >= t, 1, 0)) %>%
        arrange(adm_id, CAT_ep, LM_preds) %>%
        group_by(adm_id, CAT_ep) %>%
        mutate(new_alert = if_else(alert == 1 & 
                                     # no alert before OR gap in LM (e.g. patient transferred and comes back)
                                     (lag(alert, default = 0) == 0 | 
                                        lag(LM_preds, default = -1) != LM_preds - 1), 1, 0)) %>%
        ungroup() 
      
      n_new_alerts <- sum(new_alerts$new_alert)
      p_new_alerts <- n_new_alerts/length(unique(new_alerts$adm_id))
      
      metrics[paste0("New_alerts_at_thresh_", t)] <- n_new_alerts
      metrics[paste0("New_alerts_rate_at_thresh_", t)] <- p_new_alerts
    }
    
  }
  
  return(metrics)
  
}

evaluate_mod <- function(predictions, thresholds = NULL, 
                         only_stacked = FALSE, 
                         full_calib = TRUE,
                         eval_per_ward = FALSE
){
  
  # metrics for pooled/stacked predictions
  eval_metrics <- predictions %>%
    group_by(model, test_set) %>%
    summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                           thresholds = thresholds,
                                           full_calib = full_calib, 
                                           adm_id = functioneelDossierNr, # calculates new alerts
                                           CAT_ep = CAT_catheter_episode, 
                                           LM_preds = LM) %>%
                            t() %>% as_tibble())) %>%
    ungroup() %>%
    unnest(eval) %>%
    add_column(type = "all",
               subgroup = NA_character_,
               LM = NA_real_)
  
  results <- eval_metrics
  
  if (!only_stacked){
    # metrics for dynamic eval per LM (on an entire test set / or fold)
    eval_metrics_LM <- predictions %>%
      filter(LM <= max_LM_eval_train) %>%
      group_by(model, test_set, LM) %>%
      summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                             thresholds = thresholds,
                                             full_calib = full_calib) %>%
                              t() %>% as_tibble())) %>%
      ungroup() %>%
      unnest(eval) %>%
      add_column(type = "per_LM",
                 subgroup = NA_character_)
    
    # read LM data and get year
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
    
    # metrics per year 
    eval_metrics_year <- predictions %>% 
      left_join(LM_data_year,
                by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
      filter(LM <= max_LM_eval_train) %>%
      group_by(model, test_set, year) %>%
      summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                             thresholds = thresholds,
                                             full_calib = full_calib) %>%
                              t() %>% as_tibble())) %>%
      ungroup() %>%
      unnest(eval) %>%
      add_column(type = "per_year",
                 subgroup = NA_character_) %>% 
      mutate(year = as.character(year))
    
    # metrics per LM and year 
    eval_metrics_year_LM <- predictions %>% 
      left_join(LM_data_year,
                by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
      filter(LM <= max_LM_eval_train) %>%
      group_by(model, test_set, year, LM) %>%
      summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                             thresholds = thresholds,
                                             full_calib = full_calib) %>%
                              t() %>% as_tibble())) %>%
      ungroup() %>%
      unnest(eval) %>%
      add_column(type = "per_year_LM",
                 subgroup = NA_character_) %>% 
      mutate(year = as.character(year))
    
    results <- results %>% mutate(year = "all") %>% 
      add_row(eval_metrics_LM %>% mutate(year = "all")) %>% 
      add_row(eval_metrics_year) %>% 
      add_row(eval_metrics_year_LM)
    
    if (eval_per_ward){
      # LM data for ward
      LM_data_test_ward <- LM_data_18_20 %>% 
        select(functioneelDossierNr, CAT_catheter_episode, LM, MS_physical_ward) 
      
      LM_data_train_ward <- LM_data_14_17 %>% 
        select(functioneelDossierNr, CAT_catheter_episode, LM, MS_physical_ward) 
      
      LM_data_ward <- LM_data_test_ward %>% 
        add_row(LM_data_train_ward)
      
      # metrics per ward
      eval_metrics_ward <- predictions %>% 
        left_join(LM_data_ward,
                  by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
        filter(LM <= max_LM_eval_train) %>%
        group_by(model, test_set, MS_physical_ward) %>%
        summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                               thresholds = thresholds,
                                               full_calib = full_calib, 
                                               adm_id = functioneelDossierNr, # calculates new alerts
                                               CAT_ep = CAT_catheter_episode, 
                                               LM_preds = LM) %>%
                                t() %>% as_tibble())) %>%
        ungroup() %>%
        unnest(eval) %>%
        add_column(type = "per_ward") %>% 
        rename(subgroup = MS_physical_ward) %>% 
        mutate(year = "all")
      
      # metrics per ward and year
      eval_metrics_ward_year <- predictions %>% 
        left_join(LM_data_ward,
                  by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
        left_join(LM_data_year,
                  by = join_by(LM, functioneelDossierNr, CAT_catheter_episode)) %>% 
        filter(LM <= max_LM_eval_train) %>%
        group_by(model, test_set, MS_physical_ward, year) %>%
        summarise(eval = list(evaluate_metrics(preds, y_true_cat, positive_class = "CLABSI",
                                               thresholds = thresholds,
                                               full_calib = full_calib, 
                                               adm_id = functioneelDossierNr, # calculates new alerts
                                               CAT_ep = CAT_catheter_episode, 
                                               LM_preds = LM) %>%
                                t() %>% as_tibble())) %>%
        ungroup() %>%
        unnest(eval) %>%
        add_column(type = "per_ward_year") %>% 
        rename(subgroup = MS_physical_ward) %>% 
        mutate(year = as.character(year))
      
      results <- results %>% 
        add_row(eval_metrics_ward) %>% 
        add_row(eval_metrics_ward_year) 
      
    }
    
  }
  
  cols_wide <- colnames(results)
  cols_wide <- cols_wide[!cols_wide %in% c("model", "test_set",
                                           "type", "subgroup", "LM", "year")]
  results <- results %>%
    pivot_longer(cols = all_of(cols_wide), names_to = "metric",
                 values_to = "value")
  
  return(results)
  
}

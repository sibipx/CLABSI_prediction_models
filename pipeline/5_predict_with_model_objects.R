# THIS IS AN EXAMPLE SCRIPT FOR MAKING A DUMMY PREDICTION USING THE MODEL OBJECTS (available on request)
# the dummy observation is 0 on all features and UNKNOWN_VALUE when factors are used (RF-ALL)

# load libraries 
library(nnet)
library(ranger)
library(xgboost)
library(tidyverse)

# load function
predictLM_CSC_multi <- function(model_info1, # List object from model_CS_LIM
                                model_info2, # List object from model_multi_LIM
                                predictors_list, # List of predictors
                                newdata, # Data.frame for which to make predictions
                                horizon, # Prediction time horizon (numeric)
                                primary_event) { # Primary event (numeric)
  
  n_causes <- length(model_info1)
  causes_ind <- seq_len(n_causes)
  
  # Calculate linear predictors for all causes in new dataset
  linear_predictors <- vector("list", length(model_info1$model_terms))
  
  for (i in seq_along(model_info1$model_terms)) {
    terms <- model_info1$model_terms[[i]]
    des_matr <- as.data.frame(model.matrix(as.formula(paste0(" ~ ", paste0(predictors_list, collapse = " + "))), newdata))
    des_matr$`(Intercept)` <- NULL
    linear_predictors[[i]] <- as.vector(as.matrix(des_matr) %*% cbind(model_info1$coefficients[[i]]))
  }
  
  preds <- vapply(seq_len(nrow(newdata)), function(id) {
    time_points <- model_info1$baseline_hazards[[primary_event]][["time"]]
    hazards <- vapply(causes_ind, function(cause) {
      cumhaz <- model_info1$baseline_hazards[[cause]][["hazard"]] * exp(linear_predictors[[cause]][[id]])
      diff(c(0, cumhaz))
    }, FUN.VALUE = numeric(length(time_points)))
    
    hazards <- cbind(hazards, time_points)
    lmsi <- newdata$LM[id]
    hazards <- hazards[hazards[,"time_points"] <= lmsi + horizon & hazards[,"time_points"] >= lmsi, ]
    
    # When predicting using predictLM_CSC, problem could occur during LM130-LM230.
    # As the cause-specific hazard (instantaneous risk) of CLABSI right after LM130 given the individual is alive LM130 is 0, as no CLABSI event happened between LM130 and LM230 in our training set.
    # In predictLM_CSC we forced the predict risk into 0, which is unfair to patients during these time periods.
    # Thus we use multinomial logistic model to calculate risk if the patient is between LM130-LM230.
    
    if (length(hazards) == 0) {
      return(0)
    }
    
    if (is.null(dim(hazards))) {
      hazards <- matrix(hazards, nrow = 1)
    }
    
    hazard_sum <- rowSums(hazards[,1:n_causes])
    cumulative_hazard <- cumsum(hazard_sum)
    surv <- exp(-cumulative_hazard)
    cuminc <- cumsum(hazards[, primary_event] * c(1, surv[-length(surv)]))
    cuminc_horizon <- tail(cuminc, n=1)
    
    return(cuminc_horizon)
  }, FUN.VALUE = numeric(1L))
  
  # Now check if preds are 0 and use multinomial logistic model if necessary
  preds <- vapply(seq_len(nrow(newdata)), function(id) {
    if (preds[id]==0) {
      # Use multinomial logistic model if within the problematic range and preds are 0
      pred_multi <- predict(model_info2, newdata = newdata[id,], type = "probs")
      if (is.null(dim(pred_multi))) {
        # If new_obs is a vector (single row), select the second element
        pred_multi <- pred_multi[2]
      } else {
        # If the new_obs is a matrix (multiple rows), select the second column
        pred_multi <- pred_multi[, 2]
      }
      return(pred_multi)
    } else {
      return(preds[id])
    }
  }, FUN.VALUE = numeric(1L))
  
  return(preds)
}

positive_class <- "CLABSI"

# load models
load("models_train/train/coefs_SL_NNLS")
load("models_train/train/model_CS_LIM_MF_7d")
model_CS_LIM <- model_CS
load("models_train/train/model_multi_LIM_MF_7d") 
model_multi_LIM <- model_multi
load("models_train/train/model_RF_LIM_MF_multinom_7d")
model_RF_LIM <- model_res
load("models_train/train/model_RF_VST_MF_multinom_7d")
model_RF_ALL <- model_res
load("models_train/train/model_XGB_LIM_MF_multinom_7d")
model_XGB_LIM <- model_res
load("models_train/train/model_XGB_ALL_MF_multinom_7d")
model_XGB_ALL <- model_res

# predict CS-LIM/multi-LIM
col_list <- attr(attr(model_CS_LIM$model_terms$`Cause 1`, "factors"), "dimnames")[[2]] 
new_obs <- as_tibble(matrix(0, nrow = 1, ncol = length(col_list)), .name_repair = ~ col_list)
pred_CS_multi <- predictLM_CSC_multi(model_CS_LIM, model_multi_LIM, col_list, newdata = new_obs, horizon = 7, primary_event = 1)

# predict RF-ALL
col_list <- model_RF_ALL$forest$independent.variable.names
new_obs <- as_tibble(matrix(0, nrow = 1, ncol = length(col_list)), .name_repair = ~ col_list)
new_obs[,"MS_medical_specialty"] <- "UNKNOWN_VALUE"
new_obs[,"MS_physical_ward_base"] <- "UNKNOWN_VALUE"

pred_RF_ALL <- predict(model_RF_ALL, new_obs)$predictions[,positive_class]

# predict RF-LIM
col_list <- model_RF_LIM$forest$independent.variable.names
new_obs <- as_tibble(matrix(0, nrow = 1, ncol = length(col_list)), .name_repair = ~ col_list)

pred_RF_LIM <- predict(model_RF_LIM, new_obs)$predictions[,positive_class]

# predict XGB-LIM
col_list <- model_XGB_LIM$feature_names
new_obs <- as_tibble(matrix(0, nrow = 1, ncol = length(col_list)), .name_repair = ~ col_list)

pred_XGB_LIM <- predict(model_XGB_LIM, xgb.DMatrix(as.matrix(new_obs)))
pred_XGB_LIM <- matrix(pred_XGB_LIM, ncol = 4, byrow = TRUE)
pred_XGB_LIM <- pred_XGB_LIM[,2]

# predict XGB-ALL
col_list <- model_XGB_ALL$feature_names
new_obs <- as_tibble(matrix(0, nrow = 1, ncol = length(col_list)), .name_repair = ~ col_list)

pred_XGB_ALL <- predict(model_XGB_ALL, xgb.DMatrix(as.matrix(new_obs)))
pred_XGB_ALL <- matrix(pred_XGB_ALL, ncol = 4, byrow = TRUE)
pred_XGB_ALL <- pred_XGB_ALL[,2]

# predict SL
pred_SL <- coefs_nnls[1,"LM_LIM"] * pred_CS_multi +
  coefs_nnls[1,"RF_LIM_MF_multinom_7d"] * pred_RF_LIM +
  coefs_nnls[1,"RF_VST_MF_multinom_7d"] * pred_RF_ALL +
  coefs_nnls[1,"XGB_LIM_MF_multinom_7d"] * pred_XGB_LIM +
  coefs_nnls[1,"XGB_ALL_MF_multinom_7d"] * pred_XGB_ALL 

# print predictions
print(sprintf("CLABSI 7-days prediction CS-LIM: %s", round(pred_CS_multi, 4)))
print(sprintf("CLABSI 7-days prediction RF-LIM: %s", round(pred_RF_LIM, 4)))
print(sprintf("CLABSI 7-days prediction RF-ALL: %s", round(pred_RF_ALL, 4)))
print(sprintf("CLABSI 7-days prediction XGB-LIM: %s", round(pred_XGB_LIM, 4)))
print(sprintf("CLABSI 7-days prediction XGB-ALL: %s", round(pred_XGB_ALL, 4)))
print(sprintf("CLABSI 7-days prediction SL: %s", round(pred_SL, 4)))


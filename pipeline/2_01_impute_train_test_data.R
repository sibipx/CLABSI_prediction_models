
# libraries and functions
# -----------------------

# source all files in the R directory
files_to_source <- list.files("R/", recursive = TRUE, full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

# impute training data
# --------------------

load(paste0(data_path_train_clean, "data_train_clean"))

# summary missing values
map(data_train_clean, ~sum(is.na(.))) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>% 
  mutate(number_present = nrow(data_train_clean) - number_missing,
         percentage_missing = number_missing/dim(data_train_clean)[[1]]) %>% 
  arrange(desc(number_missing)) %>% 
  filter(number_missing > 0) %>% 
  head(20)

set.seed(2024)

MF_errors <- tibble(train_set = character(),
                    iteration = numeric(),
                    variable = character(),  
                    MSE = double(),        
                    NMSE = double(),       
                    MER = double(),       
                    macro_F1 = double(),  
                    F1_score = double())

imp_obj <- fit_impute_dynamic_MF_train(data_train_clean, 
                                       cols_not_missing = cols_not_missing_train,
                                       verbose = TRUE)

data_train_imp <- imp_obj$data

# keep MF errors for later exploration
MF_err <- imp_obj$imp_obj$imp_obj_MF$OOB_err
MF_err$train_set <- "train"
MF_errors <- MF_errors %>% add_row(MF_err)

# impute test data
# ----------------

load(paste0(data_path_test_clean, "data_test_clean"))

data_test_imp <- predict_impute_dynamic_MF_train(imp_obj$imp_obj, data_test_clean,
                                             verbose = FALSE)

# save on disk
# ------------

# remove BMI from train and test (problematic due to recording errors)
data_train_imp$CARE_PHY_BMI <- NULL
data_test_imp$CARE_PHY_BMI <- NULL

save(data_train_imp, 
     file = paste0(data_path_train_clean, "data_train_imp"))

save(data_test_imp, 
     file = paste0(data_path_test_clean, "data_test_imp"))

save(MF_errors, file = paste0(models_path_train, "MF_errors_DYN"))

missFP_imp_obj <- imp_obj$imp_obj
# remove the data from the object
missFP_imp_obj$imp_obj_MF$ximp <- NULL
missFP_imp_obj$imp_obj_MF$init <- NULL
save(missFP_imp_obj, file = paste0(models_path_train, "model_missFP_imp"))

# explore imputation
# ------------------

# load("models_train/MF_errors_DYN")
# MF_errors %>%
#   filter(!is.na(NMSE)) %>%
#   filter(str_detect(variable, "LAB_")) %>%
#   ggplot(aes(iteration, NMSE, group = interaction(variable, train_set), col = variable)) +
#   geom_line(linewidth = 1) +
#   theme_minimal(9) +
#   theme(panel.border = element_rect(colour = "black", fill=NA)) +
#   scale_color_discrete(guide = guide_legend()) +
#   theme(legend.position="bottom",
#         legend.text=element_text(size=6))
# 
# unique(MF_errors$variable)
# 
# # "PAT_"
# # "MS_"
# # "ADM_"
# # "CARE_"
# # "CAT_"
# # "LAB_"
# 
# # check imputed values for LAB_D_dimer_last and LAB_ferritin_last
# 
# load("data_for_models/2014_2017/data_train_imp")
# load("data_for_models/2014_2017/data_train_clean")
# data_train_imp %>% is.na() %>% colSums() %>% sum()
# data_train_clean
# 
# D_dimer_imp <- data_train_imp %>%
#   select(functioneelDossierNr, CAT_catheter_episode, LM, LAB_D_dimer_last) %>%
#   rename(LAB_D_dimer_last_imp = LAB_D_dimer_last) %>%
#   left_join(data_train_clean %>%
#               select(functioneelDossierNr, CAT_catheter_episode, LM, LAB_D_dimer_last),
#             by = join_by(functioneelDossierNr, CAT_catheter_episode, LM))
# 
# D_dimer_imp %>%
#   ggplot(aes(LAB_D_dimer_last)) +
#   geom_histogram()
# 
# D_dimer_imp %>%
#   ggplot(aes(LAB_D_dimer_last_imp)) +
#   geom_histogram()
# 
# # D dimer is not imputing extremely large values but might be skweded towards large values?
# 
# ferritin_imp <- data_train_imp %>%
#   select(functioneelDossierNr, CAT_catheter_episode, LM, LAB_ferritin_last) %>%
#   rename(LAB_ferritin_last_imp = LAB_ferritin_last) %>%
#   left_join(data_train_clean %>%
#               select(functioneelDossierNr, CAT_catheter_episode, LM, LAB_ferritin_last),
#             by = join_by(functioneelDossierNr, CAT_catheter_episode, LM))
# 
# ferritin_imp %>%
#   filter(LAB_ferritin_last < 20000) %>%
#   ggplot(aes(LAB_ferritin_last)) +
#   geom_histogram()
# 
# ferritin_imp %>%
#   filter(LAB_ferritin_last_imp < 20000) %>%
#   ggplot(aes(LAB_ferritin_last_imp)) +
#   geom_histogram()
# 
# # ferritin looks ok
# 
# all_cor <- data_train_imp %>%
#   select(-c(functioneelDossierNr, CAT_catheter_episode, LM, type, eventtime,
#             MS_medical_specialty, MS_physical_ward_base, GEN_LM_month_categ,
#             CARE_SAF_freedom_restriction_categorical_last)) %>%
#   ## convert to correlation matrix
#   as.matrix %>% cor %>%
#   ## set redundant to `NA`
#   `[<-`(lower.tri(., TRUE), NA) %>%
#   ## back to tibble
#   as_tibble(rownames="var1") %>%
#   ## long format, dropping redundant
#   pivot_longer(cols=-1, names_to="var2", values_to="rho", values_drop_na=TRUE) %>%
#   ## descending sort m
#   arrange(-abs(rho))





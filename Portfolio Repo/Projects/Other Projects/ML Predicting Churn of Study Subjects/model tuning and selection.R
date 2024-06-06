# Model tuning and selection

source("loading and processing.R")




# Final preprocessing
  # creating various data structures to try out

set.seed(2023)


# Splitting and folding training data

fu_ave_split <- initial_split(data = df_average_fu, prop = 0.8, strata = follow_up_ave)
fu_ave_train <- training(fu_ave_split)
fu_ave_test <- testing(fu_ave_split)
fu_ave_folds <- vfold_cv(data = fu_ave_train, v = 3, repeats = 10, strata = follow_up_ave)



fu_long_split <- initial_split(data = df_long, prop = 0.8, strata = completed_fu)
fu_long_train <- training(fu_long_split)
fu_long_test <- testing(fu_long_split)
fu_long_folds <- vfold_cv(data = fu_long_train, v = 10, repeats = 5, strata = completed_fu)


fu_emaave_split <- initial_split(data = df_average_ema, prop = 0.8, strata = follow_up_ave)
fu_emaave_train <- training(fu_emaave_split)
fu_emaave_test <- testing(fu_emaave_split)
fu_emaave_folds <- vfold_cv(data = fu_emaave_train, v = 3, repeats = 10, strata = follow_up_ave)



fu_long_emaave_split <- initial_split(data = df_long_average_ema, prop = 0.8, strata = completed_fu)
fu_long_emaave_train <- training(fu_long_emaave_split)
fu_long_emaave_test <- testing(fu_long_emaave_split)
fu_long_emaave_folds <- vfold_cv(data = fu_long_emaave_train, v = 10, repeats = 5, strata = completed_fu)






# Model 1: Regularized regression

#setting up models
fu_ave_glmnet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>% 
  set_engine("glmnet")


fu_long_glmnet_model <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>% 
  set_engine("glmnet")

fu_emaave_glmnet_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>% 
  set_engine("glmnet")

fu_long_emaave_glmnet_model <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>% 
  set_engine("glmnet")


# setting up recipes

fu_ave_recipe <-
  recipe(data = fu_ave_train, formula = follow_up_ave ~ .) %>%
  update_role(pid, new_role = "ignore") %>% 
  step_mutate(across(where(is.character), factor)) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())

fu_ave_recipe %>% prep() %>% bake(NULL) # just to check how data looks after all the manipulations  



fu_long_recipe <-
  recipe(data = fu_long_train, formula = completed_fu ~ .) %>% 
  update_role(pid, new_role = "ignore") %>% 
  update_role(fu_num, new_role = "interaction") %>% 
  step_mutate(across(where(is.character), factor)) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())


fu_long_recipe %>% prep() %>% bake(NULL)





fu_emaave_recipe <-
  recipe(data = fu_emaave_train, formula = follow_up_ave ~ .) %>% 
  update_role(pid, new_role = "ignore") %>% 
  step_mutate(across(where(is.character), factor)) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(ema_ave, impute_with = imp_vars(age, 
                                                     education, 
                                                     cigarettes,
                                                     ecigs,
                                                     alcohol,
                                                     binge,
                                                     marijuana,
                                                     sleeptime,
                                                     sleepweekday,
                                                     sleepweekend)) %>% 
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())

fu_emaave_recipe %>% prep() %>% bake(NULL)






fu_long_emaave_recipe <-
  recipe(data = fu_long_emaave_train, formula = completed_fu ~ .) %>% 
  update_role(pid, new_role = "ignore") %>% 
  update_role(fu_num, new_role = "interaction") %>% 
  step_mutate(across(where(is.character), factor)) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_mutate(ema_ave = ifelse(is.na(ema_ave), 0, ema_ave)) %>% 
  step_normalize(all_numeric_predictors())


fu_long_emaave_recipe %>% prep() %>% bake(NULL)



# Preparing workflow with recipe and model

fu_ave_wflow <- workflow() %>% add_model(fu_ave_glmnet_model) %>% add_recipe(fu_ave_recipe)

fu_long_wflow <- workflow() %>% add_model(fu_long_glmnet_model) %>% add_recipe(fu_long_recipe)

fu_emaave_wflow <- workflow() %>% add_model(fu_emaave_glmnet_model) %>% add_recipe(fu_emaave_recipe)

fu_long_emaave_wflow <- workflow() %>% add_model(fu_long_emaave_glmnet_model) %>% add_recipe(fu_long_emaave_recipe)



# finding boundaries for hyperparameter tuning

fu_ave_glmnet_param <- fu_ave_glmnet_model %>% extract_parameter_set_dials() %>% finalize(fu_ave_folds)
fu_long_glmnet_param <- fu_long_glmnet_model %>% extract_parameter_set_dials() %>% finalize(fu_long_folds)
fu_emaave_glmnet_param <- fu_emaave_glmnet_model %>% extract_parameter_set_dials() %>% finalize(fu_emaave_folds)
fu_long_emaave_glmnet_param <- fu_long_emaave_glmnet_model %>% extract_parameter_set_dials() %>% finalize(fu_long_emaave_folds)

# it seems like the ranges are fairly large and our sample is small, so I'll spend more time on tuning


# hyperparameter tuning

fu_ave_tune <- fu_ave_wflow %>% 
  tune_grid(resamples = fu_ave_folds,
            grid = 25,
            param_info = fu_ave_glmnet_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_tune <- fu_long_wflow %>% 
  tune_grid(resamples = fu_long_folds,
            grid = 15,
            param_info = fu_long_glmnet_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))


fu_emaave_tune <- fu_emaave_wflow %>% 
  tune_grid(resamples = fu_emaave_folds,
            grid = 25,
            param_info = fu_emaave_glmnet_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_emaave_tune <- fu_long_emaave_wflow %>% 
  tune_grid(resamples = fu_long_emaave_folds,
            grid = 15,
            param_info = fu_long_emaave_glmnet_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))



fu_ave_param_final <- select_best(fu_ave_tune, metric = "rmse") 
fu_long_param_final <- select_best(fu_long_tune, metric = "sensitivity")
fu_emaave_param_final <- select_best(fu_emaave_tune, metric = "rmse")
fu_long_emaave_param_final <- select_best(fu_long_emaave_tune, metric = "sensitivity")


# finalize workflows
fu_ave_wflow_final <- fu_ave_wflow %>% finalize_workflow(fu_ave_param_final)
fu_long_wflow_final <- fu_long_wflow %>% finalize_workflow(fu_long_param_final)
fu_emaave_wflow_final <- fu_emaave_wflow %>% finalize_workflow(fu_emaave_param_final)
fu_long_emaave_wflow_final <- fu_long_emaave_wflow %>% finalize_workflow(fu_long_emaave_param_final)




# Fit the final model to the entire training set and test in testing set
fu_ave_final <- fu_ave_wflow_final %>% last_fit(fu_ave_split) # last_fit with the original split
collect_metrics(fu_ave_final)

fu_long_final <- fu_long_wflow_final %>% last_fit(fu_long_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_final)

fu_emaave_final <- fu_emaave_wflow_final %>% last_fit(fu_emaave_split) 
collect_metrics(fu_emaave_final)

fu_long_emaave_final <- fu_long_emaave_wflow_final %>% last_fit(fu_long_emaave_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_emaave_final)



# Graphing models

ggplot(collect_predictions(fu_ave_final), aes(x = follow_up_ave, y = .pred)) +
  geom_point(alpha = 0.33) +
  geom_abline(color = "darkred") +
  coord_obs_pred() +
  labs(x = "Observed average FU", y = "Predicted average FU") # breaking up by week is better



fu_long_cm <- conf_mat(collect_predictions(fu_long_final), truth = completed_fu, estimate = .pred_class)
fu_long_emaave_cm <- conf_mat(collect_predictions(fu_long_emaave_final), truth = completed_fu, estimate = .pred_class)


summary(fu_long_cm)
autoplot(fu_long_cm, type = "mosaic")

fu_long_final %>% extract_fit_parsnip() %>%  vip::vi()# variables impacting prediction most
fu_long_emaave_final %>% extract_fit_parsnip() %>%  vip::vi()



# Plot the ROC curve

fu_long_rc <- roc_curve(collect_predictions(fu_long_final), truth = completed_fu, .pred_0)
autoplot(fu_long_rc)
# final model isn't bad at knowing when someone will complete a FU, 
# but not great at knowing when someone won't (if threshold is .50, should have a 87.5% chance of detecting true negative)
# Can try redoing the sampling or grabbing another fold so we can get a better estimate (ask for sensitivity this time)
# Can also experiment with an mlm machine learning model










# Model 2: random forest

#setting up models
fu_ave_rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "impurity") 


fu_long_rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity") 


fu_emaave_rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "impurity") 


fu_long_emaave_rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "impurity") 


# Preparing workflow with recipe and model

fu_ave_wflow <- workflow() %>% add_model(fu_ave_rf_model) %>% add_recipe(fu_ave_recipe)

fu_long_wflow <- workflow() %>% add_model(fu_long_rf_model) %>% add_recipe(fu_long_recipe)

fu_emaave_wflow <- workflow() %>% add_model(fu_emaave_rf_model) %>% add_recipe(fu_emaave_recipe)

fu_long_emaave_wflow <- workflow() %>% add_model(fu_long_emaave_rf_model) %>% add_recipe(fu_long_emaave_recipe)



# finding boundaries for hyperparameter tuning

fu_ave_rf_param <- fu_ave_rf_model %>% extract_parameter_set_dials() %>% finalize(fu_ave_folds)
fu_long_rf_param <- fu_long_rf_model %>% extract_parameter_set_dials() %>% finalize(fu_long_folds)
fu_emaave_rf_param <- fu_emaave_rf_model %>% extract_parameter_set_dials() %>% finalize(fu_emaave_folds)
fu_long_emaave_rf_param <- fu_long_emaave_rf_model %>% extract_parameter_set_dials() %>% finalize(fu_long_emaave_folds)

# it seems like the ranges are fairly large and our sample is small, so I'll spend more time on tuning


# hyperparameter tuning

fu_ave_tune <- fu_ave_wflow %>% 
  tune_grid(resamples = fu_ave_folds,
            grid = 25,
            param_info = fu_ave_rf_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_tune <- fu_long_wflow %>% 
  tune_grid(resamples = fu_long_folds,
            grid = 15,
            param_info = fu_long_rf_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))


fu_emaave_tune <- fu_emaave_wflow %>% 
  tune_grid(resamples = fu_emaave_folds,
            grid = 25,
            param_info = fu_emaave_rf_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_emaave_tune <- fu_long_emaave_wflow %>% 
  tune_grid(resamples = fu_long_emaave_folds,
            grid = 15,
            param_info = fu_long_emaave_rf_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))



fu_ave_param_final <- select_best(fu_ave_tune, metric = "rmse") 
fu_long_param_final <- select_best(fu_long_tune, metric = "sensitivity")
fu_emaave_param_final <- select_best(fu_emaave_tune, metric = "rmse")
fu_long_emaave_param_final <- select_best(fu_long_emaave_tune, metric = "sensitivity")


# finalize workflows
fu_ave_wflow_final <- fu_ave_wflow %>% finalize_workflow(fu_ave_param_final)
fu_long_wflow_final <- fu_long_wflow %>% finalize_workflow(fu_long_param_final)
fu_emaave_wflow_final <- fu_emaave_wflow %>% finalize_workflow(fu_emaave_param_final)
fu_long_emaave_wflow_final <- fu_long_emaave_wflow %>% finalize_workflow(fu_long_emaave_param_final)




# Fit the final model to the entire training set and test in testing set
fu_ave_rf_final <- fu_ave_wflow_final %>% last_fit(fu_ave_split) # last_fit with the original split
collect_metrics(fu_ave_rf_final)

fu_long_rf_final <- fu_long_wflow_final %>% last_fit(fu_long_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_rf_final)

fu_emaave_rf_final <- fu_emaave_wflow_final %>% last_fit(fu_emaave_split) 
collect_metrics(fu_emaave_rf_final)

fu_long_emaave_rf_final <- fu_long_emaave_wflow_final %>% last_fit(fu_long_emaave_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_emaave_rf_final)



# Graphing models

ggplot(collect_predictions(fu_ave_rf_final), aes(x = follow_up_ave, y = .pred)) +
  geom_point(alpha = 0.33) +
  geom_abline(color = "darkred") +
  coord_obs_pred() +
  labs(x = "Observed average FU", y = "Predicted average FU") # breaking up by week is better



fu_long_rf_cm <- conf_mat(collect_predictions(fu_long_rf_final), truth = completed_fu, estimate = .pred_class)
fu_long_emaave_rf_cm <- conf_mat(collect_predictions(fu_long_emaave_rf_final), truth = completed_fu, estimate = .pred_class)


summary(fu_long_rf_cm)
autoplot(fu_long_rf_cm, type = "mosaic")


# Plot the ROC curve

fu_long_rf_rc <- roc_curve(collect_predictions(fu_long_final), truth = completed_fu, .pred_0)
autoplot(fu_long_rf_rc)


# variable importance


fu_long_emaave_rf_final %>% extract_fit_parsnip() %>%  vip::vi() %>% print(n = 33)
fu_long_rf_final %>% extract_fit_parsnip() %>%  vip::vi() %>% print(n = 37)



# Model 3: SVM

#setting up models
fu_ave_svm_model <- svm_rbf(cost = tune(), margin = tune()) %>% 
  set_mode("regression") %>%
  set_engine("kernlab")


fu_long_svm_model <- svm_rbf(cost = tune(), margin = tune()) %>% 
  set_mode("classification") %>%
  set_engine("kernlab")

fu_emaave_svm_model <- svm_rbf(cost = tune(), margin = tune()) %>% 
  set_mode("regression") %>%
  set_engine("kernlab")

fu_long_emaave_svm_model <- svm_rbf(cost = tune(), margin = tune()) %>% 
  set_mode("classification") %>%
  set_engine("kernlab")

# Preparing workflow with recipe and model

fu_ave_wflow <- workflow() %>% add_model(fu_ave_svm_model) %>% add_recipe(fu_ave_recipe)

fu_long_wflow <- workflow() %>% add_model(fu_long_svm_model) %>% add_recipe(fu_long_recipe)

fu_emaave_wflow <- workflow() %>% add_model(fu_emaave_svm_model) %>% add_recipe(fu_emaave_recipe)

fu_long_emaave_wflow <- workflow() %>% add_model(fu_long_emaave_svm_model) %>% add_recipe(fu_long_emaave_recipe)



# finding boundaries for hyperparameter tuning

fu_ave_svm_param <- fu_ave_svm_model %>% extract_parameter_set_dials() %>% finalize(fu_ave_folds)
fu_long_svm_param <- fu_long_svm_model %>% extract_parameter_set_dials() %>% finalize(fu_long_folds)
fu_emaave_svm_param <- fu_emaave_svm_model %>% extract_parameter_set_dials() %>% finalize(fu_emaave_folds)
fu_long_emaave_svm_param <- fu_long_emaave_svm_model %>% extract_parameter_set_dials() %>% finalize(fu_long_emaave_folds)

# it seems like the ranges are fairly large and our sample is small, so I'll spend more time on tuning


# hyperparameter tuning

fu_ave_tune <- fu_ave_wflow %>% 
  tune_grid(resamples = fu_ave_folds,
            grid = 25,
            param_info = fu_ave_svm_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_tune <- fu_long_wflow %>% 
  tune_grid(resamples = fu_long_folds,
            grid = 15,
            param_info = fu_long_svm_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))


fu_emaave_tune <- fu_emaave_wflow %>% 
  tune_grid(resamples = fu_emaave_folds,
            grid = 25,
            param_info = fu_emaave_svm_param,
            metrics = yardstick::metric_set(rsq, rmse),
            control = control_resamples(save_pred = TRUE))


fu_long_emaave_tune <- fu_long_emaave_wflow %>% 
  tune_grid(resamples = fu_long_emaave_folds,
            grid = 15,
            param_info = fu_long_emaave_svm_param,
            metrics = yardstick::metric_set(specificity, roc_auc, sensitivity),
            control = control_resamples(save_pred = TRUE))



fu_ave_param_final <- select_best(fu_ave_tune, metric = "rmse") 
fu_long_param_final <- select_best(fu_long_tune, metric = "sensitivity")
fu_emaave_param_final <- select_best(fu_emaave_tune, metric = "rmse")
fu_long_emaave_param_final <- select_best(fu_long_emaave_tune, metric = "sensitivity")


# finalize workflows
fu_ave_wflow_svm_final <- fu_ave_wflow %>% finalize_workflow(fu_ave_param_final)
fu_long_wflow_svm_final <- fu_long_wflow %>% finalize_workflow(fu_long_param_final)
fu_emaave_wflow_svm_final <- fu_emaave_wflow %>% finalize_workflow(fu_emaave_param_final)
fu_long_emaave_wflow_svm_final <- fu_long_emaave_wflow %>% finalize_workflow(fu_long_emaave_param_final)




# Fit the final model to the entire training set and test in testing set
fu_ave_svm_final <- fu_ave_wflow_final %>% last_fit(fu_ave_split) 
collect_metrics(fu_ave_svm_final)

fu_long_svm_final <- fu_long_wflow_final %>% last_fit(fu_long_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_svm_final)

fu_emaave_svm_final <- fu_emaave_wflow_final %>% last_fit(fu_emaave_split) 
collect_metrics(fu_emaave_svm_final)

fu_long_emaave_svm_final <- fu_long_emaave_wflow_final %>% last_fit(fu_long_emaave_split, metrics = yardstick::metric_set(specificity, roc_auc, sensitivity))
collect_metrics(fu_long_emaave_svm_final)



# Graphing models

ggplot(collect_predictions(fu_ave_svm_final), aes(x = follow_up_ave, y = .pred)) +
  geom_point(alpha = 0.33) +
  geom_abline(color = "darkred") +
  coord_obs_pred() +
  labs(x = "Observed average FU", y = "Predicted average FU") # breaking up by week is better



fu_long_svm_cm <- conf_mat(collect_predictions(fu_long_svm_final), truth = completed_fu, estimate = .pred_class)
fu_long_emaave_svm_cm <- conf_mat(collect_predictions(fu_long_emaave_svm_final), truth = completed_fu, estimate = .pred_class)


summary(fu_long_svm_cm)
autoplot(fu_long_svm_cm, type = "mosaic")

# Plot the ROC curve

fu_long_svm_rc <- roc_curve(collect_predictions(fu_long_svm_final), truth = completed_fu, .pred_0)
autoplot(fu_long_svm_rc)


# testing out metrics at different thresholds
test <- collect_predictions(fu_long_svm_final) %>% mutate(.pred_new = factor(if_else(.pred_1 > .9, 1,0)))
test_cm <- conf_mat(test, truth = completed_fu, .pred_new)
summary(test_cm)
autoplot(test_cm, type = "mosaic")
# it looks like we can get up to 78% if we set the threshold really high 
# since our model is really good at telling FU successes





# MLM model comparisons
# since data can be nested and we can use data to predict the same sample in the future, we can split within person


# dfs

df_long
df_long_average_ema

# Splitting and folding
fu_long_mlm_split <- group_initial_split(data = df_long, group = pid)
fu_long_mlm_train <- training(fu_long_mlm_split)
fu_long_mlm_test <- testing(fu_long_mlm_split)
fu_long_mlm_folds <- group_vfold_cv(data = fu_long_mlm_train, group = pid, v = 5, repeats = 5)


fu_long_emaave_mlm_split <- group_initial_split(data = df_long_average_ema, group = pid)
fu_long_emaave_mlm_train <- training(fu_long_emaave_mlm_split)
fu_long_emaave_mlm_test <- testing(fu_long_emaave_mlm_split)
fu_long_emaave_mlm_folds <- group_vfold_cv(data = fu_long_emaave_mlm_train, group = pid, v = 5, repeats = 5)



# feature extraction: 
# still need to get variance measures for some variables and 
# can get unique info on FU occasion, like season, weekday, year, or month sent the survey



# probably going to grab the variables that have the largest importance 
#  fu_long_emaave_rf_final %>% extract_fit_parsnip() %>%  vip::vi() %>% print(n = 33)

# Recipes

fu_long_recipe <-
  recipe(x = fu_long_mlm_train) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())


fu_long_recipe %>% prep() %>% bake(NULL)






fu_long_emaave_recipe <-
  recipe(x = fu_long_emaave_mlm_train) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())


fu_long_emaave_test_recipe <-
  recipe(x = fu_long_emaave_mlm_test) %>% 
  step_impute_median(healthy_eating, exercise) %>% #imputing the median here because only missing one obs from each
  step_impute_linear(contains("wk"), impute_with = imp_vars(age, 
                                                            education, 
                                                            cigarettes,
                                                            ecigs,
                                                            alcohol,
                                                            binge,
                                                            marijuana,
                                                            sleeptime,
                                                            sleepweekday,
                                                            sleepweekend)) %>% 
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors()) %>% #wk variables are highly correlated
  step_normalize(all_numeric_predictors())




fu_long_emaave_mlm_train <- fu_long_emaave_recipe %>% prep() %>% bake(NULL)
fu_long_emaave_mlm_test <- fu_long_emaave_test_recipe %>% prep() %>% bake(NULL)




#setting up models



fu_long_mlm_model <- logistic_reg() %>% 
  set_engine("glmer")


fu_long_emaave_mlm_model <- logistic_reg() %>% 
  set_engine("glmer")


# Preparing workflow with recipe and model


fu_long_wflow <- workflow() %>% 
  add_variables(outcomes = completed_fu, predictors = c()) %>% 
  add_model(fu_long_mlm_model) %>% 
  add_recipe(fu_long_recipe)



fu_long_emaave_wflow <-  workflow() %>%
  add_variables(outcomes = completed_fu, predictors = c(ema_ave,
                                                        screener_EMA_delay,
                                                        healthy_eating,
                                                        exercise,
                                                        marijuana,
                                                        sleep_diff,
                                                        sleepweekday,
                                                        start_month,
                                                        sleepweekend,
                                                        sleeptime,
                                                        ecigs,
                                                        alcohol,
                                                        age,
                                                        binge,
                                                        pid)) %>%  
  add_model(fu_long_emaave_mlm_model, 
            formula = completed_fu ~
              ema_ave +
              screener_EMA_delay+
              healthy_eating+
              exercise+
              marijuana+
              sleep_diff+
              sleepweekday+
              start_month+
              sleepweekend+
              sleeptime+
              ecigs+
              alcohol+
              age +
              binge + 
              (ema_ave|pid))



# Don't need to tune



# Fit the final model to the entire training set and test in testing set

fu_long_emaave_mlm_final <- fu_long_emaave_wflow %>% fit(fu_long_emaave_mlm_test)
mlm_predictions <- predict(fu_long_emaave_mlm_final, fu_long_emaave_mlm_test) %>% bind_cols(fu_long_emaave_mlm_test)



fu_long_emaave_mlm_cm <- conf_mat(mlm_predictions, truth = completed_fu, estimate = .pred_class)


summary(fu_long_emaave_mlm_cm)
autoplot(fu_long_emaave_mlm_cm, type = "mosaic") # not great at detecting incompletes

# Maybe we can use PCA to still have all the predictors, but reduce the complexity of the model for model convergence
# Should also experiment with nested SVM
# Should also look into regularized multilevel models













# Final model for deployment

fu_final_fit <- fu_long_wflow_svm_final %>% fit(df_long)
saveRDS(fu_final_fit, "fu_machine_learning_algorithm.rds")



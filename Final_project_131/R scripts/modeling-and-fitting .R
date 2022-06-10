
train=read.csv("~/Desktop/Final_project_131/Data/processed/train.csv")

test=read.csv("~/Desktop/Final_project_131/Data/processed/test.csv")

K_folds=read.csv("~/Desktop/Final_project_131/Data/processed/test.csv")


## Recipe 
diagnosis_recipe <- recipe(diagnosis ~texture_mean+radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean, data = dt) %>%
  step_normalize(all_predictors())


## logestic model 

log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

auto_wkflow <- workflow() %>% 
  add_recipe(diagnosis_recipe) %>% 
  add_model(log_reg) %>% set_args(penalty = tune(),mixture = tune())



log_fit=fit_resamples(log_wkflow,K_folds)

log_fit_train <- fit(log_wkflow, train)


predict(log_fit_train, new_data = train, type = "class") %>% 
  bind_cols(train) %>% 
  accuracy(truth = diagnosis, estimate = .pred_class)

augment(log_fit_train, new_data = train) %>%
  conf_mat(truth = diagnosis, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")


log_fit_test <- fit(log_wkflow, test)


predict(log_fit_test, new_data = test, type = "class") %>% 
  bind_cols(test) %>% 
  accuracy(truth = diagnosis, estimate = .pred_class)

augment(log_fit_test, new_data = test) %>%
  conf_mat(truth = diagnosis, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

## DT model 


tree_spec <- decision_tree() %>%
  set_engine("rpart")

class_tree_spec <- tree_spec %>%
  set_mode("classification")




auto_wrkflw=workflow()%>%
  add_recipe((diagnosis_recipe)) %>%
  add_model(class_tree_spec %>% set_args(cost_complexity = tune())) 

param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)

tune_res2 <- tune_grid(
  auto_wrkflw, 
  resamples = K_folds, 
  grid = param_grid,
  metrics = metric_set(roc_auc,accuracy)
)

autoplot(tune_res2)

collect_metrics(tune_res2)

metrics_2=collect_metrics(tune_res2)
arrange(metrics_2,mean)
best2=select_best(tune_res2)

## RF model 



rf_spec <- rand_forest(mtry = tune(),trees=tune(),min_n=tune())%>%
  set_engine("ranger", importance = 'impurity') %>%
  set_mode("classification")



auto_wrkflw3=workflow()%>%
  add_recipe((diagnosis_recipe)) %>%
  add_model(rf_spec)

grid_pen=grid_regular(mtry(range = c(1,6)),trees(range = c(1,10)),min_n(range = c(1,10)),
                      levels =6)


tune_res3=tune_grid(
  object = auto_wrkflw3,
  resamples = K_folds,
  grid = grid_pen,
  metrics = metric_set(roc_auc,accuracy)
)



autoplot(tune_res3)

metrics_3=collect_metrics(tune_res3)
arrange(metrics_3,mean)
best3=select_best(tune_res3)

## Bosted model 


boost_spec <- boost_tree(trees = tune() )%>%
  set_engine("xgboost") %>%
  set_mode("classification")



auto_wrkflw4=workflow()%>%
  add_recipe((diagnosis_recipe)) %>%
  add_model(boost_spec)

grid_trees=grid_regular(trees(range = c(10,2000)),levels =10)


tune_res4=tune_grid(
  object = auto_wrkflw4,
  resamples = K_folds,
  grid = grid_trees,
  metrics = metric_set(roc_auc,accuracy)
)


autoplot(tune_res4)

metrics_4=collect_metrics(tune_res4)
arrange(metrics_4,mean)
best4=select_best(tune_res4)

Boost_final=finalize_workflow(auto_wrkflw4,best4)

DT_fit=fit(Boost_final,train)

predict(DT_fit, new_data = train, type = "class") %>% 
  bind_cols(train) %>% 
  accuracy(truth = diagnosis, estimate = .pred_class)




library(gridExtra)
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
library(dplyr)
library(MASS)
library(janitor)

## Data cleaning 


dt=read.csv("Desktop/pstat131_final/data.csv") %>%
                                      mutate(diagnosis = factor(diagnosis, levels = c("M", "B")))
view(dt)
str(dt)

dt <- dt %>% 
  clean_names()



## Data splitting
set.seed(69)
split <- initial_split(dt, prop = 0.80, strata = diagnosis)
train <- training(split)
test <- testing(split)

K_folds <- vfold_cv(train, strata = diagnosis, 
                          v = 10)

## EDA
train_nu=select_if(train,is.numeric)

## train correlation 

train_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

## correlation on factor of diagnosis 

M_nu=train_nu[train[2]=='M',]

B_nu=train_nu[train[2]=='B',]

M_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

B_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

## see relationship look for reduce demnsion 

M_c=M_nu%>%
  cor() 

B_c=B_nu%>%
  cor() 



corr_M <- as.data.frame(as.table(M_c))
#corr=corr[lower.tri(corr,diag=TRUE)] <- NA 
corr_M=corr_M[!duplicated(corr_M$Freq), ]

corr_B<- as.data.frame(as.table(B_c))
#corr=corr[lower.tri(corr,diag=TRUE)] <- NA 
corr_B=corr_B[!duplicated(corr_B$Freq), ]


sig_m=corr_M %>%
   filter(Freq>.7 )

sig_b=corr_B %>%
  filter(Freq>.7 )


sig_M2 = sig_m%>% group_by(Var2) %>% summarise(n=n())
sig_B2 = sig_b%>% group_by(Var2) %>% summarise(n=n())



view(sig_M2)
view(sig_B2)

sig_M2 %>%
  filter(n>1)

sig_B2 %>%
  filter(n>1)


sig_m %>% filter(Freq>.98)

sig_b %>% filter(Freq>.98)

ggplot(train, aes(x =area_mean , y = radius_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_mean , y = perimeter_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =concave.points_mean , y = concavity_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_worst , y = radius_worst, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_worst , y = texture_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)



c1=corr_M %>% filter(Freq<.20)

c2=corr_B %>% filter(Freq<.20)

sig_M2 = c1%>% group_by(Var2) %>% summarise(n=n())
sig_B2 = c2%>% group_by(Var2) %>% summarise(n=n())

## texture mean ,radius mean ,perimeter mean , area mean ,compactness mean, concavity mean
 

train=subset(train,select=c(diagnosis,texture_mean,radius_mean,perimeter_mean,area_mean,compactness_mean,concavity_mean))



train %>% 
  ggplot(aes(x = radius_mean,colour=factor(diagnosis))) +
  geom_histogram()
train %>% 
  ggplot(aes(x = texture_mean,colour=factor(diagnosis))) +
  geom_histogram()

train %>% 
  ggplot(aes(x = area_mean,colour=factor(diagnosis))) +
  geom_histogram()

train %>% 
  ggplot(aes(x = compactness_mean,colour=factor(diagnosis))) +
  geom_histogram()













dt=subset(dt,select=c(diagnosis,texture_mean,radius_mean,perimeter_mean,area_mean,compactness_mean,concavity_mean))



diagnosis_recipe <- recipe(diagnosis ~texture_mean+radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean, data = dt) %>%
  step_normalize(all_predictors())
  
log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

auto_wkflow <- workflow() %>% 
  add_recipe(diagnosis_recipe) %>% 
  add_model(log_reg) %>% set_args(penalty = tune(),mixture = tune())

log_grid <- grid_regular(penalty(range = c(-10, 0)),mixture(range=c(0,1)) ,levels = 10)

log_grid


log_fit=fit_resamples(log_wkflow,K_folds)

collect_metrics(log_fit)

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




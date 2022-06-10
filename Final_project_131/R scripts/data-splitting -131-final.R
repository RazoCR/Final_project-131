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


dt=read.csv("~/Desktop/Final_project_131/Data/unprocessed/data.csv") %>%
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

write.csv(train,"~/Desktop/Final_project_131/Data/processed/train.csv", row.names = FALSE)

write.csv(test,"~/Desktop/Final_project_131/Data/processed/test.csv", row.names = FALSE)


write.csv(K_folds,"~/Desktop/Final_project_131/Data/processed/K_folds.csv", row.names = FALSE)




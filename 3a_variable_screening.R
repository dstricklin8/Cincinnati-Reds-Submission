library(tidyverse)
library(tidymodels)
library(tictoc)
library(ggrepel)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
doMC::registerDoMC(cores = 8)

# Load Data
load("data/model_files.rda")

pitch_split <- initial_split(train_test_df, prop = 0.8, strata = pitch_group)

pitch_train <- training(pitch_split)
pitch_test <- testing(pitch_split)

# Recipe
pitch_recipe <- recipe(num_pitches ~ ., data = pitch_train) %>%
  update_role(batter_id, player_name, total_pitches_pitch, prop, new_role = "id") %>%
  step_corr(all_numeric_predictors())  %>%  
  step_impute_knn(all_predictors()) %>%
  step_zv(all_numeric_predictors())  %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

pitch_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>%
  head(10) %>%
  view()

# Random Forest Variable Reduction ----
rf_model <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

## Workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(pitch_recipe)

## Fitting ----
rf_fit <- fit(rf_workflow, data = pitch_train)

#### Estimated Parameters
parameter_graph <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip(scale = T)

parameter_graph

parameter_table <- rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vi(scale = T) %>% 
  as_tibble()

parameter_table

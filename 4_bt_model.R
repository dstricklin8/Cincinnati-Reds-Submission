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

pitch_folds <- vfold_cv(pitch_train, v = 5, repeats = 5, strata = pitch_group)

# Recipe
pitch_recipe <- recipe(num_pitches ~ ., data = pitch_train) %>%
  update_role(batter_id, player_name, total_pitches_pitch, prop, new_role = "id") %>%
  step_corr(all_numeric_predictors())  %>%  
  step_zv(all_numeric_predictors())  %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# pitch_recipe %>% 
#   prep() %>% 
#   bake(new_data = NULL) %>% 
#   head(10) %>% 
#   view()

# XGBoost Model
bt_model <- boost_tree(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost")

bt_params <- extract_parameter_set_dials(bt_model) %>% 
  update(min_n = min_n(range = c(1, 100)),
         trees = trees(range = c(1, 100)),
         mtry = mtry(range = c(1, 50))
  )

bt_grid <- grid_regular(bt_params, levels = 10)

# Workflow
bt_wflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(pitch_recipe)

## Tuning ----
# Tune grid 
# clear and start timer
tic.clearlog()
tic("XGBoost Tuning")

bt_tune <- bt_wflow %>% 
  tune_grid(
    metrics = metric_set(rmse, rsq, mae),  
    resamples = pitch_folds,
    grid = bt_grid,
    control = control_grid(save_pred = TRUE,
                           save_workflow = TRUE,
                           parallel_over = "everything"))

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

var_screen_toc <- tibble(model = time_log[[1]]$msg,
                         runtime = time_log[[1]]$toc - time_log[[1]]$tic)

beepr::beep(2)

# Evaluate Results
autoplot(bt_tune, metric = "rmse")
show_best(bt_tune, metric = "rmse")[1,] # 57.2, 0.719

autoplot(bt_tune, metric = "rsq")
show_best(bt_tune, metric = "rsq")[1,] # 0.981, 0.000509

autoplot(bt_tune, metric = "mae")
show_best(bt_tune, metric = "mae")[1,] # 41.1, 0.458

bt_workflow_final <- bt_wflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rsq"))

# Fit Final Workflow
bt_fit <- fit(bt_workflow_final, data = pitch_train)

save(bt_tune, file = "data/bt_tune.rda")
save(bt_fit, file = "data/bt_fit.rda")

# Create Preds
preds <- bt_fit %>% predict(pitch_test) %>% 
  bind_cols(pitch_test) %>% 
  select(batter_id, player_name, pitch_group, num_pitches, .pred)

metrics <- preds %>%
  metrics(truth = .pred, estimate = num_pitches)
metrics

preds %>% 
  ggplot(aes(.pred, num_pitches, color = pitch_group)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(mapping = aes(label = player_name), max.overlaps = 5, max.iter = 2) +
  theme_minimal()


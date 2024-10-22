library(tidyverse)
library(tidymodels)
library(tictoc)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
parallel::detectCores()
doMC::registerDoMC(cores = 8)

# Load Data
load("data/model_files.rda")
load("data/bt_fit.rda")

# Create 2024 Predictions
preds <- bt_fit %>% 
  predict(predict_df) %>% 
  bind_cols(predict_df) %>% 
  group_by(batter_id, player_name) %>% 
  mutate(
    .pred = if_else(.pred < 0, 0, .pred)
  ) %>% 
  mutate(
    xprop = .pred / sum(.pred)
  )

final_pred_df <- preds %>% 
  select(batter_id, player_name, game_year, pitch_group, xprop) %>% 
  rename(
    BATTER_ID = batter_id,
    PLAYER_NAME = player_name,
    GAME_YEAR = game_year
  ) %>% 
  mutate(
    GAME_YEAR = 2024
  ) %>% 
  pivot_wider(values_from = xprop, names_from = pitch_group) %>% 
  rename(
    PITCH_TYPE_FB = FB,
    PITCH_TYPE_BB = BB,
    PITCH_TYPE_OS = OS
  ) %>% 
  select(BATTER_ID, PLAYER_NAME, GAME_YEAR, PITCH_TYPE_FB, PITCH_TYPE_BB, PITCH_TYPE_OS) %>% 
  arrange(BATTER_ID)

# og_df <- read_csv("predictions.csv")

# unique_in_df2 <- og_df %>%
#   anti_join(final_pred_df, by = "PLAYER_NAME") %>%
#   select(PLAYER_NAME) %>%
#   distinct()


write_csv(final_pred_df, file = "predictions.csv")





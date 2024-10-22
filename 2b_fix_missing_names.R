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

load("data/full_df.rda")

missing_names <- full_df %>% 
  filter(game_year == 2023)

all_names <- full_df %>% 
  filter(game_year != 2023)

missing_hitters <- all_names %>%
  anti_join(missing_names, by = "player_name") %>%
  select(player_name) %>%
  distinct() %>% 
  pull(player_name)

model_df <- full_df %>% 
  mutate(
    pred_year = case_when(
      player_name %in% missing_hitters & game_year == 2022 ~ 2024,
      TRUE ~ pred_year
    )
  )

save(model_df, file = "data/model_df.rda")


library(tidyverse)
library(tidymodels)
library(gt)
library(gtUtils)
library(gtExtras)
library(shiny)
library(shinythemes)
library(bslib)
# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(6432)

# set up parallel processing
# parallel::detectCores()
doMC::registerDoMC(cores = 8)

# Load Data
load("data/model_files.rda")
load("data/full_df.rda")
load("data/data.rda")
load("data/bt_fit.rda")

player_info <- data %>% 
  mutate(
    batting_team = if_else(inning_topbot == "Top", away_team, home_team)
  ) %>% 
  select(batter_id, player_name, game_year, batting_team) %>% 
  distinct()

pitch_counts <- data %>% 
  group_by(batter_id, player_name, game_year, pitch_group) %>% 
  summarise(
    num_pitches = n()
  ) %>% 
  ungroup() %>% 
  group_by(batter_id, player_name, game_year) %>% 
  mutate(
    prop = num_pitches / sum(num_pitches)
  ) %>% 
  ungroup()

preds <- bt_fit %>% 
  predict(predict_df) %>% 
  bind_cols(predict_df) %>% 
  group_by(batter_id, player_name) %>% 
  mutate(
    num_pitches = if_else(.pred < 0, 0, .pred),
    game_year = 2024
  ) %>% 
  mutate(
    prop = num_pitches / sum(num_pitches)
  ) %>% 
  select(batter_id, player_name, game_year, pitch_group, num_pitches, prop)

df <- pitch_counts %>% 
  bind_rows(preds) %>% 
  arrange(batter_id) %>% 
  mutate(
    type = if_else(game_year != 2024, "True", "Pred")
  )

# Pitch Summary ----

pitch_summary <- data %>% 
  group_by(batter_id, player_name, pitch_group, game_year) %>% 
  summarise(
    total_pitches = n(),
    swing_prop = mean(is_swing, na.rm = T),
    chase_prop = mean(is_chase_swing, na.rm = T),
    zone_swing_prop = mean(is_zone_swing, na.rm = T),
    whiff_prop = mean(is_whiff, na.rm = T),
    putaway_prop = mean(is_putaway, na.rm = T),
    woba = mean(woba_value[woba_denom == 1], na.rm = T),
    xwoba = mean(estimated_woba_using_speedangle[woba_denom == 1], na.rm = T),
    exit_velo = mean(launch_speed, na.rm = T),
    exit_angle = mean(launch_angle, na.rm = T),
    sweetspot_prop = mean(is_sweetspot, na.rm = T),
    hardhit_prop = mean(is_hardhit, na.rm = T),
    barrel_prop = mean(is_barrel, na.rm = T),
    
    sz_top = mean(sz_top, na.rm = T),
    sz_bot = mean(sz_bot, na.rm = T),
    
    win_prob_added = sum(delta_bat_win_exp, na.rm = T),
    
    ks = sum(is_strikeout, na.rm = T),
    walks = sum(is_walk, na.rm = T),
    singles = sum(is_single, na.rm = T),
    doubles = sum(is_double, na.rm = T),
    triples = sum(is_triple, na.rm = T),
    homeruns = sum(is_home_run, na.rm = T)
  ) %>% 
  mutate(
    total_bases = (singles + walks) * 1 + doubles * 2 + triples * 3 + homeruns * 4,
    pred_year = game_year + 1
  ) %>% 
  ungroup() %>% 
  group_by(batter_id, player_name, game_year) %>% 
  mutate(
    prop = total_pitches / sum(total_pitches)
  ) %>% 
  ungroup() %>% 
  arrange(game_year, total_pitches)

# table_df ----
table_df <- pitch_summary %>% 
  group_by(batter_id, player_name, game_year) %>% 
  mutate(num_pitches = sum(total_pitches)) %>% 
  select(batter_id, player_name, game_year, pitch_group, num_pitches, prop) %>% 
  bind_rows(preds %>% mutate(num_pitches = sum(num_pitches))) %>% 
  pivot_wider(names_from = pitch_group, values_from = prop) %>% 
  mutate(
    OS = if_else(is.na(OS), 0, OS),
    FB = if_else(is.na(FB), 0, FB),
    BB = if_else(is.na(BB), 0, BB)
  ) %>% 
  arrange(batter_id) %>% 
  ungroup()

# Pitch Colors ----
pitch_group_colors <- c(
  "BB" = "#04d1ed",
  "OS" = "#1fbe3a",
  "FB" = "#d22d4a"
)

pitch_group_tibble <- tibble(
  pitch_group = c("FB", "OS", "BB", "All"),
  
  pitch_hex = c("#d22d4a", "#1fbe3a", "#04d1ed", "grey20")
)

# Pitch Overall Summary ----
pitch_summary_overall <- data %>% 
  group_by(batter_id, player_name, pitch_group) %>% 
  summarise(
    total_pitches = n(),
    swing_prop = 100 * mean(is_swing, na.rm = T),
    chase_prop = 100 * mean(is_chase_swing, na.rm = T),
    zone_swing_prop = 100 * mean(is_zone_swing, na.rm = T),
    whiff_prop = 100 * mean(is_whiff, na.rm = T),
    putaway_prop = 100 * mean(is_putaway, na.rm = T),
    woba = mean(woba_value[woba_denom == 1], na.rm = T),
    xwoba = mean(estimated_woba_using_speedangle[woba_denom == 1], na.rm = T),
    sweetspot_prop = 100 * mean(is_sweetspot, na.rm = T),
    hardhit_prop = 100 * mean(is_hardhit, na.rm = T),
    barrel_prop = 100 * mean(is_barrel, na.rm = T),
    
    exit_velo = mean(launch_speed[is_bip == 1], na.rm = T),
    exit_angle = mean(launch_angle[is_bip == 1], na.rm = T),
    
    sz_top = mean(sz_top, na.rm = T),
    sz_bot = mean(sz_bot, na.rm = T),
    
    win_prob_added = sum(delta_bat_win_exp, na.rm = T),
    
    ks = sum(is_strikeout, na.rm = T),
    walks = sum(is_walk, na.rm = T),
    singles = sum(is_single, na.rm = T),
    doubles = sum(is_double, na.rm = T),
    triples = sum(is_triple, na.rm = T),
    homeruns = sum(is_home_run, na.rm = T)
  ) %>% 
  mutate(
    total_bases = (singles + walks) * 1 + doubles * 2 + triples * 3 + homeruns * 4
  ) %>% 
  ungroup() %>% 
  group_by(batter_id, player_name) %>% 
  mutate(
    prop = total_pitches / sum(total_pitches)
  ) %>% 
  ungroup() %>% 
  arrange(total_pitches)


save(player_info, df, pitch_group_colors, table_df, pitch_summary_overall, pitch_group_tibble,
     file = "app_files/files.rda")

library(tidyverse)
library(tidymodels)

load("data/df_clean.rda")

set.seed(6432)

unique(df_clean$events)

# New Variables ----
data <- df_clean %>% 
  mutate(
    is_bip = if_else(type == "X", 1, 0),
    
    is_swing = case_when(
      description %in% c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip") ~ 1,
      TRUE ~ 0
    ),
    is_contact = case_when(
      description %in% c("foul", "hit_into_play") ~ 1,
      TRUE ~ 0
    ),
    is_whiff = case_when(
      is_swing == 1 & is_contact == 0 ~ 1,
      is_swing == 1 & is_contact == 1 ~ 0,
      is_swing == 0 ~ NA,
      TRUE ~ NA
    ),
    is_zone_swing = case_when(
      zone %in% c(1:9) & is_swing == 1 ~ 1,
      zone %in% c(1:9) & is_swing == 0 ~ 0,
      TRUE ~ NA
    ),
    is_chase_swing = case_when(
      zone %in% c(11:14) & is_swing == 1 ~ 1,
      zone %in% c(11:14) & is_swing == 0 ~ 0,
      TRUE ~ NA
    ),
    is_putaway = case_when(
      strikes == 2 & events %in% c("strikeout", "strikeout_double_play") ~ 1,
      strikes == 2 & !events %in% c("strikeout", "strikeout_double_play") ~ 0,
      TRUE ~ NA
    ),
    is_sweetspot = case_when(
      is_bip == 1 & launch_angle >= 8 & launch_angle <= 32 ~ 1,
      is_bip == 1 & launch_angle < 8 | launch_angle > 32 ~ 0,
      is_swing != 1 ~ NA,
      TRUE ~ NA
    ),
    is_hardhit = case_when(
      is_bip == 1 & launch_speed >= 95 ~ 1,
      is_bip == 1 & !launch_speed >= 95 ~ 0,
      is_swing != 1 ~ NA,
      TRUE ~ NA
    ),
    is_barrel = case_when(
      is_bip == 1 & is_sweetspot == 1 & is_hardhit == 1 ~ 1,
      is_bip == 1 & is_sweetspot == 0 | is_hardhit == 0 ~ 0,
      is_swing != 1 ~ NA,
      TRUE ~ NA
    ),
    
    is_single = if_else(events == "single", 1, 0),
    
    is_double = if_else(events == "double", 1, 0),
    
    is_triple = if_else(events == "triple", 1, 0),
    
    is_home_run = if_else(events == "home_run", 1, 0),
    
    is_walk = case_when(
      events %in% c("walk", "hit_by_pitch") ~ 1,
      TRUE ~ 0
    ),
    
    is_strikeout = if_else(events %in% c("strikeout", "strikeout_double_play"), 1, 0),
    
    delta_bat_win_exp = case_when(
      inning_topbot == "Top" ~ -1 * delta_home_win_exp,
      inning_topbot == "Bot" ~ 1 * delta_home_win_exp,
      TRUE ~ NA
    )
  )

save(data, file = "data/data.rda")


# Yearly Summary
yearly_summary <- data %>% 
  group_by(batter_id, player_name, game_year) %>% 
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
  )

# Pitch Summary
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
  ungroup()

# Pitch Counts
pitch_counts <- data %>% 
  group_by(batter_id, player_name, game_year, pitch_group) %>% 
  summarise(
    num_pitches = n()
  )


# Full DF

player_summary <- left_join(yearly_summary, pitch_summary, 
                            by = c("batter_id", "player_name", "game_year", "pred_year"),
                            suffix = c("_previous", "_pitch")) %>% 
  select(batter_id, player_name, game_year, pred_year, everything()) %>% 
  arrange(player_name)

pitch_df <- left_join(player_summary, pitch_counts)

pred_df <- read_csv("predictions.csv") %>% 
  janitor::clean_names() %>% 
  select(-c(pitch_type_fb, pitch_type_bb, pitch_type_os)) %>% 
  mutate(pred_year = 2024, game_year = 2023)

full_df <- left_join(pitch_df, pred_df) %>% 
  ungroup()

save(full_df, file = "data/full_df.rda")

full_df %>% 
  ungroup() %>% 
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete") %>% 
  corrplot::corrplot(method = "circle")

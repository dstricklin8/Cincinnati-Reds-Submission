library(tidyverse)
library(tidymodels)

df <- read_csv("data.csv")

unique(df$PITCH_NAME)

# Fastball: 4 Seam, 2 Seam, Cutter, Sinker
# Offspeed: Split, Change, Fork, Screw
# Breaking: Slider, Curve, Knuckle, Sweeper, Slurve, Other

df_clean <- df %>% 
  mutate(
    pitch_group = case_when(
      PITCH_NAME %in% c("4-Seam Fastball",  "Four-Seam", "FourSeamFastBall",
                             "TwoSeamFastBall", "Cutter", "Sinker") ~ "FB",
      PITCH_NAME %in% c("Changeup", "ChangeUp", "Split-Finger",
                             "Forkball", "Screwball") ~ "OS",
      PITCH_NAME %in% c("Slider", "Curveball", "Knuckle Curve", "Sweeper",
                        "Slurve", "Slow Curve", "Other", "Knuckleball", "Eephus") ~ "BB",
      TRUE ~ "Other"
    )
  ) %>% 
  janitor::clean_names() %>% 
  filter(pitch_group != "Other")

save(df_clean, file = "data/df_clean.rda")


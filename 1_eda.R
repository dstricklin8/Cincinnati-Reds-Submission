library(tidyverse)
library(tidymodels)

load("data/df_clean.rda")

df_clean %>% 
  ggplot(aes(home_team, fill = home_team)) +
  geom_bar() +
  facet_wrap(~pitch_group)

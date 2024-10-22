library(tidyverse)
library(tidymodels)

set.seed(6432)

load("data/model_df.rda")

train_test_df <- model_df %>% 
  filter(pred_year != 2024)

predict_df <- model_df %>% 
  filter(pred_year == 2024)

save(train_test_df, predict_df, file = "data/model_files.rda")

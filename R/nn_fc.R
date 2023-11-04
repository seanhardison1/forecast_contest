library(rEDM)
library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)
library(fpp3)
library(fable.prophet)
library(feasts)

source(here::here("R/process.R"))

# fit neural network model
nn_fit <- weath_df %>% 
  model(
    nn2 = NNETAR(temp_air, n_nodes = 20),
  )

#model accuracy
nn_fit %>% accuracy()

# simulate from model and visualize
nn_fit %>% 
  generate(times = 200, h = 48) |>
  autoplot(.sim) +
  autolayer(weath_df %>% 
              filter(datetime > as.Date("2023-11-01")) %>% 
              as_tsibble(index = hr), temp_air) +
  guides(color = "none")

# compare summary stats with smap projections
# load(here::here("data/temp_pred_smap.rdata"))
# 
# temp_pred_smap %>% 
#   dplyr::summarise(max_temp_smap = max(temp_pred_smap),
#                    min_temp_smap = min(temp_pred_smap))

nn_preds <- nn_fit |>
  forecast(h = 48) 

nn_preds %>%   
  autoplot()

nn_pred_out <- 
  nn_preds %>% 
  as.data.frame() %>% 
  # mutate(temp_air_nn = .mean * 9/5 + 32) %>% 
  dplyr::select(hr, temp_air_nn = .mean) %>%
  ungroup() %>% 
  left_join(.,connect_df %>% 
              dplyr::select(hr, datetime))
# 
# all_preds <- temp_pred_smap %>% 
#   left_join(.,nn_pred_out)
#   
# ggplot(all_preds) +
#     geom_line(aes(y = temp_pred_smap, 
#                   x = datetime))+
#     geom_line(aes(y = temp_air_nn, x = datetime),
#               color = "red")

nn_pred_out %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  dplyr::summarise(max_nn = max(temp_air_nn),
                   min_nn = min(temp_air_nn))

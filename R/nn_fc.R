library(rEDM)
library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)
library(fpp3)
library(fable.prophet)
library(feasts)

source(here::here("R/process.R"))

train <- weath_df# %>% 
  # filter(year(datetime) == 2022)

train_df <- train %>% as.data.frame()

test <- weath_df %>% 
  filter(year(datetime) == 2023) %>% 
  slice(1:48)

# fit neural network model
nn_fit <- train %>%
  model(
    # nn2 = NNETAR(temp_air, n_nodes = 10),
    # nn3 = NNETAR(temp_air, n_nodes = 20),
    # nn4 = NNETAR(temp_air, n_nodes = 10, n_networks = 25),
    nn5 = NNETAR(temp_air, n_nodes = 20, n_networks = 25),
  )

#model accuracy
nn_fit %>% accuracy()

fc <- nn_fit %>% forecast(h = 51)

max_time <- train %>% pull(hr) %>% length

sm <- SMap( dataFrame = train_df,    # input data
                lib     = paste(1, max_time - 51), # portion of data to train
                pred    = paste(max_time - 51, max_time), # portion of data to predict
                columns = "temp_air",
                target  = "temp_air",
                Tp = 51,
                E = 9)

sm_proj <- sm$predictions %>% 
  filter(!is.na(Predictions)) %>% 
  mutate(hr = seq(max_time,
                  max_time + 51, by = 1)) %>% 
  # filter(datetime > as.Date("2023-11-01")) %>% 
  as_tsibble(index = hr) 

train %>% 
  filter(datetime > as.Date("2023-11-01")) %>% 
  autoplot() +
  autolayer(fc, .mean) +
  autolayer(sm_proj, Predictions)
  

nn_pred_out <- 
  fc %>% 
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

sm_proj %>% 
  as.data.frame() %>% 
  mutate(date = day(ymd_hms(datetime))) %>% 
  group_by(date) %>% 
  dplyr::summarise(max_smap = max(Predictions),
                   min_smap = min(Predictions))

library(rEDM)
library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)


source(here::here("R/process.R"))


ggplot(weath_df) +
  geom_line(aes(y = temp_air, x = hr)) +
  geom_point(aes(y = temp_air, x = hr))


max_time <- weath_df %>% filter(!is.na(temp_air)) %>% pull(hr) %>% max
max_proj_time <- weath_df %>% filter(is.na(temp_air)) %>% pull(hr) %>% max

# Simplex

E.opt = EmbedDimension( dataFrame = weath_df,    # input data
                        lib     = paste(1, max_time - 48), # portion of data to train
                        pred    = paste(max_time - 47, max_time), # portion of data to predict
                        columns = "temp_air",
                        target  = "temp_air")

E <- E.opt %>% filter(max(rho) == rho) %>% pull(E)

simplex_check = Simplex( dataFrame = weath_df,    # input data
                   lib     = paste(1, max_time - 48), # portion of data to train
                   pred    = paste(max_time - 47, max_time), # portion of data to predict
                   columns = "temp_air",
                   target  = "temp_air",
                   # Tp = 48,
                   E = E)

# rho_theta <- PredictNonlinear(dataFrame = weath_df, 
#                               lib     = paste(1, max_time - 48), # portion of data to train
#                               pred    = paste(max_time - 47, max_time), # portion of data to predict
#                               target = "temp_air", 
#                               columns = "temp_air", E = E)

sm_check = SMap( dataFrame = weath_df,    # input data
                 lib     = paste(1, max_time - 48), # portion of data to train
                 pred    = paste(max_time - 47, max_time), # portion of data to predict
           columns = "temp_air",
           target  = "temp_air",
           # Tp = 48,
           E = 9)

ComputeError(simplex$Observations, simplex$Predictions)
ComputeError(sm_check$predictions$Observations, sm_check$predictions$Predictions)

# projections
simplex = Simplex( dataFrame = weath_df,    # input data
                   lib     = paste(1, max_time - 49), # portion of data to train
                   pred    = paste(max_time - 48, max_time), # portion of data to predict
                   columns = "temp_air",
                   target  = "temp_air",
                   Tp = 48,
                   E = E)


sm = SMap( dataFrame = weath_df,    # input data
                   lib     = paste(1, max_time - 49), # portion of data to train
                   pred    = paste(max_time - 48, max_time), # portion of data to predict
                   columns = "temp_air",
                   target  = "temp_air",
                   Tp = 48,
                   E = 9)

plot( weath_df$hr[(max_time - 100):max_proj_time], 
      weath_df$temp_air[(max_time - 100):max_proj_time], 
      type = "l", lwd = 2,
      xlab = "year", ylab = "air temp")
lines( simplex$hr, simplex$Predictions, col = "red", lwd = 2) 
lines( sm$predictions$hr, sm$predictions$Predictions, col = "blue", lwd = 2)


temp_pred_smap <- 
  connect_df %>% 
  filter(is.na(temp_air)) %>% 
  dplyr::select(datetime, hr) %>% 
  left_join(.,sm$predictions) %>% 
  rename(temp_pred_smap = Predictions) %>% 
  mutate(temp_pred_smap = temp_pred_smap * 9/5 + 32) %>% 
  dplyr::select(-Observations)

save(temp_pred_smap, file = here::here("data/temp_pred_smap.rdata"))

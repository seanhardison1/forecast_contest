library(rEDM)
library(tidyverse)

test <- weath_df %>% 
  filter(year(datetime) %in% c(2022, 2023))# %>%
  # slice(1:300)

test_df <- test %>% 
  as.data.frame() %>% 
  mutate(hr = 1:nrow(.))

h <- 6
n <- nrow(test_df)
lib_len <- n - 200

ts <- c(test_df$temp_air[1:(n - h)], rep(NA, h))
lib <- c(1, lib_len)
pred <- c((lib_len + 1), n)

rEDM::EmbedDimension(dataFrame = test_df,
                     lib = paste(lib), 
                     pred = paste(pred), 
                     target = "temp_air", 
                     Tp = 6,
                     columns = "temp_air")

rEDM::PredictNonlinear(dataFrame = test_df,
                       lib = paste(lib), 
                       pred = paste(pred), 
                       target = "temp_air", 
                       Tp = 6,
                       columns = "temp_air",
                       E = 3)

sm <- s_map(ts,
            lib = lib,
            pred = pred,
            E = 3,
            theta = 0,
              # c(0, 
              #         8),
            tp = h,
            stats_only = F)

df = data.frame(yr = as.numeric(time(sunspot.year)), 
                sunspot_count = as.numeric(sunspot.year)) %>% 
  mutate(Index = 1:nrow(.))

h <- 12
n <- nrow(df)
lib_len <- n - 100

ts <- c(df$sunspot_count[1:(n - h)], rep(NA, h))
lib <- c(1, lib_len)
pred <- c((lib_len + 1), n)

sm = simplex(ts, 
                   lib     = lib, # portion of data to train
                   pred    = pred, # portion of data to predict
                   E       = 10,
             tp = h,
             stats_only = F)

proj_out <- NULL
simpl <- T
for (i in 1:length(sm$model_output)){
  nm <- names(sm$model_output)[i]
  
  if (simpl){
    proj <- sm$model_output[[i]] %>% 
      filter((is.na(Observations) & !is.na(Predictions))) %>%
      mutate(mod = nm)
  } else {
    proj <- sm$model_output[[i]] %>% 
      # filter(is.na(Observations)) %>%
      mutate(mod = nm)
  }
  

  
  assign("proj_out", rbind(proj, proj_out))
  
}


ggplot(test_df %>%  filter(hr > lib_len)) +
  geom_line(aes(y = temp_air, x = hr)) +
  geom_point(data = proj_out, aes(y = Const_Predictions,
                                  x = Index, 
                                  color = mod)) +
  theme(axis.title.y = element_blank())

ggplot(df %>% filter(Index > 250)) +
  geom_line(aes(y = sunspot_count, x = Index)) +
  geom_point(data = proj_out, aes(y = Const_Predictions,
                                  x = Index, 
                                  color = mod)) +
  theme(axis.title.y = element_blank())

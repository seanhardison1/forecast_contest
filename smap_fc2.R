library(rEDM)
library(tidyverse)

test <- weath_df %>% 
  filter(year(datetime) == 2023) %>% 
  slice(1:300)

test_df <- test %>% 
  as.data.frame() %>% 
  mutate(hr = 1:nrow(.))

h <- 6

ts <- c(test_df$temp_air[1:294], rep(NA, 6))
lib <- c(1, 250)
pred <- c(251, 300)

sm <- s_map(ts,
            lib = lib,
            pred = pred,
            E = 9,
            tp = 6,
            stats_only = F)

proj_out <- NULL
for (i in 1:length(sm$model_output)){
  nm <- names(sm$model_output)[i]
  
  proj <- sm$model_output[[i]] %>% 
    filter(is.na(Observations)) %>% 
    mutate(mod = nm)
  
  assign("proj_out", rbind(proj, proj_out))
  
}

ggplot(test_df %>%  filter(hr > 250)) +
  geom_line(aes(y = temp_air, x = hr)) +
  geom_line(data = proj_out, aes(y = Predictions,
                                 x = Index, 
                                 color = mod))


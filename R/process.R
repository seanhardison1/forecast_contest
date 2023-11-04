library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)
library(magrittr)

source(here::here("R/get_data.R"))

weath <- tibble()
for (i in list.files(here::here("data/queries/"))){
  n <- read_csv(here::here("data/queries/",i))
  assign("weath", rbind(weath, n))
}

weath_df2 <- weath %>%
  filter(as.Date(datetime) < as.Date("2023-11-20")) %>% 
  distinct() %>% 
  dplyr::select(datetime, temp_air, precip = precip_1hr) %>% 
  mutate(precip = ifelse(is.na(precip), 0, precip),
         datetime = ceiling_date(datetime,
                                 unit = "hours"))

# write_csv(weath_df, file = here::here("R/processed_data.csv"))

# 
weath_df <- read_csv(here::here("data/KCHO 2020-01-01 to 2023-11-03.csv")) %>% 
  arrange(datetime) %>% 
  filter(datetime < min(weath_df2$datetime)) %>% 
  dplyr::rename(temp_air = temp) %>% 
  dplyr::select(datetime, temp_air, precip) %>% 
  mutate(temp_air = temp_air * 9/5 + 32) %>% 
  bind_rows(weath_df2) %>% 
  distinct() %>% 
  group_by(datetime) %>% 
  dplyr::summarise(temp_air = mean(temp_air),
                   precip = mean(precip)) %>% 
  mutate(hr = 1:nrow(.)) %>%
  add_row(hr = (max(.$hr) + 1):(max(.$hr) + 48),
          datetime = 
            seq(max(.$datetime, na.rm = T) + hours(1),
                (max(.$datetime, na.rm = T) + hours(48)),
                by = "hour")) %>%  
  {. ->> connect_df} %>%
  filter(!is.na(temp_air)) %>% 
  as_tsibble(index = hr) #%>% 
  # fill_gaps()

save(weath_df, file = here::here("data/current_weath_ts.rdata"))

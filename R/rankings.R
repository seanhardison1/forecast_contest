library(rEDM)
library(tidyverse)
library(fable)
library(tsibble)
library(stringr)
library(lubridate)

weath <- read.csv(here::here("data/weather_kg.csv")) %>% 
  filter(forecast_id != "forecast 00") %>% 
  mutate(fc_index = 1:nrow(.)) %>% 
  dplyr::rename(day1_hi_actual = day1_hi,
                day2_hi_actual = day2_hi,
                day1_lo_actual = day1_lo,
                day2_lo_actual = day2_lo,
                day1_prec_actual_cat = day1_prec,
                day2_prec_actual_cat = day2_prec) %>% 
  dplyr::select(-day1_prec_actual, 
                -day2_prec_actual)

roster <- read.csv(here::here("data/roster.csv")) %>% 
  dplyr::select(name = Student,
                user_id = 3) %>% 
  filter(name != "Student, Test")

fcs_df1 <- read.csv(here::here("data/forecast_kg.csv")) %>% 
  mutate(user_id = str_remove(user_id, " "))

users <- unique(fcs_df1$user_id)

int <- tibble()
for (i in 1:length(users)){
# for (i in 1:3){
  fcs_user_df <- fcs_df1 %>% 
    filter(user_id == #"deh2rt")
             users[i])
  
  
  for (j in 1:nrow(weath)){
    weath_row_df <- weath %>% slice(j)
    
    fcs_user_proj <- 
      fcs_user_df %>% 
      filter(between(forecast_date,
                     weath_row_df$forecast_start,
                     weath_row_df$forecast_end)) %>% 
      mutate(fc_index = j) %>% 
      slice_tail(n = 1)
    
    if (nrow(fcs_user_proj) == 0) {
      fcs_user_proj <- tibble(id = NA,
                              user_id = users[i],
                              forecast_date = NA,
                              day1_hi = NA,
                              day1_lo = NA,
                              day1_prec = NA,
                              day2_hi = NA,
                              day2_lo = NA,
                              day2_prec = NA,
                              fc_index = j)
    }
    
    assign("int", rbind(int, fcs_user_proj))
  }
  # assign("out", rbind(out, int))
}

scores_out <- int %>% 
  dplyr::select(-id) %>% 
  filter(fc_index != 8) %>%
  # filter(user_id %in% c("deh2rt")) %>%
  arrange(user_id, fc_index) %>% 
  left_join(.,weath %>% 
              dplyr::select(-id),
            by = "fc_index") %>% 
  group_by(user_id) %>% 
  rowwise() %>% 
  mutate(day1_hi_diff = abs(day1_hi - day1_hi_actual),
         day2_hi_diff = abs(day2_hi - day2_hi_actual),
         day1_lo_diff = abs(day1_lo - day1_lo_actual),
         day2_lo_diff = abs(day2_lo - day2_lo_actual),
         day1_prec_diff = abs(day1_prec - day1_prec_actual_cat) * 5,
         day2_prec_diff = abs(day2_prec - day2_prec_actual_cat) * 5) %>% 
  dplyr::select(user_id, fc_index, day1_hi_diff :day2_prec_diff) %>% 
  group_by(user_id) %>% 
  gather(var, value,-user_id, -fc_index) %>% 
  group_by(user_id, fc_index) %>% 
  dplyr::summarise(score = sum(value, na.rm = T),
                   missing = sum(is.na(value))/6) %>% 
  mutate(score = ifelse(score == 0 & missing == 1, NA, score)) %>% 
  group_by(user_id) %>% 
  dplyr::summarise(avg_score = mean(score, na.rm = T),
                   n_missing = sum(missing)) %>%
  arrange(n_missing, avg_score) %>% 
  left_join(.,roster, by = "user_id") %>% 
  mutate(name = ifelse(user_id == "nws999", "National Weather Service", 
                       ifelse(user_id == "red3u", "Prof. Davis",
                              ifelse(user_id == "sh5rs", "Sean", name))),
         avg_score = round(avg_score, 3))  


write.csv(scores_out, file = here::here("data/scores_out.csv"), row.names = F)

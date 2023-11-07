library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(tsibble)

url <- "https://w1.weather.gov/obhistory/KCHO.html"


raw <- read_html(url) %>% 
  html_table()

# 1. Next day max temp
# 2. Next day min temp
# 3. Next day's Precip category

# Category 0: no precipitation–trace 
# Category 1: trace–0.05 inch 
# Category 2: 0.06 inch–0.24 inch 
# Category 3: 0.25 inch–0.49 inch 
# Category 4: 0.50 inch–0.99 inch 
# Category 5: ≥1.00 inch 

weather <-
  raw[[4]] %>% 
    dplyr::select(date = 1,
                  time = 2,
                  wind = 3,
                  temp_air = 7,
                  temp_dewpoint = 8,
                  precip_1hr = 16,
                  precip_3hr = 17) %>% 
  filter(date != "Date") %>% 
  mutate(month = ifelse(date %in% c("29", "30", "31"), 10, 11),
         date = as.numeric(date),
         date = paste(month, date, 2023, sep = "-"),
         datetime = mdy_hm(paste(date, time)),
         temp_air = as.numeric(temp_air))

fn <- paste(Sys.Date(),Sys.time(), "weather.csv", sep = "_")

write_csv(weather, file = paste0("/users/seanhardison/documents/git/forecast_contest/data/queries/",fn))



library(tidyverse)
library(nycflights13)

flights %>% 
  filter(month <= 03 & distance > 2500)

flights %>% 
  arrange(day, desc(dep_delay))

flights %>% 
  select(carrier, tail_number = tailnum)

flights %>% 
  mutate(id = 1:n(), 
         dist_km = distance * 1.60934) %>% 
  select(id, distance, dist_km)

flights %>% 
  filter(!is.na(arr_delay))
  summarise(
    min_delay = min(arr_delay),
    avg_delay = mean(arr_delay),
    max_delay = max(arr_delay)
  )

flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  mutate(delta_arr_delay = arr_delay - mean(arr_delay)) %>% 
  select(carrier, delta_arr_delay)

flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  summarise(
    mean_delay = mean(arr_delay),
    median_delay = median(arr_delay)
  )

library(haven)

ISSP <- read_dta("written/data/ISSP.dta")

Germany_raw <- ISSP %>% 
  filter(V3==276.1 | V3==276.2)
  
Germany <- Germany_raw %>% 
  select(sex, age, DE_RINC) %>% 
  mutate(DE_RINC = as.numeric(DE_RINC)) %>% 
  mutate(sex = factor(sex, levels=c(1,2), labels=c("m", "f"))) %>% 
  mutate(highInc = ifelse(DE_RINC > 5000, 1, 0)) 

Germany %>% 
  group_by(sex) %>% 
  summarise(percHighInc=mean(highInc, na.rm=T)) %>% 
  arrange(percHighInc)
  

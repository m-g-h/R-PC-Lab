## TIDYVERSE INTRODUCTION

library(tidyverse)
library(nycflights13)

# 1.
flights %>% 
  filter(month <= 03 & distance > 2500)

# 2.
flights %>% 
  arrange(day, desc(dep_delay))

# 3.
flights %>% 
  select(carrier, tail_number = tailnum)

# 4.
flights %>% 
  mutate(id = 1:n(), 
         dist_km = distance * 1.60934) %>% 
  select(id, distance, dist_km)

# 5.
flights %>% 
  filter(!is.na(arr_delay))
  summarise(
    min_delay = min(arr_delay),
    avg_delay = mean(arr_delay),
    max_delay = max(arr_delay)
  )

# 6.
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  mutate(delta_arr_delay = arr_delay - mean(arr_delay)) %>% 
  select(carrier, delta_arr_delay)

# 7.
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  summarise(
    mean_delay = mean(arr_delay),
    median_delay = median(arr_delay)
  )

## DATA MANIPULATION EXERCISES

# 1.
library(haven)
ISSP <- read_dta("written/data/ISSP.dta")

# 2.
Germany_raw <- ISSP %>% 
  filter(V3==276.1 | V3==276.2)

# 3a-e  
Germany <- Germany_raw %>% 
  select(sex, age, DE_RINC) %>% 
  mutate(DE_RINC = as.numeric(DE_RINC)) %>% 
  mutate(sex = factor(sex, levels=c(1,2), labels=c("m", "f"))) %>% 
  mutate(highInc = ifelse(DE_RINC > 5000, 1, 0)) 

# 4.
Germany %>% 
  group_by(sex) %>% 
  summarise(percHighInc=mean(highInc, na.rm=T)) %>% 
  arrange(percHighInc)

# 5a
Australia_raw <- ISSP %>% 
  filter(V3==36)
AUSGER <- bind_rows(Australia_raw, Germany_raw)

# 5b
Germany <- bind_cols(Germany, Germany_raw["degree"])

# 5c
randomData <- data_frame(ID=1:nrow(Germany), 
                         rand=sample(1:1000, nrow(Germany), replace=T))
Germany <- Germany %>% 
  mutate(ID=1:n())

Germany <- left_join(Germany, randomData, by="ID")

library(tidyverse)
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
  
  
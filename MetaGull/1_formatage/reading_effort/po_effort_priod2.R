

setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)

load(file = "1_formatage/real_dataset/period2/CH_p2_v1.Rda")



reading_effort = CH %>% 
  as_tibble() %>% 
  pivot_longer(everything()) %>% 
  mutate(value = as.factor(value)) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(name = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  arrange(name) %>% 
  pivot_wider(names_from = name, values_from = n) %>% 
  arrange(., value)

reading_effort = reading_effort %>%
  ungroup() %>% 
  slice(c(12:20)) %>% 
  select(-1) %>% 
  as.matrix()

save(reading_effort, file = "reading_effort_period2.Rda")

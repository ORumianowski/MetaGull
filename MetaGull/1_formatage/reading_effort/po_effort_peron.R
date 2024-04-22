

setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)

load(file = "1_formatage/real_dataset/peron_v1/CH_peron.Rda")

CH = CH_peron

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
  slice(c(7:10)) %>% 
  select(-1) %>% 
  as.matrix()

save(reading_effort, file = "reading_effort_peron.Rda")

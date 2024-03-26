

setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull/1_formatage/comparaison_CH_real_vs_simulation")

library(tidyverse)

load(file = "CH.Rda")

n.colony = 5
n.years = ncol(CH)

colnames(CH) = 1:ncol(CH)


reading_effort = CH %>%
  as_tibble() %>% 
  pivot_longer(everything()) %>% 
  mutate(value = as.factor(value)) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(name = as.integer(name)) %>% 
  arrange(name) %>% 
  pivot_wider(names_from = name, values_from = n) %>% 
  arrange(., value)

reading_effort = reading_effort %>%
  ungroup() %>% 
  slice(c(6:9)) %>% 
  select(-1) %>% 
  as.matrix()

save(reading_effort, file = "reading_effort.Rda")


detection = ifelse(is.na(reading_effort), 0, 1) 

detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
colnames(detection_index) = c("colony", "year")
nrow_detection_index = nrow(detection_index)

all_col_year = cbind(rep(1:n.colony, each = (n.years-1)), rep(1:(n.years-1), time = n.colony)) %>% as.data.frame()
colnames(all_col_year) = c("colony","year")

no_detection_index = anti_join(all_col_year, detection_index, by = c("colony","year"))
nrow_no_detection_index = nrow(no_detection_index)



setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(IPMbook)
library(tidyverse)

load(file = "1_formatage/formatage_cmr/v1/CH.Rda")

n.colony = 10

y = CH # Capture Histories
#y = rbind(y, c(rep(0, time = ncol(y)-1), n.colony)) # one virtual nestling ringed in colony 5 at last occasion
marr = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 (adults in AliveElsewhere) and 11 to 15 (prebreeders)
# save(marr, file = "1_formatage/real_dataset/v4/marr_AE.Rda")
# save(marr, file = "marr_period2_2.Rda")


rel = rowSums(marr) %>% 
  as.vector() %>% 
  matrix(., ncol = 15, nrow = 19, byrow = T)

table(CH)

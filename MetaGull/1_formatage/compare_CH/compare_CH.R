
library(tidyverse)

load(file = "CH.Rda")
load(file = "CH_simul.Rda")

str(CH)
str(y)

table(CH)
table(y)

table(CH) %>% sum()
table(y) %>% sum()

# pas le même nombre d'année et baguage d'adulte en réel, pas dans les simulations


library(IPMbook)
n.colony = 5

y = rbind(y, c(rep(0, time = ncol(y)-1), 5)) # one virtual nestling ringed in colony 5 at last occasion
marr_simul = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 and 11 to 15
rel1 = rowSums(marr_simul) %>% 
  as.vector() %>% 
  matrix(., ncol = 15, nrow = 20, byrow = T)

n.colony = 5
n.years = ncol(CH)
  
CH = rbind(CH, c(rep(0, time = ncol(CH)-1), 5)) # one virtual nestling ringed in colony 5 at last occasion
marr_reel = marray(CH, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 and 11 to 15
rel2 = rowSums(marr_reel) %>% 
  as.vector() %>% 
  matrix(., ncol = 15, nrow = 19, byrow = T)


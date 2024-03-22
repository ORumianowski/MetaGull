
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


# Detection

detection = ifelse(rel2[,(n.colony+1):(2*n.colony)] == 0, 0, 1) 

detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
colnames(detection_index) = c("year", "colony")
nrow_detection_index = nrow(detection_index)

all_col_year = cbind(rep(1:(n.years-1), time = n.colony),rep(1:n.colony, each = (n.years-1))) %>% as.data.frame()
colnames(all_col_year) = c("year", "colony")

no_detection_index = anti_join(all_col_year, detection_index, by = c("year", "colony"))
nrow_no_detection_index = nrow(no_detection_index)



#  Define re-encounter probabilities
for (t in 1:(n.years-1)){
# Nestlings & Prebreeders
  for (state in c(nest.states, prebr.states)){
    po[state,t] <- 0
  }
}
# Breeders
# Surveyed colonies
for (k in 1:nrow_detection_index){ 
  po[detection_index[k,2]+n.colony,detection_index[k,1]] ~ dunif(0, 1)
}
# Non-Surveyed colonies
for (k in 1:nrow_no_detection_index){ 
  po[no_detection_index[k,2]+n.colony,no_detection_index[k,1]] ~ dunif(0, 1)
}



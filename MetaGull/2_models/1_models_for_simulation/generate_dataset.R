
library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------


source("simulation/simul_cmr_v6.R")
source("simulation/simul_survey_v6.R")


survey_data = B # Survey dataset - Number of breeders reported

n.colony = 5
y = rbind(y, c(rep(0, time = ncol(y)-1), n.colony)) # one virtual nestling ringed in colony 5 at last occasion
marr = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 (adults in AliveElsewhere) and 11 to 15 (prebreeders)

save(B, file = "simulated_dataset/B.Rda")
save(marr, file = "simulated_dataset/marr.Rda")



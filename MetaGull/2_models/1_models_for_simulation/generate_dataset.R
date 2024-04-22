
library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------


source("simulation/simul_cmr_v6.R")
source("simulation/simul_survey_v6.R")

n.colony = 5

survey_data = B # Survey dataset - Number of breeders reported
marr = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 (adults in AliveElsewhere) and 11 to 15 (prebreeders)

save(B, file = "B_simulation.Rda")
save(marr, file = "marr_simulation.Rda")



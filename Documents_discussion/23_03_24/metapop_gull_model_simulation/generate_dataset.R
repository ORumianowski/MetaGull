

# version 


library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------


source("simulation/simul_cmr_v6.R")
source("simulation/simul_survey_v6.R")


survey_data = B # Survey dataset - Number of breeders reported
marr = marray(y, unobs=n.colony) # convert capture history to marray

save(B, file = "B.Rda")
save(marr, file = "marr.Rda")

save(y, file = "y.Rda")



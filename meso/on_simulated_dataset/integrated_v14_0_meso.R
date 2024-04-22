#setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

#library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------

load(file = "/lustre/rumianowskio/simulated_dataset/B_simulation.Rda")
load(file = "/lustre/rumianowskio/simulated_dataset/marr_simulation.Rda")

#load(file = "2_models/1_models_for_simulation/simulated_dataset/B_simulation.Rda")
#load(file = "2_models/1_models_for_simulation/simulated_dataset/marr_simulation.Rda")


# Bundle data  ------------------------------------------------------------

n.age.class = 3 # number of age classes in this model
survey_data = B # Survey dataset - Number of breeders reported
rel = rowSums(marr) # number of released birds at each occasions

n.colony = nrow(survey_data) # number of colonies in this survey
n.years = ncol(survey_data)  # number of years in this survey

ns=n.colony*n.age.class # number of state in this model 

nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 


# We determine the numbers of prebreeders and breeders for the priors, the first year

repro_ratio = c(0.34/0.66, rep(0.14/0.86,n.colony-1))  

pop_init = data.frame(estim = B[,1]) %>% 
  mutate(B_inf = estim*0.7)%>% 
  mutate(B_sup = estim*1.3+1)%>% 
  mutate(N_inf = estim*(repro_ratio)*0.7)%>% 
  mutate(N_sup = estim*(repro_ratio)*1.3+1) %>% 
  round() %>% 
  as.matrix()

# Extinction
# Where and when are no reproduction ?
# PRESENCE contains the combination of colonies and date when reproduction occurred.
# ABSENCE contains the combinations  when reproduction not occurred - 0 pairs

# Extincted
E = ifelse(survey_data == 0, 0, 1) %>%
  replace(is.na(.), 1)

marr_index_with_cmr = 1:((n.years-1)*ns)


# Built list of data and constant for jags
jags.data <- list(C=survey_data,
                  marr = marr, rel=rel,
                  
                  ns=ns,
                  zero=matrix(0, ncol=ns, nrow=ns),
                  ones=diag(ns),
                  
                  n.colony=n.colony,
                  n.years=n.years,
                  pop_init=pop_init,
                  
                  E=E,
                  marr_index_with_cmr=marr_index_with_cmr,
                  
                  nest.states = nest.states,
                  breed.states = breed.states,
                  prebr.states = prebr.states,
                  col2colclass = col2colclass,
                  state2col = state2col,
                  state2ageclass = state2ageclass,
                  state2colclass = state2colclass) 

# Initial values
inits <- function(){
  
  return(list())
}

# Parameters monitored
parameters <- c("phi", "kappa", "eta_monitored", "nu_monitored",
                "rho", "sigma",
                "po",
                "N", "B")
# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 40000; nb <- 10000; nc <- 3; nt <- 10; na <- 4000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "/lustre/rumianowskio/models/integrated_v14_0.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/out1_integrated_v14_0.Rda")




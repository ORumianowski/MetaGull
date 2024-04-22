
# setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(jagsUI)
library(tidyverse)


# Dataset -----------------------------------------------------------------


load(file = "/lustre/rumianowskio/real_dataset/v1_AE/B.Rda")
load(file = "/lustre/rumianowskio/real_dataset/v1_AE/marr_AE2.Rda")
load(file = "/lustre/rumianowskio/reading_effort/reading_effort_AE.Rda")

# load(file = "1_formatage/real_dataset/v1_AE/B.Rda")
# load(file = "1_formatage/real_dataset/v1_AE/marr_AE2.Rda")
# load(file = "1_formatage/reading_effort/reading_effort_AE.Rda")

# Bundle data  ------------------------------------------------------------

survey_data = B # Survey dataset - Number of breeders reported 
rel = rowSums(marr) # number of released birds at each occasions

# Study period
start_year = 1986
end_year = 2005
n.years = end_year-start_year+1 # number of years in this survey 

# Metapopulation traits
n.age.class = 3 # number of age classes in this model
n.colony = 5 # number of colonies in this survey

# Shortcut variables
ns=n.colony*n.age.class # number of state in this model 

nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 


# Numbers of prebreeders and breeders for the priors, the first year
repro_ratio = c(0.34/(1-0.34), rep(0.14/(1-0.14),n.colony-1))  # Age ratio estimated with a formula from Kery
prior_incert = 0.60 # Uncertainty about first-year numbers

pop_init = data.frame(estim = B[,1]) %>%
  mutate(B_inf = estim*(1-prior_incert))%>%
  mutate(B_sup = estim*(1+prior_incert)+1)%>%
  mutate(N_inf = estim*(repro_ratio)*(1-prior_incert))%>%
  mutate(N_sup = estim*(repro_ratio)*(1+prior_incert)+1) %>%
  round() %>%
  as.matrix()

# Dirichlet prior parameters for dispersion
dirich_param <- rep(1, n.colony)

# Extinction
# Where and when are no reproduction ?
E = ifelse(survey_data == 0, 0, 1) %>%
  replace(is.na(.), 1)

# Detection effort
# The probability of recapture is set at 0 in years when there has been no observation pressure for ring readings

# Matrice de detection
detection = ifelse(is.na(reading_effort), 0, 1)%>%
  as.data.frame() %>% 
  select(2:ncol(.))  # no detection on first year

# Ensemble des indices
all_col_year = cbind(rep((n.colony+1):(2*n.colony), each = (n.years-1)), rep(1:(n.years-1), time = n.colony)) %>% as.data.frame()
colnames(all_col_year) = c("state","year")

# Indices avec detection
detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
colnames(detection_index) = c("state", "year")
detection_index[,c("state")] = detection_index[,c("state")] + n.colony
nrow_detection_index = nrow(detection_index)

# Indices sans detection
no_detection_index = anti_join(all_col_year, detection_index, by = c("state","year"))
nrow_no_detection_index = nrow(no_detection_index)

# Convertion en matrice
detection_index = detection_index %>% as.matrix()
no_detection_index = no_detection_index %>% as.matrix()


# Built list of data and constant for jags
jags.data <- list(C=survey_data,
                  marr = marr, rel=rel,
                  
                  ns=ns,
                  zero=matrix(0, ncol=ns, nrow=ns),
                  ones=diag(ns),
                  
                  n.colony=n.colony,
                  n.years=n.years,
                  pop_init=pop_init,
                  dirich_param=dirich_param,
                  
                  E=E,
                  
                  detection_index = detection_index,
                  nrow_detection_index = nrow_detection_index,
                  
                  no_detection_index = no_detection_index,
                  nrow_no_detection_index = nrow_no_detection_index,
                  
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
parameters <- c("phi", "kappa", 
                "natalfidelity", "breedingfidelity",
                "DIFFbreedingfidelity",
                "eta", "nu",
                "rho", "sigma",
                "po",
                "N", "B"
                )
# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 50000; nb <- 15000; nc <- 3; nt <- 20; na <- 5000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "/lustre/rumianowskio/models/integrated_v14_diriclet.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/v14_diriclet_real.Rda")



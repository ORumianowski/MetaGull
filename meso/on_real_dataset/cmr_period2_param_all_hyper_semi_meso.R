
# setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(jagsUI)
library(tidyverse)


# Dataset -----------------------------------------------------------------


load(file = "/lustre/rumianowskio/real_dataset/period2/B_P2_v3.Rda")
load(file = "/lustre/rumianowskio/real_dataset/period2/marr_period2_2.Rda")
load(file = "/lustre/rumianowskio/reading_effort/reading_effort_period2.Rda")

# load(file = "1_formatage/real_dataset/period2/B_P2_v3.Rda")
# load(file = "1_formatage/real_dataset/period2/marr_period2_2.Rda")
# load(file = "1_formatage/reading_effort/reading_effort_period2.Rda")


B[6,"2017"] = 35
B[7,"2017"] = 35
B[7,"2016"] = 35

# B[6:8,c("2016", "2017")] = 2000

# Bundle data  ------------------------------------------------------------

survey_data = B # Survey dataset - Number of breeders reported 
rel = rowSums(marr) # number of released birds at each occasions

# Study period
start_year = 1986
end_year = 2019
end_period1 = 2006
end_period2 = 2009
n.years = end_year-start_year+1 # number of years in this survey 

# Metapopulation traits
n.age.class = 3 # number of age classes in this model
n.colony = 10 # number of colonies in this survey

# Shortcut variables
ns=n.colony*n.age.class # number of state in this model 

nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 

year2period = c(rep(1, (end_period1-start_year+1)), rep(2, (end_period2-end_period1+2)), rep(3, (end_year-end_period2+2)))
nb_period = max(year2period)

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
                  state2colclass = state2colclass,
                  year2period = year2period,
                  nb_period=nb_period) 



# Initial values
inits <- function(){
  return(list())
}

# Parameters monitored
parameters <- c("phi", "kappa", 
                "natalfidelity", "breedingfidelity",
                "DIFFbreedingfidelity",
                "eta_monitored", "nu_monitored",
                "rho", "sigma",
                "po",
                "N", "B"
                )
# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 20000; nb <- 10000; nc <- 3; nt <- 4; na <- 4000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "/lustre/rumianowskio/models/cmr_period2_v2_all_hyper_semi.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/cmr_period2_v2_all_hyper_semi.Rda")

# 
# marr[666,1:991]

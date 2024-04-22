
#setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(jagsUI)
library(tidyverse)


# Dataset -----------------------------------------------------------------

load(file = "/lustre/rumianowskio/real_dataset/v1_AE/B.Rda")
load(file = "/lustre/rumianowskio/real_dataset/v1_AE/marr_AE2.Rda")

# load(file = "1_formatage/real_dataset/v1_AE/B.Rda")
# load(file = "1_formatage/real_dataset/v1_AE/marr_AE2.Rda")

# Bundle data  ------------------------------------------------------------

n.age.class = 3 # number of age classes in this model
survey_data = B # Survey dataset - Number of breeders reported 
n.colony = 5 # number of colonies in this survey
n.years = 20 # number of years in this survey

rel = rowSums(marr) # number of released birds at each occasions

ns=n.colony*n.age.class # number of state in this model 

nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 

# Numbers of prebreeders and breeders for the priors, the first year

# repro_ratio = c(0.34/0.66, rep(0.14/0.86,n.colony-1))  # Age ratio estimated with a formula from Kery 
# 
# pop_init = data.frame(estim = B[,1]) %>% 
#   mutate(B_inf = estim*0.4)%>% 
#   mutate(B_sup = estim*1.6+1)%>% 
#   mutate(N_inf = estim*(repro_ratio)*0.4)%>% 
#   mutate(N_sup = estim*(repro_ratio)*1.6+1) %>% 
#   round() %>% 
#   as.matrix()

# Extinction
# Where and when are no reproduction ?

# Extincted
E = ifelse(survey_data == 0, 0, 1) %>%
  replace(is.na(.), 1)

marr_index_with_cmr = 1:((n.years-1)*ns)

# Detection effort
# The probability of recapture is set at 0 in years when there has been no observation pressure for ring readings

#load(file = "1_formatage/reading_effort/reading_effort_AE.Rda")
load(file = "/lustre/rumianowskio/reading_effort/reading_effort_AE.Rda")

detection = ifelse(is.na(reading_effort), 0, 1)
detection = detection[,2:ncol(detection)] # no detection on first year

detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
detection_index[,1] = detection_index[,1] + n.colony

colnames(detection_index) = c("colony", "year")


all_col_year = cbind(rep((n.colony+1):(2*n.colony), each = (n.years-1)), rep(1:(n.years-1), time = n.colony)) %>% as.data.frame()
colnames(all_col_year) = c("colony","year")

no_detection_index = anti_join(all_col_year, detection_index, by = c("colony","year"))
nrow_no_detection_index = nrow(no_detection_index)

detection_index = detection_index %>% as.matrix()

detection_index = detection_index %>% 
  as.data.frame() %>% 
  filter(., colony!=6) %>% 
  as.matrix()

nrow_detection_index = nrow(detection_index)

no_detection_index = no_detection_index %>% as.matrix()

# Built list of data and constant for jags
jags.data <- list(#C=survey_data,
                  marr = marr, rel=rel,
                  
                  ns=ns,
                  zero=matrix(0, ncol=ns, nrow=ns),
                  ones=diag(ns),
                  
                  n.colony=n.colony,
                  n.years=n.years,
                  #pop_init=pop_init,
                  
                  E=E,
                  marr_index_with_cmr=marr_index_with_cmr,
                  
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
                "eta_monitored", "nu_monitored",
                #"rho", "sigma",
                "po"#,
                #"N", "B"
                )
# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 40000; nb <- 10000; nc <- 3; nt <- 40; na <- 4000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "/lustre/rumianowskio/models/semi_cmr_AE_v12_priors_fort.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/out1_cmr_AE_v12_priors_fort.Rda")




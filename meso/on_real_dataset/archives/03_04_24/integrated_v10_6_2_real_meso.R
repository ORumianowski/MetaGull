
#setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull/2_models/2_models_for_real")

# library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------

load(file = "/lustre/rumianowskio/real_dataset_v4/B.Rda")
load(file = "/lustre/rumianowskio/real_dataset_v4/marr_reel.Rda")

# load(file = "real_dataset_v3/B.Rda")
# load(file = "real_dataset_v3/marr_reel.Rda")
marr = marr_reel

# load(file = "real_dataset/CH_real.Rda")
# CH_real = matrix(CH_real %>% as.integer(), ncol = ncol(CH_real), nrow = nrow(CH_real))
# 
# breed.1st.summer = c(8843, 10720, 13638, 14573, 15207, 16036, 16440, 18105, 19706, 19809, 20030, 21491, 21678, 21832, 22122, 22944, 25818, 26150, 26263, 26389)
# 
# CH_real = CH_real %>%
#   as.data.frame() %>%
#   slice(-breed.1st.summer) %>%
#   as.matrix()

# Bundle data  ------------------------------------------------------------

n.age.class = 3 # number of age classes in this model
survey_data = B # Survey dataset - Number of breeders reported # j'enlenve les NA pour chercher l'erreur
n.colony = nrow(survey_data) # number of colonies in this survey
n.years = ncol(survey_data)  # number of years in this survey
# y = CH_real # Capture Histories
# y = rbind(y, c(rep(0, time = ncol(y)-1), 5)) # one virtual nestling ringed in colony 5 at last occasion
# marr = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 and 11 to 15
# save(marr, file = "marr.Rda")
rel = rowSums(marr) # number of released birds at each occasions



ns=n.colony*n.age.class # number of state in this model 

nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 

# For informative prior 
#real_phi = phi
#real_rho = rho
#real_kappa = kappa
#real_eta = eta
#real_nu = nu

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



# Absence of CMR

ABSENCE = which(E == 0, arr.ind = TRUE)
colnames(ABSENCE) = c("colony", "year")
nrow_ABSENCE = nrow(ABSENCE)

# Determine the m-array indexes for which there was no cmr
index_no_cmr = c()
if (nrow(ABSENCE)>0){
  for (k in 1:nrow(ABSENCE)){
    colony = ABSENCE[k,"colony"]
    year = ABSENCE[k,"year"]
    index = (year-1)*ns+(n.colony+colony)
    index_no_cmr = c(index_no_cmr, index)
  }
}
marr_index_with_cmr = setdiff(1:((n.years-1)*ns), index_no_cmr) 

# Detection effort

load(file = "/lustre/rumianowskio/read_effort_v1/reading_effort.Rda")

detection = ifelse(is.na(reading_effort), 0, 1)
detection = detection[,2:ncol(detection)] # no detection on first year

detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
detection_index[,1] = detection_index[,1] + n.colony

colnames(detection_index) = c("colony", "year")
nrow_detection_index = nrow(detection_index)

all_col_year = cbind(rep((n.colony+1):(2*n.colony), each = (n.years-1)), rep(1:(n.years-1), time = n.colony)) %>% as.data.frame()
colnames(all_col_year) = c("colony","year")

no_detection_index = anti_join(all_col_year, detection_index, by = c("colony","year"))
nrow_no_detection_index = nrow(no_detection_index)

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
parameters <- c("phi", "kappa", "eta_monitored", "nu_monitored",
                "rho", "sigma",
                "po",
                "N", "B"
                )
# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
#ni <- 10000; nb <- 5000; nc <- 1; nt <- 500; na <- 4000
#ni <- 1000; nb <- 500; nc <- 1; nt <- 5; na <- 2000
ni <- 50000; nb <- 10000; nc <- 3; nt <- 60; na <- 4000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "/lustre/rumianowskio/model_v10_6.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/out1_integrated_v10_6_2_real.Rda")





#setwd("C:/Users/33763/Documents/CMRi/0_Github/Metapopulation-dynamics-following-a-major-disturbance---Master-s-thesis/1_Modélisation/1_IPM/6_IPM_on_real_dataset")


# library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------

# load(file = "/lustre/rumianowskio/real_dataset/B.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/marr.Rda")

load(file = "real_dataset_v3/B.Rda")
load(file = "real_dataset_v3/marr_reel.Rda")
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

rel_matrix = rowSums(marr_reel) %>% 
  as.vector() %>% 
  matrix(., ncol = 15, nrow = 19, byrow = T)

all_col_year = cbind(rep(1:(n.years-1), time = n.colony),rep(1:n.colony, each = (n.years-1))) %>% as.data.frame()
colnames(all_col_year) = c("year", "colony")

detection = ifelse(rel_matrix[,(n.colony+1):(2*n.colony)] == 0, 0, 1) 


detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
detection_index = all_col_year[-c(20:28,77:95),]

colnames(detection_index) = c("year", "colony")
nrow_detection_index = nrow(detection_index)



no_detection_index = anti_join(all_col_year, detection_index, by = c("year", "colony")) %>% as.matrix()
nrow_no_detection_index = nrow(no_detection_index)

detection_index = detection_index %>% as.matrix()
nrow_detection_index = nrow(detection_index)

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




# Write JAGS model file
cat(file = "model_v10_5.txt", "
model {
 # -------------------------------------------------
  # Stages:
  # N: not-yet recruited individuals
  # B: breeders
  # Parameters:
  # phi[age]: survival probability
  # eta_t[departure site, arrival site, time]: natal dispersal
  # nu_t[departure site, arrival site, time]: breeding dispersal
  # kappa[site]: recruitment probability - two classes (LaRonze or not)
  # rho[site]: productivity - two classes (LaRonze or not)
  # p[site, time]: recapture probability 
  # -------------------------------------------------
  # Priors 
  
  rho[1] ~ dunif(0.1, 3) # Productivity - La Ronze
  rho[2] ~ dunif(0.1, 3) # Productivity - Satellite colonies
  
  phi[1] ~ dunif(0, 1) # Survival prob. - First year
  phi[2] ~ dunif(0, 1) # Survival prob. - Subadult and adults
  
  kappa[1] ~ dunif(0, 1) # Recruitment prob. -  La Ronze
  kappa[2] ~ dunif(0, 1) # Recruitment prob. -  Satellite colonies
  
  # eta : parameter which is defined with a prior - at this stage: sum(eta[dep,]) is not equal to 1
  # eta_monitored : parameter of interest, eta_monitored is eta normalised i.e. sum equal to 1
  # eta_ext depends partly on time. It is set to eta by default and updated in the event of extinction
  # eta_t is the normalized and updated version of eta - This is the one used in the process equation.
  
  for (dep in 1:n.colony){
    for (arr in 1:n.colony){
      eta[dep, arr]  ~ dunif(0, 1)   # natal dispersal
      nu[dep, arr]  ~ dunif(0, 1)    # breeding dispersal
      eta_monitored[dep, arr] <- (eta[dep, arr] / sum(eta[dep,])) 
      nu_monitored[dep, arr] <- (nu[dep, arr] / sum(nu[dep,])) 
    }
  }
  

  
  for (t in 1:(n.years-1)){
    for (dep in 1:n.colony){
      for (arr in 1:n.colony){
          eta_ext[dep, arr, t] <-  eta[dep, arr]   * (1-equals(E[arr,(t+1)],0)) # The value is 0 if you are moving towards a colony that has disappeared
          nu_ext[dep, arr, t] <- nu[dep, arr]  * (1-equals(E[arr,(t+1)],0)) # The value is 0 if you are moving towards a colony that has disappeared
      }
    }
  }
  


  for (t in 1:(n.years-1)){
    for (dep in 1:n.colony){
      for (arr in 1:n.colony){
          eta_t[dep, arr, t] <-  (eta_ext[dep, arr, t] / sum(eta_ext[dep, , t]))  
          nu_t[dep, arr, t] <- (nu_ext[dep, arr, t] / sum(nu_ext[dep, , t]))   
      }
    }
  }
   
  # Population count data (state-space model)

  # Models for the initial population size: uniform priors
  for (col in 1:n.colony){
    B[col,1] ~ dunif(pop_init[col,2], pop_init[col,3])
    N[col,1] ~ dunif(pop_init[col,4], pop_init[col,5])
  }

   # Process model over time: our model of population dynamics
  for (t in 1:(n.years-1)){
    for (s in 1:n.colony){

    N[s,t+1] <- B[s,t] * rho[col2colclass[s]] * phi[1] +
                N[s,t] * phi[2] * sum(eta_t[s,1:n.colony,t] * (1-kappa[col2colclass]))

    B[s,t+1] <- sum(N[1:n.colony,t] * phi[2] * eta_t[1:n.colony,s,t]  * kappa[col2colclass[s]] ) +
                sum(B[1:n.colony,t] * phi[2] * nu_t[1:n.colony,s,t])
    }
  }

  # Residual (observation) error

  sigma[1] ~ dunif(0.05, 1000) # La Ronze
  sigma[2] ~ dunif(0.05, 1000) # Satellite colonies

  # Survey observation model
  for (t in 1:(n.years)){
    for (s in 1:n.colony){

    R[s,t] <- sigma[col2colclass[s]] * B[s,t]* (1-equals(E[s,t],0)) + 1E-9
    tau[s,t] <- pow(R[s,t], -2)

      C[s,t] ~ dnorm(B[s,t],tau[s,t])
    }
  }


  
  # Define state-transition - CMR multistate
  for (t in 1:(n.years-1)){
    # Nestling
    for (prev.state in nest.states){
      # nestlings may become pre breeders
      for (next_state in prebr.states){
        psi[prev.state,next_state,t] <- phi[1] * equals(state2col[next_state], state2col[prev.state]) 
      }
      # nestlings do not become nestling or breeders
      for (next_state in c(nest.states,breed.states)){
        psi[prev.state,next_state,t] <- 0
      }
    }
    # Breeders
    for (prev.state in breed.states){
      # Breeders may stay breeders
      for (next_state in breed.states){
        psi[prev.state,next_state,t] <- phi[2] * nu_t[state2col[prev.state],state2col[next_state],t]
      }
      # Breeders cannot become nestling or prebreeders
          for (next_state in c(nest.states, prebr.states)){
        psi[prev.state,next_state,t] <- 0
      }
    }
    # Pre-Breeders
    for (prev.state in prebr.states){
     # Prebreeders cannot become nestling
      for (next_state in nest.states){
        psi[prev.state,next_state,t] <- 0
      }
      
    # Prebreeders may become breeders
    for (next_state in breed.states){
        psi[prev.state,next_state,t] <- phi[2] * kappa[state2colclass[next_state]] * eta_t[state2col[prev.state],state2col[next_state],t]
    }
    
    # Prebreeders may stay prebreeders
    for (next_state in prebr.states){
          psi[prev.state,next_state,t] <- equals(state2col[next_state], state2col[prev.state]) 
          * phi[2] * sum(eta_t[state2col[prev.state],1:n.colony,t] * (1-kappa[col2colclass]))
        
      }  
    }
  }
  
  #  Define re-encounter probabilities
  for (t in 1:(n.years - 1)) {
  # Nestlings & Prebreeders
    for (state in c(nest.states, prebr.states)) {
      po[state, t] <- 0
      }
    }
  # Breeders
  # Surveyed colonies
  for (k in 1:nrow_detection_index) {
    po[detection_index[k, 2] + n.colony, detection_index[k, 1]] ~ dunif(0, 1)
  }
  # Non-Surveyed colonies
  for (k in 1:nrow_no_detection_index) {
    po[no_detection_index[k, 2] + n.colony, no_detection_index[k, 1]] <- 0
  }



    # Calculate probability of non-encounter (dq) and reshape the array for the encounter
    # probabilities
  for (t in 1:(n.years-1)){  
    for (s in 1:ns){
      dp[s,t,s] <- po[s,t]
      dq[s,t,s] <- 1-po[s,t]
    } #s
    for (s in 1:(ns-1)){
      for (m in (s+1):ns){
        dp[s,t,m] <- 0
        dq[s,t,m] <- 0
      } #m
    } #s
    for (s in 2:ns){
      for (m in 1:(s-1)){
        dp[s,t,m] <- 0
        dq[s,t,m] <- 0
      } #m
    } #s
  } #t
  
  # Define the cell probabilities of the multistate m-array
    for (t in 1:(n.years-2)){
      U[(t-1)*ns+(1:ns), (t-1)*ns+(1:ns)] <- ones
      for (j in (t+1):(n.years-1)){
        U[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns), (j-2)*ns+(1:ns)] %*% psi[,,j] %*% dq[,t,]
    } #j
  } #t
  
  U[(n.years-2)*ns+(1:ns), (n.years-2)*ns+(1:ns)] <- ones
  
  # Diagonal
  for (t in 1:(n.years-2)){
    pr[(t-1)*ns+(1:ns),(t-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns),(t-1)*ns+(1:ns)] %*% psi[,,t] %*% dp[,t,]
  # Above main diagonal
  for (j in (t+1):(n.years-1)){
    pr[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] %*% psi[,,j] %*% dp[,j,]
    } #j
  } #t
  
  pr[(n.years-2)*ns+(1:ns), (n.years-2)*ns+(1:ns)] <- psi[,,n.years-1] %*% dp[,n.years-1,] 
  
  # Below main diagonal
  for (t in 2:(n.years-1)){
    for (j in 1:(t-1)){
      pr[(t-1)*ns+(1:ns),(j-1)*ns+(1:ns)] <- zero
    } #j
  } #t
  
  # Last column: probability of non-recapture
  for (t in 1:((n.years-1)*ns)){
    pr[t,(n.years*ns-(ns-1))] <- 1-sum(pr[t,1:((n.years-1)*ns)])
  } #t
    
  # Define the multinomial likelihood
  for (t in marr_index_with_cmr){
     marr[t,1:(n.years*ns-(ns-1))] ~ dmulti(pr[t,], rel[t])
  }
}
")

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
out1 <- jags(jags.data, inits, parameters, "model_v10_5.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)

save(out1, file = "/lustre/rumianowskio/out1_integrated_v10_5_cmr_real.Rda")




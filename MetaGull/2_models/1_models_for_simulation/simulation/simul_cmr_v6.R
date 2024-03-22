

source("simulation/model_parameters_v6.R")

# Simulate CMR dataset ----------------------------------------------------

# Determine occasion when an individual first captured and marked
# The nestlings ringed the two last year bring no informations (no occasion to detect them)
f = c()
for (k in 1:(n.years-2)){
  f = c(f, rep(k, time = n.marked[k, "total_LR"]))
  f = c(f, rep(k, time = n.marked[k, "total_SAT"]))
}

nind = length(f) # Total number of marked individuals


# Construct the transition probability matrix (TPM),
# Includes the dead state as last state 
# Departure state in rows (time t), arrival state in columns (t+1)

# Future of nestlings 
TMP_nestling = function(birth_colony){
  
  next_state = matrix(0, nrow = 1, ncol = n.state)
  
  # nestling becomes prebreeder in its birth colony
  next_state[1,(2*n.colony+birth_colony)] = phi1
  # nestling becomes dead
  next_state[1,n.state] = 1 - phi1
  
  return(next_state)
}


# Future of breeders 
TMP_breeder = function(former_colony, nu_t){
  
  next_state = matrix(0, nrow = 1, ncol = n.state)
  
  # breeder stays breeder
  next_state[1,(1*n.colony+1):(2*n.colony)] = phiA * nu_t[former_colony,] 
  # becomes dead
  next_state[1,n.state] = 1 - phiA 
  
  return(next_state)
}

# Future of prebreeders
TMP_prebreeder = function(birth_colony, eta_t){
  
  next_state = matrix(0, nrow = 1, ncol = n.state)
  
  # becomes a breeder
  next_state[1,(1*n.colony+1):(2*n.colony)] = phiA * kappa[col2colclass] * eta_t[birth_colony,]
  # remains a prebreeders
  next_state[1,(2*n.colony+birth_colony)] = phiA * sum(eta_t[birth_colony,1:n.colony] * (1-kappa[col2colclass]))
  # becomes dead
  next_state[1,n.state] = 1 - phiA 
  
  return(next_state)
}


get_TPM = function(year){
  
  eta_ = ETA_t[[year]]
  nu_ = NU_t[[year]]
  
  TPM = matrix(NA , nrow=n.state, ncol=n.state)
  
  # Building TPM
  for (col in 1:n.colony){
   TPM[col,] = TMP_nestling(birth_colony = col)
   TPM[(n.colony + col),] = TMP_breeder(former_colony = col, nu_t = nu_)
   TPM[(2* n.colony + col),] = TMP_prebreeder(birth_colony = col, eta_t = eta_)
  }
  
  # Last line of TPM : Dead stays dead
  TPM[n.state, ] = 0
  TPM[n.state, n.state] = 1
  
  return(TPM)
}

TPM_t = list()
for (t in 1:(n.years-1)){
  TPM_t = c(TPM_t,list(get_TPM(year = t)))
}


# Construct the observation probability matrix (OPM)
# True state is in rows, observed state is in columns

OPM = array(0 , dim = c(n.state, n.state, n.years))
for (t in 1:n.years){
  for (col in 1:n.colony){
    OPM[n.colony + col, n.colony + col, t] = p[t,col]
  }
  OPM[,ncol(OPM), t] = c(1-rowSums(OPM[,,t]))
}

# State or ecological process
# Simulate true system state
z = array(NA, dim=c(nind, n.years)) # Empty alive/dead matrix
# Initial conditions: all individuals alive at f(i)

#

initial.state = c()
for (k in 1:(n.years-2)){
  
  # Nestling from La Ronze
  initial.state = c(initial.state, rep(1, time = n.marked[k, "total_LR"]))
  
  # Nestling from satellite colonies
  # Normal number of nestling in one satellite colony
  n.sat = round(n.marked[k, "total_SAT"]/(n.colony-1))
  # Total number of nestling in satellite colonies
  n.total.sat = sum(n.marked[k,"total_SAT"])
  # Number of nestling in the last satellite colony - to reach right amount despite the 'round()'
  n.last.sat = n.total.sat - ((n.colony-2)*n.sat)
  
  initial.state = c(initial.state, rep(2:(n.colony-1), each = n.sat))
  initial.state = c(initial.state, rep(n.colony, time = n.last.sat)) 
}

initial.state = as.integer(initial.state)

for (i in 1:nind){
  z[i,f[i]] = initial.state[i]
}

# Propagate alive/dead process forwards via transition rule (=TPM)
for (i in 1:nind){
  for (t in (f[i]+1):n.years){
    
    departure.state = z[i, (t-1)]
    arrival.state = which(rmultinom(1, 1, TPM_t[[t-1]][departure.state, ]) == 1)
    z[i, t] = arrival.state
    
  } #t
} #i


# Observation process: simulate observations using observation matrix OPM 
y = array(16, dim=c(nind, n.years))
for (i in 1:nind){
  y[i,f[i]] = z[i,f[i]]
  for (t in (f[i]+1):n.years){
    true.state = z[i,t] # t et non pas t-1 !
    observed.state = which(rmultinom(1,1, OPM[true.state,,t])==1)
    y[i,t] = observed.state
  } #t
} #i

y[y==16] <- 0
#marr <- marray(y, unobs=5)
# 
library(tidyverse)
# créer le site fantome
# retirer les individus bagués sur le site 5

y <- y %>%
  as.data.frame() %>%
  filter(!(if_any(everything(), ~. == 5))) %>%
  as.matrix()

# retirer les contacts réalisés sur le site 5
y[y==10] <- 0
y[y==15] <- 0

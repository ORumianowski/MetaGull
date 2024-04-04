

library(jagsUI)
library(tidyverse)
library(IPMbook)

# Choose values for data simulation
nmarked <- 10 # Number of marked individuals at each occasion
nyears <- 11 # Number of years
phi <- 0.8 # Constant apparent survival probability 
p <- 0.4 # Constant recapture probability 

# Determine occasion when an individual first captured and marked
f <- rep(1:(nyears-1), each=nmarked)
nind <- length(f) # Total number of marked individuals
# State or ecological process
z <- array(NA, dim=c(nind, nyears)) # Empty alive/dead matrix
# Initial conditions: all individuals alive at f(i)
for (i in 1:nind){
  z[i,f[i]] <- 1
}
set.seed(1) # Initialize the RNGs in R
# Propagate alive/dead process forwards via transition rule: 
# Alive individuals survive with probability phi
for (i in 1:nind){
  for (t in (f[i]+1):nyears){
    z[i,t] <- rbinom(1, 1, z[i,t-1] * phi)
  } #t
} #i
head(z); tail(z) # Not shown: look at start and end of z


# Observation process: simulate observations
y <- array(0, dim=c(nind, nyears))
for (i in 1:nind){
  y[i,f[i]] <- 1 
  for(t in (f[i]+1):nyears){
    y[i,t] <- rbinom(1, 1, z[i,t] * p)
  } #t
} #i




# Data bundle
jags.data <- list(y=y, f=f, nind=nind, nyears=ncol(y))
str(jags.data)


# Write JAGS model file
cat(file="model14.txt", "
model {
# Priors and linear models
phi.const ~ dunif(0, 1) # Vague prior for constant phi
p.const ~ dunif(0, 1) # Vague prior for constant p
for (i in 1:nind){ # Loop over individuals
for (t in f[i]:(nyears-1)){ # Loop over time intervals/occasions
phi[i,t] <- phi.const # Here we model pattern in phi ...
p[i,t] <- p.const # ... and p
} #t
} #i
# Likelihood
for (i in 1:nind){
# Define latent state at first capture
z[i,f[i]] <- 1
for (t in (f[i]+1):nyears){
# State process
z[i,t] ~ dbern(z[i,t-1] * phi[i,t-1])
# Observation process
y[i,t] ~ dbern(z[i,t] * p[i,t-1])
} #t
} #i
}
")


# Initial values
inits <- function(){list(z=zInit(y))}

# Parameters monitored
parameters <- c("phi.const", "p.const") # Could also add "z"
# MCMC settings
ni <- 3000; nb <- 1000; nc <- 3; nt <- 1; na <- 1000
# Call JAGS from R (ART < 1 min) and check convergence
out17 <- jags(jags.data, inits, parameters, "model14.txt", n.iter=ni, n.burnin=nb, n.chains=nc,
              n.thin=nt, n.adapt=na, parallel=TRUE)


out17$mean

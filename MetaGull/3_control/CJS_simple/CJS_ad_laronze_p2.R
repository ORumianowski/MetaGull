

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)


# Specify model in BUGS language
cat(file = "3_control/model_CJS.txt", "
model {

  phi ~ dunif(0, 1) # Prior for mean survival
  p ~ dunif(0, 1) # Prior for mean recapture

  # Likelihood
  for (i in 1:nind) {
    # Define latent state at first capture
    z[i, f[i]] <- 1
    
    for (t in (f[i] + 1):n.occasions) {
      # State process
        z[i, t] ~ dbern(mu1[i, t])
        mu1[i, t] <- phi* z[i, t-1]
        
      # Observation process
       y[i, t] ~ dbern(mu2[i, t])
       mu2[i, t] <- p * z[i, t]
       
    } #t
  } #i
}
")

# Initial values
inits <- function(){
  return(list())
}


# Bundle data
jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions =
                    dim(CH)[2])


# Parameters monitored
parameters <- c("mean.phi", "mean.p")

# MCMC settings
ni <- 10000
nt <- 6
nb <- 5000
nc <- 1 # 3
na <- 400

# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "3_control/model_CJS.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)
















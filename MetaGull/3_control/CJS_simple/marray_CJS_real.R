

library(nimble)



n.occasions = 19

CH.A = CH


# Convert CH to m-array ---------------------------------------------------

# Function to create a m-array based on capture-histories (CH)
marray <- function(CH){
  nind <- dim(CH)[1]
  n.occasions <- dim(CH)[2]
  m.array <- matrix(data = 0, ncol = n.occasions+1, nrow =
                      n.occasions)
  # Calculate the number of released individuals at each time period
  for (t in 1:n.occasions){
    m.array[t,1] <- sum(CH[,t])
  }
  for (i in 1:nind){
    pos <- which(CH[i,]!=0)
    g <- length(pos)
    for (z in 1:(g-1)){
      m.array[pos[z],pos[z+1]] <- m.array[pos[z],pos[z+1]] + 1
    } #z
  } #i
  # Calculate the number of individuals that is never recaptured
  for (t in 1:n.occasions){
    m.array[t,n.occasions+1] <- m.array[t,1] -
      sum(m.array[t,2:n.occasions])
  }
  out <- m.array[1:(n.occasions-1),2:(n.occasions+1)]
  return(out)
}

# Split CH according age-class --------------------------------------------

cap <- apply(CH.A, 1, sum)
ind <- which(cap >= 2)


CH.A.marray <- marray(CH.A)


# Model and computational parameters --------------------------------------

CJSCode = nimbleCode(code =
                       {
                         # Priors and constraints
                         for (t in 1:(n.occasions-1)){
                           phi.ad[t] <- mean.phiad
                           p[t] <- mean.p
                         }
                         mean.phiad ~ dunif(0, 1) # Prior for mean ad. survival
                         mean.p ~ dunif(0, 1) # Prior for mean recapture
                         # Define the multinomial likelihood
                         for (t in 1:(n.occasions-1)){
                           marr.a[t,1:n.occasions] ~ dmulti(pr.a[t,1:n.occasions], r.a[t])
                         }
                         # Define the cell probabilities of the m-arrays
                         # Main diagonal
                         for (t in 1:(n.occasions-1)){
                           q[t] <- 1-p[t] # Probability of non-recapture
                           pr.a[t,t] <- phi.ad[t]*p[t]
                           # Above main diagonal
                           for (j in (t+1):(n.occasions-1)){
                             pr.a[t,j] <- prod(phi.ad[t:j])*prod(q[t:(j-1)])*p[j]
                           } #j
                           # Below main diagonal
                           for (j in 1:(t-1)){
                             pr.a[t,j] <- 0
                           } #j
                         } #t
                         # Last column: probability of non-recapture
                         for (t in 1:(n.occasions-1)){
                           pr.a[t,n.occasions] <- 1-sum(pr.a[t,1:(n.occasions-1)])
                         } #t 
                       }
)




r.a = c()
# Calculate the number of birds released each year
for (t in 1:(n.occasions-1)){

  r.a[t] <- sum(CH.A.marray[t,])
}

# Constants
CJSConsts <- list(n.occasions = dim(CH.A.marray)[2],  r.a = r.a)

# Bundle data
CJSData <- list(marr.a = CH.A.marray,
                  n.occasions = dim(CH.A.marray)[2])
# Initial values
inits <- function(){list( mean.phiad =
                           runif(1, 0, 1), mean.p = runif(1, 0, 1))}


# Parameters monitored
parameters <- c( "mean.phiad", "mean.p")

# MCMC settings
ni <- 50000
nt <- 5
nb <- 10000
nc <- 3


# Run ---------------------------------------------------------------------

# mcmc.out <- nimbleMCMC(code = CJSCode, constants = CJSConsts,
#                        data = CJSData, inits = inits,
#                        monitors = parameters,
#                        niter = ni, nchains = nc, nburnin = nb, thin = nt,
#                        summary = TRUE, WAIC = TRUE)

# Version self-designed

start_time <- proc.time()

CJS <- nimbleModel(code = CJSCode, name = "CJS", constants = CJSConsts,
                  data = CJSData, inits = inits())
CCJS<- compileNimble(CJS)

CJSConf <- configureMCMC(CJS, enableWAIC = TRUE, print = TRUE)

CJSMCMC <- buildMCMC(CJSConf)
CCJSMCMC <- compileNimble(CJSMCMC, project = CJS)

mcmc.out <- runMCMC(CCJSMCMC,
                    niter = ni, nchains = nc,
                    nburnin = nb, thin = nt,
                    inits = inits, setSeed = TRUE,
                    samples = TRUE, summary = TRUE)


# CCJSMCMC$run(niter = ni, time = TRUE)
# CCJSMCMC$getTimes()
# calculateWAIC(CCJSMCMC)

end_time <- proc.time()

elapsed_time <- end_time - start_time
print(elapsed_time)

# Check and plot ----------------------------------------------------------


MCMCvis::MCMCtrace(object = mcmc.out[["samples"]],
                     pdf = FALSE, # no export to PDF
                     ind = TRUE)

MCMCvis::MCMCsummary(mcmc.out[["samples"]])

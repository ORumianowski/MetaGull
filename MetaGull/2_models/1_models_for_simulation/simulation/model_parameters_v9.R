


# Model parameters --------------------------------------------------------

set.seed(452)

#load("simulation/ringed_nest_for_simulation.Rda") 
load("simulation/ringed_nest_for_simulation.Rda") 

n.marked = ringed_nest_for_simulation # Number of marked individuals each year and site
n.colony = 5 # Number of colonies
n.years = nrow(n.marked) # Number of years
years = 1985:(1985+n.years-1) # Studied years

n.state = 3 * n.colony + 1
# State code
# 1 : Nestling born in colony 1
# 2 : Nestling born in colony 2
# 3 : Nestling born in colony 3
# 4 : Nestling born in colony 4
# 5 : Nestling born in colony 5
# 6 : Breeders in colony 1
# 7 : Breeders in colony 2
# 8 : Breeders in colony 3
# 9 : Breeders in colony 4
# 10 : Breeders in colony 5
# 11 : Pre-Breeders in colony 1
# 12 : Pre-Breeders in colony 2
# 13 : Pre-Breeders in colony 3
# 14 : Pre-Breeders in colony 4
# 15 : Pre-Breeders in colony 5
# 16 : DEAD

# Apparent survival probability - age class dependence
phi1 = 0.213 # First year
phiA = 0.78 # Subadult and adult
phi = c(phi1, phiA)

# Productivity
rhoLR = 1.732 # La Ronze
rhoSAT = 0.605 # Satellite colonies
rho = c(rhoLR, rhoSAT)

# Recruitment probability
kappaLR = 0.619 # La Ronze
kappaSAT = 0.846 # Satellite colonies
kappa = c(kappaLR, kappaSAT)

# correspondence from colony to colony type i.e. La Ronze ou satellite colony
col2colclass = c(1, rep(2, (n.colony-1)))

# Natal dispersion from one colony to another - colony dependence
# Quite random & few natal dispersion toward La Ronze
eta = matrix(c(
  0.3, 0.05, 0.2, 0.05, 0.4,
  0.2, 0.2, 0.5, 0.05, 0.05,
  0.1, 0.1, 0.3, 0.2, 0.3,
  0.1, 0.1, 0.1, 0.3, 0.4,
  NA, NA, NA, NA, NA), 
  nrow = n.colony, ncol = n.colony, byrow = T)

eta[5, 1] <- (eta[2, 1] + eta[3, 1] + eta[4, 1])/3 
eta[5, 2] <- (eta[1, 2] + eta[3, 2] + eta[4, 2])/3  
eta[5, 3] <- (eta[1, 3] + eta[2, 3] + eta[4, 3])/3 
eta[5, 4] <- (eta[1, 4] + eta[2, 4] + eta[3, 4])/3 
eta[5, 5] <- 1-eta[5, 1]-eta[5, 2]-eta[5, 3]-eta[5, 4]

# rowSums(eta)

# Breeding dispersion from one colony to another - colony dependence
# Great fidelity and attractiveness ofLa Ronze 
nu = matrix(c(
  0.8, 0.05, 0.1, 0.05, 0.0,
  0.0, 0.4, 0.1, 0.2, 0.3,
  0.4, 0.0, 0.5, 0.0, 0.1,
  0.1, 0.3, 0.3, 0.2, 0.1,
  0.4, 0.2, 0.2, 0.0, 0.2), 
  nrow = n.colony, ncol = n.colony, byrow = T)

# rowSums(nu)


# recapture probability - time and colony dependence 
p = matrix(runif(n = n.years*n.colony , min = 0.2, max = 0.4),
           nrow = n.years, ncol = n.colony)


# Extinction year
ETA_t = list()
NU_t = list()

for (t in 1:(n.years-1)) {
  
  extinted_col = sample(c(2:n.colony), size = rpois(1, 0.4))
  
  # The disappearance of a colony is modelled only via the dispersion matrices eta and nu
  # If a colony disappear,
  # step 1 - natal and breeding dispersion toward the extinted colony is impossible
  # stage 2 - natal and reproductive dispersal to other colonies remains proportional to eta/nu
  # If no colonies disappear, eta_t = eta
  eta_t  = eta
  eta_t[,extinted_col] = 0
  eta_t = eta_t / rowSums(eta_t)
  ETA_t = c(ETA_t, list(eta_t))
  
  nu_t  = nu
  nu_t[,extinted_col] = 0
  nu_t = nu_t / rowSums(nu_t)
  NU_t = c(NU_t, list(nu_t))
}


# Age structure at equilibrium --------------------------------------------
# Used to define the priors of N and B in the first year

# LaRonze
A <- matrix(c(
  phi[2] * (1-kappa[1]), phi[1] * rho[1],
  phi[2] * kappa[1], phi[2]), byrow=TRUE, ncol=2)
z <- which.max(Re(eigen(A)$values))
revec <- Re(eigen(A)$vectors[,z])
m1 = matrix(revec / sum(revec)) 
# 0.34 0.66


#Satellite
A <- matrix(c(
  phi[2] * (1-kappa[2]), phi[1] * rho[2],
  phi[2] * kappa[2], phi[2]), byrow=TRUE, ncol=2)
z <- which.max(Re(eigen(A)$values))
revec <- Re(eigen(A)$vectors[,z])
m2 = matrix(revec / sum(revec))
#  0.14 0.86



# Number of Breeders and Prebreeders at year 1 ----------------------------

B1 = c(2500, 400, 1500, 300, 1100)
repro_ratio = c(0.34/0.66, rep(0.14/0.86,n.colony-1))  
N1 = round(repro_ratio * B1)


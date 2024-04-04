

n.colony= 5

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
  0.1, 0.05, 0.3, 0.05, 0.5), 
  nrow = n.colony, ncol = n.colony, byrow = T)

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

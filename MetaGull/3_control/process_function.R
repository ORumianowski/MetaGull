
library(ggplot2)
library(patchwork)

# Model parameters --------------------------------------------------------

set.seed(452)

n.colony = 5 # Number of colonies
n.years = 20 # Number of years
years = 1985:(1985+n.years-1) # Studied years

n.state = 3 * n.colony + 1


# Apparent survival probability - age class dependence
phi1 = 0.184 # First year
phiA = 0.743 # Subadult and adult
phi = c(phi1, phiA)

# Productivity
rhoLR = 2.1 # La Ronze
rhoSAT = 1.6 # Satellite colonies
rho = c(rhoLR, rhoSAT)

# Recruitment probability
kappaLR = 0.54 # La Ronze
kappaSAT = 0.69 # Satellite colonies
kappa = c(kappaLR, kappaSAT)

# correspondence from colony to colony type i.e. La Ronze ou satellite colony
col2colclass = c(1, rep(2, (n.colony-1)))

# Natal dispersion from one colony to another - colony dependence
# Quite random & few natal dispersion toward La Ronze
eta = matrix(c(
  0.82, 0.03, 0.05, 0.01, 0.06,
  0.25, 0.36, 0.11, 0.04, 0.22,
  0.25, 0.11, 0.33, 0.10, 0.19,
  0.29, 0.09, 0.09, 0.33, 0.19,
  0.21, 0.11, 0.22, 0.15, 0.29), 
  nrow = n.colony, ncol = n.colony, byrow = T)

# rowSums(eta)

# Breeding dispersion from one colony to another - colony dependence
# Great fidelity and attractiveness ofLa Ronze 
nu = matrix(c(
  0.97, 0.01, 0.01, 0.01, 0.01,
  0.06, 0.49, 0.09, 0.02, 0.33,
  0.06, 0.08, 0.30, 0.05, 0.50,
  0.24, 0.08, 0.10, 0.40, 0.18,
  0.07, 0.08, 0.14, 0.03, 0.68), 
  nrow = n.colony, ncol = n.colony, byrow = T)

# rowSums(nu)

# Extinction year
ETA_t = list()
NU_t = list()

for (t in 1:(n.years-1)) {
  
  extinted_col = sample(c(2:n.colony), size = rpois(1, 0.000004))
  
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


# Number of Breeders and Prebreeders at year 1 ----------------------------

B1 = c(4000, 1000, 1000, 1000, 3700)
repro_ratio = c(0.34/0.66, rep(0.14/0.86,n.colony-1))  
N1 = round(repro_ratio * B1)



#  Simulate survey dataset ------------------------------------------------

N = matrix(NA, nrow = n.colony, ncol = n.years)
B = matrix(NA, nrow = n.colony, ncol = n.years)

#Initialization

N[,1] = N1
B[,1] = B1

#Next generation
for (t in 1:(n.years-1)) {
  for (s in 1:n.colony) {
    
    # Metapopulation dynamics equation
    N[s,t+1] <- B[s,t] * rho[col2colclass[s]] * phi[1]  +
      N[s,t] * phi[2] * sum(ETA_t[[t]][s,1:n.colony] * (1-kappa[col2colclass]))
    
    B[s,t+1] = sum(N[1:n.colony,t] * phi[2] * ETA_t[[t]][1:n.colony,s] * kappa[col2colclass[s]])  +
      sum(B[1:n.colony,t] * phi[2] * NU_t[[t]][1:n.colony,s]) 
    
  }
  N = round(N)
  B = round(B)
}



df <- data.frame(
  Temps = rep(seq_len(ncol(B)), each = nrow(B)),
  
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(B)),
  Valeur = as.vector(B))

p1 = ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Effectifs des 5 populations - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

p1



df <- data.frame(
  Temps = rep(seq_len(ncol(N)), each = nrow(N)),
  
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(N)),
  Valeur = as.vector(N))

p2 = ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Effectifs des 5 populations - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

#p2


library(ggplot2)

# Model parameters --------------------------------------------------------

set.seed(452)

n.colony = 5 # Number of colonies
n.years = 20 # Number of years
years = 1985:(1985+n.years-1) # Studied years

n.state = 3 * n.colony + 1


# Apparent survival probability - age class dependence
phi1 = 0.184 # First year
phiA = 0.743 # Subadult and adult
phi = c(phi1, phiA)

# Productivity
rhoLR = 2.1 # La Ronze
rhoSAT = 1.6 # Satellite colonies
rho = c(rhoLR, rhoSAT)

# Recruitment probability
kappaLR = 0.54 # La Ronze
kappaSAT = 0.69 # Satellite colonies
kappa = c(kappaLR, kappaSAT)

# correspondence from colony to colony type i.e. La Ronze ou satellite colony
col2colclass = c(1, rep(2, (n.colony-1)))

# Natal dispersion from one colony to another - colony dependence
# Quite random & few natal dispersion toward La Ronze
eta = matrix(c(
  0.82, 0.03, 0.05, 0.01, 0.06,
  0.25, 0.36, 0.11, 0.04, 0.22,
  0.25, 0.11, 0.33, 0.10, 0.19,
  0.29, 0.09, 0.09, 0.33, 0.19,
  0.21, 0.11, 0.22, 0.15, 0.29), 
  nrow = n.colony, ncol = n.colony, byrow = T)

# rowSums(eta)

# Breeding dispersion from one colony to another - colony dependence
# Great fidelity and attractiveness ofLa Ronze 
nu = matrix(c(
  0.97, 0.01, 0.01, 0.01, 0.01,
  0.06, 0.49, 0.09, 0.02, 0.33,
  0.06, 0.08, 0.30, 0.05, 0.50,
  0.24, 0.08, 0.10, 0.40, 0.18,
  0.01, 0.01, 0.01, 0.01, 0.96), 
  nrow = n.colony, ncol = n.colony, byrow = T)

# rowSums(nu)

# Extinction year
ETA_t = list()
NU_t = list()

for (t in 1:(n.years-1)) {
  
  extinted_col = sample(c(2:n.colony), size = rpois(1, 0.000004))
  
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


# Number of Breeders and Prebreeders at year 1 ----------------------------

B1 = c(4000, 1000, 1000, 1000, 3700)
repro_ratio = c(0.34/0.66, rep(0.14/0.86,n.colony-1))  
N1 = round(repro_ratio * B1)



#  Simulate survey dataset ------------------------------------------------

N = matrix(NA, nrow = n.colony, ncol = n.years)
B = matrix(NA, nrow = n.colony, ncol = n.years)

#Initialization

N[,1] = N1
B[,1] = B1

#Next generation
for (t in 1:(n.years-1)) {
  for (s in 1:n.colony) {
    
    # Metapopulation dynamics equation
    N[s,t+1] <- B[s,t] * rho[col2colclass[s]] * phi[1]  +
      N[s,t] * phi[2] * sum(ETA_t[[t]][s,1:n.colony] * (1-kappa[col2colclass]))
    
    B[s,t+1] = sum(N[1:n.colony,t] * phi[2] * ETA_t[[t]][1:n.colony,s] * kappa[col2colclass[s]])  +
      sum(B[1:n.colony,t] * phi[2] * NU_t[[t]][1:n.colony,s]) 
    
  }
  N = round(N)
  B = round(B)
}



df <- data.frame(
  Temps = rep(seq_len(ncol(B)), each = nrow(B)),
  
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(B)),
  Valeur = as.vector(B))

p1_ = ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Effectifs des 5 populations - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

p1_



df <- data.frame(
  Temps = rep(seq_len(ncol(N)), each = nrow(N)),
  
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(N)),
  Valeur = as.vector(N))

p2 = ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Effectifs des 5 populations - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

#p2


p1 + p1_

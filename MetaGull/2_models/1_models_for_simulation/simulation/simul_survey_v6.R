
source("simulation/model_parameters_v9.R")


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


# Ajout de NA
B[4,5] = NA
B[3,6] = NA
B[4,6] = NA


library(ggplot2)

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

# p2





library(tidyverse)
library(ggplot2)
library(MCMCvis)

source("model_parameters_v6.R")
load(file = "B_simulation.Rda")
B = B_simulation
load(file = "N.Rda")
load(file = "out1_integrated_v10_4.Rda")
load(file = "out1_integrated_v10_4_long.Rda")
load(file = "out1_integrated_v10_6_simul.Rda")


GV = c(phi1, phiA, rhoLR, rhoSAT, kappaLR, kappaSAT)


MCMCtrace(out1, params = c("phi",
                           "rho",
                           "kappa"),
         # gvals = GV,
          Rhat = TRUE,
          ind = TRUE, pdf = FALSE)

out1$mean$eta_monitored
out1$mean$nu_monitored

GV = eta %>% as.vector()

MCMCtrace(out1, params = c("eta_monitored"),
          gvals = GV,
          Rhat = TRUE,
          ind = TRUE, pdf = FALSE)

# La 5 ligne n'est pas identifiable et il y a du mal avec les 0


GV = nu %>% as.vector()

MCMCtrace(out1, params = c("nu_monitored"),
          gvals = GV,
          Rhat = TRUE,
          ind = TRUE, pdf = FALSE)



#MCMCtraceeta_monitored#MCMCtrace(out1, params = c("po"), ind = TRUE, pdf = FALSE)

# je vais lancer un modele avec des eta[5, ] = 0.2, voire comment cela améliore



out1$mean$po[6,] %>% 
  as.data.frame() %>%
  mutate(., val = .) %>% 
  ggplot(., aes(x =val)) +
  geom_histogram() +
  labs(title = "Histogramme avec ggplot2", x = "Valeurs", y = "Fréquence")


out1$mean$po[7:9,] %>% 
  as.vector() %>% 
  as.data.frame() %>%
  mutate(., val = .) %>% 
  ggplot(., aes(x =val)) +
  geom_histogram()




df <- data.frame(
  Temps = rep(seq_len(ncol(B)), each = nrow(B)),
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(B)),
  Valeur = as.vector(B))

ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Breeders - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()


df <- data.frame(
  Temps = rep(seq_len(ncol(N)), each = nrow(N)),
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(N)),
  Valeur = as.vector(N))

ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = " Prebreeders - données simulées",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()




data_ = t(out1[["mean"]][["B"]])

df <- data.frame(
  Temps = rep(seq_len(nrow(data_)), time = ncol(data_)),
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), each = nrow(data_)),
  Valeur = as.vector(data_))

ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Breeders - d'après l'analyse",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

data_ = t(out1[["mean"]][["N"]])

df <- data.frame(
  Temps = rep(seq_len(nrow(data_)), time = ncol(data_)),
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), each = nrow(data_)),
  Valeur = as.vector(data_))

ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Prebreeders - d'après l'analyse",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()


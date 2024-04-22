

library(MCMCvis)


MCMCtrace(out1, params = c("phi",
                           "rho",
                           "kappa"),
          Rhat = TRUE,
          ind = TRUE, pdf = FALSE)


MCMCtrace(out1, params = c("rho",
                           "kappa"), ind = TRUE, pdf = FALSE)

out1$mean$eta_monitored

out1$mean$nu_monitored

MCMCtrace(out1, params = c("eta_monitored"), ind = TRUE, pdf = FALSE)

#MCMCtraceeta_monitored#MCMCtrace(out1, params = c("po"), ind = TRUE, pdf = FALSE)


mean(out1$mean$po[6,])
mean(out1$mean$po[7:10,])

#out1$sd

library(ggplot2)

load(file = "simulated_dataset_v9/B.Rda")

df <- data.frame(
  Temps = rep(seq_len(ncol(B)), each = nrow(B)),
  Population = rep(c("pop1", "pop2", "pop3", "pop4", "pop5"), ti = ncol(B)),
  Valeur = as.vector(B))

ggplot(df, aes(x = Temps, y = Valeur, color = Population)) +
  geom_line() +
  labs(title = "Effectifs des 5 populations - données simulées",
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
  labs(title = "Effectifs des 5 populations - d'après l'analyse",
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
  labs(title = "Effectifs des 5 populations - d'après l'analyse",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()


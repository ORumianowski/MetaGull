setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

# dataset cleaning minimal v2
# pret à l envoi

# !!! : quand je fais un subset des données et des manipulations de données importantes 

library(jagsUI)
library(tidyverse)
library(IPMbook)

#path_dataset_gull = "/lustre/rumianowskio/dataset_110324/histoire_capture_kg.csv"
path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_110324/histoire_capture_kg.csv"


dgull = read.csv(path_dataset_gull)

dgull = dgull %>%
  dplyr::select(-c("type_controle_mesure", "nom_site_principal", "nom_site_mesure")) %>%
  mutate(id_mesure = id_mesure %>% as.factor(),
         id_individu = id_individu %>% as.factor(),
         date_mesure = date_mesure %>% as.POSIXct(., tz = "UTC"),
         id_site_principal = id_site_principal %>% as.factor(),
         type_controle_simple = type_controle_simple %>% as.factor(),
         age = age %>% as.factor(),
         repro_statut = repro_statut %>% as.factor()) %>%
  arrange(date_mesure)

str(dgull)
summary(dgull)

# !!! - les adultes aux filets n'apportent pas d'information - leurs baguages permettent les relectures futures
dgull = subset(dgull, source != "filet")

dgull[dgull$age=="NULL", "age"] <- "A"

dgull = dgull %>%
  mutate(annee = year(date_mesure))


dgull = dgull %>% 
  # !!! Comme Péron
  subset(.,id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E")) %>% 
  subset(., annee %in% 1986:2005)

# Filtering for adult and LaRonze -----------------------------------------
# Pour le CJS simple sur LaRonze

dgull = dgull %>%
  # !!! Pour le CJS simple
  subset(., age %in% c("A"))%>% 
  subset(.,id_site_principal %in% c("LR_E")) 


# 1 - ID   -------------------------------------------------------------

dgull_1 = dgull %>%
  # !!! On ne peut construire de CH sans identifiant
  # ind. with id.
  subset(., id_individu != "NULL") 

dgull_NULL = dgull %>%
  # ind. with id.
  subset(., id_individu == "NULL") 


# Cela correspond à une perte consequante - biaisé ? vieux individu plus difficilement lu parce que moins de darvic ?
table(dgull_NULL$annee)
table(dgull_1$annee)

# One encounter per year --------------------------------------------------

# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

# Ici, pour simplifier, je prends directement le premier contacte - le site n'a pas d'effet sur la survie

dgull_2_2 <- dgull_1 %>%
  group_by(id_individu, year(date_mesure)) %>%
  mutate(
    first_contact = if_else(row_number() == 1, "ok", NA_character_)
  ) %>%
  ungroup() %>%
  mutate(
    selected_contact = case_when(
      !is.na(first_contact) ~ "ok",
      TRUE ~ NA_character_
    )
  )


dgull_2_3 = dgull_2_2 %>%  
  # !!! a priori, pas d 'effet ??
  subset(.,  !is.na(selected_contact))%>%
  mutate(annee = year(date_mesure))



dgull_final = dgull_2_3



# Numbering the sites -----------------------------------------------------

dgull_final = dgull_final %>% 
  dplyr::select(id_individu,
                id_site_principal,
                annee,
                age,
                repro_statut)

dgull_final$site_int =  dgull_final$id_site_principal %>% as.integer()

colony_int_list = unique(dgull_final$site_int)
n.colony = length(colony_int_list)

table_correspondance <- data.frame(
  ancienne_valeur = as.integer(colony_int_list),
  nouvelle_valeur = as.integer(1:n.colony)
)

dgull_final <- dgull_final %>%
  mutate(site= recode(site_int, !!!setNames(as.character(table_correspondance$nouvelle_valeur), table_correspondance$ancienne_valeur))) %>% 
  mutate(site = as.integer(site))

#  Formatting in Capture Histories ------------------------------------------

dgull_final <- dgull_final %>% 
  dplyr::select(id_individu,
                annee,
                site,
                age,
                repro_statut)


dgull_final <- dgull_final %>% 
  dplyr::select(id_individu,
                annee)

# Constants

ind_list = unique(dgull_final$id_individu) # ID
nb_ind = length(ind_list) # number of ID

year_list = min(dgull_final$annee):max(dgull_final$annee) # years with survey
nb_year = length(year_list) # number of survey years

CH = matrix(NA, nrow = nb_ind, ncol = nb_year) 

# !!! : la construiction du CH semble correct ?

for (i in 1:nb_ind){
  
  data_i = subset(dgull_final, id_individu == ind_list[i] )
  year_ind_list =  unique(data_i$annee)
  
  for(j in which(year_list==year_ind_list[1]):nb_year){
    year = year_list[j]
    # Not detected
    if (!(year %in% year_ind_list)){
      CH[i,j] = 0 
    }
    # If detected
    else{
      CH[i,j] =  1
    }
  }
}

CH[is.na(CH)] = 0

y = CH

# Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(CH, 1, get.first)


# Data bundle
jags.data <- list(y=y, f=f, nind=nrow(y), nyears=ncol(y))
str(jags.data)


# Write JAGS model file
cat(file="model14.txt", "
model {
# Priors and linear models
phi.const ~ dunif(0, 1) # Vague prior for constant phi
p.const ~ dunif(0, 1) # Vague prior for constant p
for (i in 1:nind){ # Loop over individuals
for (t in f[i]:(nyears-1)){ 
phi[i,t] <- phi.const 
p[i,t] <- p.const 
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
parameters <- c("phi.const", "p.const") 
# MCMC settings
ni <- 3000; nb <- 1000; nc <- 1; nt <- 1; na <- 1000
# Call JAGS from R (ART < 1 min) and check convergence
out17 <- jags(jags.data, inits, parameters, "model14.txt", n.iter=ni, n.burnin=nb, n.chains=nc,
              n.thin=nt, n.adapt=na, parallel=TRUE)


out17$mean

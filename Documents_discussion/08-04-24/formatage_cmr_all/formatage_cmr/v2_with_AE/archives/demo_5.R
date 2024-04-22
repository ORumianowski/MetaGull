setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

# dataset cleaning minimal v3

# 

# !!! : quand je fais un subset des données et des manipulations de données importantes 

library(jagsUI)
library(tidyverse)
library(IPMbook)

#path_dataset_gull = "/lustre/rumianowskio/dataset_110324/histoire_capture_kg.csv"
path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_29032024/Affut.xlsx"

dgull <- read_excel(path_dataset_gull)


dgull = dgull %>%
  dplyr::select(c("ind", "date", "et")) %>%
  mutate(ind = ind %>% as.factor(),
         date = date %>% as.POSIXct(., tz = "UTC")) %>%
  arrange(date)%>%
  mutate(annee = year(date))

str(dgull)
summary(dgull)

table(dgull$et)


dgull = dgull %>% 
  # !!!
  subset(.,et %in% c("LR", "MA", "PC", "V5")) %>% 
  subset(., annee %in% 1986:2005)

dgull = dgull %>% 
  na.omit()


# One encounter per year --------------------------------------------------

# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

# Ici, j'ai simplidié en prenant directement la première

dgull_2_2 <- dgull %>%
  group_by(ind, year(date)) %>%
  mutate(
    first_contact = if_else(row_number() == 1, "ok", NA_character_)
  ) %>%
  ungroup() 


dgull_2_3 = dgull_2_2 %>%  
  # !!!
  subset(.,  !is.na(first_contact))%>%
  mutate(annee = year(date))


#   -----------------------------------------------------------------------


dgull_final = dgull_2_3

dgull_final <- dgull_final %>% 
  dplyr::select(ind,
                annee)

# Constants

ind_list = unique(dgull_final$ind) # ID
nb_ind = length(ind_list) # number of ID

year_list = min(dgull_final$annee):max(dgull_final$annee) # years with survey
nb_year = length(year_list) # number of survey years

dgull_final <- dgull_final %>%
  mutate(num_annee = recode(annee, !!!setNames(as.character(1:length(year_list)), year_list))) %>% 
  mutate(num_annee = as.integer(num_annee))

CH = matrix(NA, nrow = nb_ind, ncol = nb_year) 


for (i in 1:nb_ind){
  
  data_i = subset(dgull_final, ind == ind_list[i] )
  year_ind_list =  unique(data_i$num_annee)
  
  for(j in 1:nb_year){
    
    # If detected
    if (j %in% year_ind_list){
      CH[i,j] = 1 
    }
    # If not detected
    else{
      CH[i,j] = 0
    }
  }
}


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
ni <- 3000; nb <- 700; nc <- 3; nt <- 10; na <- 300
# Call JAGS from R (ART < 1 min) and check convergence
out17 <- jags(jags.data, inits, parameters, "model14.txt", n.iter=ni, n.burnin=nb, n.chains=nc,
              n.thin=nt, n.adapt=na, parallel=TRUE)

out17$mean

library(MCMCvis)

MCMCtrace(out17, params = c("phi.const",
                           "p.const"),
          Rhat = TRUE,
          ind = TRUE, pdf = FALSE)

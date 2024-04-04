
setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------

load(file = "1_formatage/formatage_cmr/v2/dgull_minimal2.Rda")

# Select colonies and years ---------------------------------------------------------

#dgull_final = dgull_minimal
dgull_final = dgull_minimal2

dgull_final = dgull_final %>% 
  subset(.,id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E")) %>% 
  subset(., annee %in% 1986:2005)


# Filtering for adult and LaRonze -----------------------------------------

dgull_final = dgull_final %>% 
  subset(., age=="A") %>% 
  subset(., id_site_principal == "LR_E")


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

#save(CH, file = "CH.Rda")

# load(file = "CH.Rda")

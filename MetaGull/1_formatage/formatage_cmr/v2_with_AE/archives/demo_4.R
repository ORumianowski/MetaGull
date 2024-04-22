setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

# dataset cleaning minimal v2

# !!! : quand je fais un subset des données et des manipulations de données importantes 

library(jagsUI)
library(tidyverse)
library(IPMbook)

#path_dataset_gull = "/lustre/rumianowskio/dataset_110324/histoire_capture_kg.csv"
path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_28032024/histoire_capture2.csv"
# path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_11032024/histoire_capture_kg.csv"
# 
# dgull <- read_excel("C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_29032024/Affut.xlsx")

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
  arrange(date_mesure)%>%
  mutate(annee = year(date_mesure))

str(dgull)
summary(dgull)


dgull[dgull$age=="NULL", "age"] <- "A"

dgull = dgull %>%
  # !!!
  subset(.,id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E") | age == "P")


dgull = dgull %>% 
  subset(., annee %in% 1986:2005)

dgull_all = dgull

dgull_all_2 = dgull %>% 
  subset(., type_controle_simple == "B")

dgull_all$id_individu %>% 
  unique() %>% 
  length()

# !!!
dgull = subset(dgull, source != "filet")

subset(dgull, source != "filet")

# Filtering for adult and LaRonze -----------------------------------------
# Pour le CJS simple sur LaRonze

dgull = dgull %>%
  # !!!
  subset(., age %in% c("A"))
#%>% 
  #subset(.,id_site_principal %in% c("LR_E")) 


# 1 - ID   -------------------------------------------------------------

dgull_1 = dgull %>%
  # !!!
  # ind. with id.
  subset(., id_individu != "NULL") 


  

dgull_NULL = dgull %>%
  # ind. with id.
  subset(., id_individu == "NULL") 

table(dgull_NULL$annee)
table(dgull_1$annee)



# Les ind. enlevés sont plus vieux que la moyennes

# age moy des deux 


# One encounter per year --------------------------------------------------

# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

dgull_2_2 <- dgull_1 %>%
  group_by(id_individu, year(date_mesure)) %>%
  mutate(
    first_contact = if_else(row_number() == 1, "ok", NA_character_)
  ) %>%
  ungroup() 



dgull_2_3 = dgull_2_2 %>%  
  # !!!
  subset(.,  !is.na(first_contact))%>%
  mutate(annee = year(date_mesure))


#   -----------------------------------------------------------------------


dgull_final = dgull_2_3


dgull_final <- dgull_final %>% 
  dplyr::select(id_individu,
                annee)



# Constants

ind_list = unique(dgull_final$id_individu) # ID
nb_ind = length(ind_list) # number of ID

year_list = min(dgull_final$annee):max(dgull_final$annee) # years with survey
nb_year = length(year_list) # number of survey years

dgull_final <- dgull_final %>%
  mutate(num_annee = recode(annee, !!!setNames(as.character(1:length(year_list)), year_list))) %>% 
  mutate(num_annee = as.integer(num_annee))

CH = matrix(NA, nrow = nb_ind, ncol = nb_year) 



nrow(CH)



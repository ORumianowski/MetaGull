setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

# Process minimal


library(tidyverse)

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


dgull = subset(dgull, source != "filet")

dgull[dgull$age=="NULL", "age"] <- "A"

dgull = dgull %>%
  subset(., age %in% c("A"))%>% 
  subset(.,id_site_principal %in% c("LR_E")) 

dgull = dgull %>%
  mutate(annee = year(date_mesure))


dgull = dgull %>% 
  subset(.,id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E")) %>% 
  subset(., annee %in% 1986:2005)



# 1 - ID   -------------------------------------------------------------

dgull_1 = dgull %>%
  # ind. with id.
  subset(., id_individu != "NULL") 

dgull_NULL = dgull %>%
  # ind. with id.
  subset(., id_individu == "NULL") 

table(dgull_NULL$annee)
table(dgull_1$annee)



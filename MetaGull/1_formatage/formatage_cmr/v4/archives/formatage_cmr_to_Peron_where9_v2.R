
setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)
library(readxl)

# Dataset de Killian

path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_28032024/histoire_capture.csv"

dgull0 = read.csv(path_dataset_gull)

dgull0 = dgull0 %>%
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

# Dataset affut

# path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_29032024/Affut.xlsx"
# 
# dgull0 <- read_excel(path_dataset_gull)
# 
# 
# dgull0 = dgull0 %>% 
#   dplyr::select(c("ind", "date", "et", "controle")) %>% 
#   mutate(ind = ind %>%  as.factor(),
#          date = date %>%  as.POSIXct(., tz = "UTC")) %>% 
#   arrange(date)%>% 
#   mutate(annee = year(date))

# Summary du dataset brut

str(dgull0)
summary(dgull0)


# Filtre sur la période ---------------------------------------------------

dgull0 = dgull0 %>%  
  subset(., annee %in% 1980:2005)


# Integration des contacts d'adultes capturés au filets -------------------

filet = subset(dgull0, source == "filet")
dgull = subset(dgull0, source != "filet")


# Formatages préliminaires -------------------------------------------------

dgull = dgull0

# Les individus avec un age NULL sont des adultes par défauts
#dgull[dgull$age=="NULL", "age"] <- "A"

# dgull = dgull %>% 
#   subset(., age %in% c("P", "A"))

# Les autres colonies sont regroupeés en AliveElsewhere (AE)
dgull = dgull %>% 
  mutate(site_ch = if_else(id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E"), id_site_principal, "AE"))

# 1 - ID   -------------------------------------------------------------

dgull_1 = dgull %>% 
  # ind. with id.
  subset(., id_individu != "NULL") 


dgull_1 %>% 
  #subset(., site_ch == "AE") %>% 
  subset(., age == "P") %>% 
  select(site_ch, annee) %>% 
  table()

dgull_1 %>% 
  subset(., site_ch == "AE") %>% 
  pull(age) %>% 
  table()


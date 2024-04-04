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

# One encounter per year --------------------------------------------------

dgull_1$annual_encounter_occasion = NA

dgull_2_1 <- dgull_1 %>%
  group_by(id_individu, year(date_mesure)) %>%
  mutate(annual_encounter_occasion = row_number())


# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

dgull_2_2 <- dgull_2_1 %>%
  group_by(id_individu, year(date_mesure)) %>%
  mutate(
    # 1 - The first contact with a reproductive status
    first_repro_contact = if_else(repro_statut == "R", "ok1", NA_character_),
    # 2 - The first contact
    first_contact = if_else(row_number() == 1, "ok2", NA_character_)
  ) %>%
  ungroup() %>%
  mutate(
    selected_contact = case_when(
      !is.na(first_repro_contact) ~ "ok1",
      !is.na(first_contact) ~ "ok2",
      TRUE ~ NA_character_
    )
  )



dgull_2_3 = dgull_2_2 %>%  
  subset(.,  !is.na(selected_contact))%>%
  mutate(annee = year(date_mesure))


dgull_minimal2 = dgull_2_3

save(dgull_minimal2, file = "1_formatage/formatage_cmr/v2/dgull_minimal2.Rda")

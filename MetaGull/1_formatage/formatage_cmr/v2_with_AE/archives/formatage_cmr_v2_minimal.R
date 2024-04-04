setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

# Process minimal


library(tidyverse)

#path_dataset_gull = "/lustre/rumianowskio/dataset_110324/histoire_capture_kg.csv"
# path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_110324/histoire_capture_kg.csv"
# 
# 
# dgull = read.csv(path_dataset_gull)
# 
# dgull = dgull %>%
#   dplyr::select(-c("type_controle_mesure", "nom_site_principal", "nom_site_mesure")) %>%
#   mutate(id_mesure = id_mesure %>% as.factor(),
#          id_individu = id_individu %>% as.factor(),
#          date_mesure = date_mesure %>% as.POSIXct(., tz = "UTC"),
#          id_site_principal = id_site_principal %>% as.factor(),
#          type_controle_simple = type_controle_simple %>% as.factor(),
#          age = age %>% as.factor(),
#          repro_statut = repro_statut %>% as.factor()) %>%
#   arrange(date_mesure)
# 
# str(dgull)
# summary(dgull)
# 
# 
# dgull = subset(dgull, source != "filet")
# 
# 
# 
# # 1 - ID   -------------------------------------------------------------
# 
# dgull_1 = dgull %>%
#   # ind. with id.
#   subset(., id_individu != "NULL") %>%
#   # ind. with at least one ring
#   subset(., !(metal1_lue_mesure=="NULL" &  metal2_lue_mesure=="NULL" & darvic_lue_mesure=="NULL" & combinaison_lue_mesure=="NULL"))
# 
# # NB: les "PNB" n'ont pas d'identifiant (id_individu = "NULL") et sont donc éliminés par cette étape
# 
# # two individuals for one ring (metal1_lue_mesure)
# id_list <- dgull_1 %>%
#   subset(., metal1_lue_mesure != "NULL") %>%
#   group_by(metal1_lue_mesure) %>%
#   filter(n() > 1) %>%
#   filter(length(unique(id_individu)) > 1) %>%
#   pull(metal1_lue_mesure) %>%
#   unique()
# 
# dgull_1 = dgull_1 %>%
#   # remove these ind. (metal1_lue_mesure)
#   subset(., !(metal1_lue_mesure  %in% id_list))
# 
# # two individuals for one ring  (metal2_lue_mesure)
# id_list <- dgull_1 %>%
#   subset(., metal2_lue_mesure != "NULL") %>%
#   group_by(metal2_lue_mesure) %>%
#   filter(n() > 1) %>%
#   filter(length(unique(id_individu)) > 1) %>%
#   pull(metal2_lue_mesure) %>%
#   unique()
# 
# 
# dgull_1 = dgull_1 %>%
#   # remove these ind. (metal2_lue_mesure)
#   subset(., !(metal2_lue_mesure  %in% id_list))
# 
# 
# # two individuals for one ring (darvic_lue_mesure)
# id_list <- dgull_1 %>%
#   subset(., darvic_lue_mesure != "NULL") %>%
#   group_by(darvic_lue_mesure) %>%
#   filter(n() > 1) %>%
#   filter(length(unique(id_individu)) > 1) %>%
#   pull(darvic_lue_mesure) %>%
#   unique()
# 
# dgull_1 = dgull_1 %>%
#   # remove these ind. (darvic_lue_mesure)
#   subset(., !(darvic_lue_mesure  %in% id_list))
# 
# save(dgull_1, file = "/lustre/rumianowskio/dgull_1.Rda")
# # load(file = "dgull_1.Rda")
# 
# 
# # One encounter per year --------------------------------------------------
# 
# dgull_1$annual_encounter_occasion = NA
# 
# # Anotation of the order of occurrence of intra-annual data
# for (ind in unique(dgull_1$id_individu)){
#   dind = subset(dgull_1, id_individu == ind)
#     for (cyear in year(dind$date_mesure)){
#       dind_year = subset(dind, year(date_mesure) == cyear)
#       for (cdate in 1:nrow(dind_year)){
#         dgull_1[(dgull_1$id_individu == ind & year(dgull_1$date_mesure) == cyear), c("annual_encounter_occasion")][cdate] = cdate
#       }
#     }
# }
# 
# dgull_2_1 = dgull_1
# 
# save(dgull_2_1, file = "/lustre/rumianowskio/dgull_2_1.Rda")
# # load(file = "dgull_2_1.Rda")
# 
# 
# # Selection of one encounter per year
# # Hierarchical criteria :
# # 1 - The first contact with a reproductive status
# # 2 - The first contact
# 
# dgull_2_1$selected_contact = NA
# 
# for (ind in unique(dgull_2_1$id_individu)){
#   dind = subset(dgull_2_1, id_individu == ind)
#     for (cyear in unique(year(dind$date_mesure))){
#       dind_year = subset(dind, year(date_mesure) == cyear)
# 
#       # 1 - The first contact with a reproductive status
#       mesure_repro = dind_year %>%
#         filter(repro_statut == "R")  %>%
#         pull(id_mesure) %>%
#         head(., 1)
# 
#       if (length(mesure_repro)>0){
#         dgull_2_1[dgull_2_1$id_mesure==mesure_repro,"selected_contact"] = "ok1"
#       }
# 
#       # 2 - The first contact
#       else{
#         dgull_2_1[dgull_2_1$id_individu == ind &  year(dgull_2_1$date_mesure) == cyear & dgull_2_1$annual_encounter_occasion==1,"selected_contact"] = "ok2"
# 
#       }
#     }#cyear
# }#ind
# 
# dgull_2_2 = dgull_2_1
# 
# save(dgull_2_2, file = "/lustre/rumianowskio/dgull_2_2.Rda")
# # load(file = "dgull_2_2.Rda")
# 
# dgull_2_3 = dgull_2_2 %>%
#   subset(.,  !is.na(selected_contact))%>%
#   mutate(annee = year(date_mesure))
# 
# save(dgull_2_3, file = "/lustre/rumianowskio/dgull_2_3.Rda")



load(file = "1_formatage/formatage_cmr/v2/dgull_2_3.Rda")



dgull_5 = dgull_2_3


dgull_5[dgull_5$age=="NULL", "age"] <- "A"

dgull_5 = dgull_5 %>%
  subset(., age %in% c("P", "A"))


dgull_minimal = dgull_5

save(dgull_minimal, file = "1_formatage/formatage_cmr/v2/dgull_minimal.Rda")

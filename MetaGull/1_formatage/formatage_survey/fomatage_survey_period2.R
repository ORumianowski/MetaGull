setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)
library(ggplot2)
library(readxl)

path_dataset_survey1 = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/Peron_dataset_0502024/public-colonie.csv"

survey1 = read.csv(path_dataset_survey1) %>% 
  dplyr::select(annee_observation_colonie,
                id_site_mesure,
                nombre_couple_colonie) %>% 
  rename(year = annee_observation_colonie,
         site = id_site_mesure,
         N = nombre_couple_colonie)

path_dataset_survey2 <- "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/survey_post_2008_150424/colonie.xlsx"

survey2 = read_excel(path_dataset_survey2) %>% 
  dplyr::select(annee,
                etang, 
                nombre_couple) %>% 
  rename(year = annee,
         site = etang,
         N = nombre_couple) %>% 
  mutate(site = case_when(
    site == "LR" ~ "LR_E",
    site == "MA" ~ "MA_E",
    site == "LV" ~ "PC_E",
    site == "VS" ~ "VS_E",
    site == "LA" ~ "LA_E",
    site == "SV" ~ "SV_E",
    site == "WV" ~ "WV_E",
    site == "SZ" ~ "SZ_E",
    
    TRUE ~ site)
    )


site_period1 = c("LR_E", "MA_E", "PC_E", "V5_E")

site_period2 = c("LR_E", "MA_E", "PC_E", "V5_E", 
                 "VS_E", "LA_E","SV_E", "WV_E", "SZ_E")


# Filtre sur la période et les colonies ---------------------------------------------------

survey_p1 = survey1 %>%  
  subset(., year %in% 1986:2006)%>% 
  mutate(site = if_else(site %in% site_period1, site, "AE"))

survey_p1_CMR = survey_p1 %>%
  filter(!(site == "AE"))

survey_p1_AE = survey_p1 %>%
  filter(site == "AE") %>%
  group_by(year) %>%
  summarize(N = sum(N)) %>% 
  mutate(site = "AE")

survey_p1 = rbind(survey_p1_CMR, survey_p1_AE) 

survey_p1

survey_p2 = survey1 %>%  
  subset(., year %in% 2007:2008)%>% 
  mutate(site = if_else(site %in% site_period2, site, "AE"))

survey_p2_CMR = survey_p2 %>%
  filter(!(site == "AE"))

survey_p2_AE = survey_p2 %>%
  filter(site == "AE") %>%
  group_by(year) %>%
  summarize(N = sum(N)) %>% 
  mutate(site = "AE")

survey_p2 = rbind(survey_p2_CMR, survey_p2_AE)
survey_p2

survey_p3 = survey2 %>%  
  subset(., year %in% 2009:2019)%>% 
  mutate(site = if_else(site %in% site_period2, site, "AE"))

survey_p3_CMR = survey_p3 %>%
  filter(!(site == "AE"))

survey_p3_AE = survey_p3 %>%
  filter(site == "AE") %>%
  group_by(year) %>%
  summarize(N = sum(N)) %>% 
  mutate(site = "AE")

survey_p3 = rbind(survey_p3_CMR, survey_p3_AE) 

survey = rbind(survey_p1, survey_p2, survey_p3)%>%
  pivot_wider(names_from = year, values_from = N) 

survey = rbind( filter(survey, site == "LR_E"),
                 filter(survey, site == "MA_E"),
                 filter(survey, site == "PC_E"),
                 filter(survey, site == "V5_E"),
                 filter(survey, site == "VS_E"),
                 filter(survey, site == "LA_E"),
                 filter(survey, site == "SV_E"),
                 filter(survey, site == "WV_E"),
                 filter(survey, site == "SZ_E"),
                 filter(survey, site == "AE")
               )



survey[, 23:35][is.na(survey)[, 23:35]] = 0

# -------------------------------------------------------------------------

survey = survey %>% 
  select(-site)

B = survey %>% 
  as.matrix()

# Nombre de couple issu de la thèse de V.GROBOIS
B[1,1:20] = c(7145, 8059, 5585, 7893, 8487, 10428, 10877, 7310, 5882, 6027, 6584, 8499, 9225, 6639, 7000, 7000, 7000, 7000, 7000, 7000)%/%2

# Année d'inction issu de Péron
B[3, c(9, 19:20)] = 0
B[2, c(7, 17)] = 0
B[4, c(1:5, 7:10, 16:20)] = 0


# Manip perso !!

B[1,21]  = 3500

#save(B, file = "1_formatage/real_dataset/v1_AE/B.Rda")


# Plot --------------------------------------------------------------------

data_ = as.matrix(B)

df <- data.frame(
  Temps = rep(1986:2019, each = nrow(data_)),
  Population = rep(c("LR_E", "MA_E", "PC_E", "V5_E", 
                     "AE",
                     "VS_E", "LA_E","SV_E", "WV_E", "SZ_E"
                     ), time = ncol(data_)),
  Valeur = as.vector(data_))


ggplot(df)+
  geom_line(aes(x = Temps, y = Valeur, color = Population)) +
  geom_point(aes(x = Temps, y = Valeur, color = Population)) +
  labs(title = "Effectifs des 5 populations - données réelles",
       x = "Temps",
       y = "Nombre d'individus") +
  theme_minimal()

save(B, file = "B_P2_v2.Rda")



setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)
library(readxl)
library(tidyr)


# Dataset de Killian ------------------------------------------------------

load("1_formatage/real_dataset/v1_AE/CH_AE2.Rda")


my2peron <- function(colonne) {
  
  colonne <- case_when(
    colonne == 0 ~ 0, # not detected
    colonne == 6 ~ 1, # breeders LR
    colonne == 7 ~ 2, # breeders MA
    colonne == 9 ~ 3, # breeders V5
    colonne == 8 ~ 4, # breeders VR
    colonne == 1 ~ 5, # Nestling LR
    colonne == 2 ~ 6, # Nestling MA
    colonne == 4 ~ 7, # Nestling V5
    colonne == 3 ~ 8, # Nestling VE
    colonne == 5 ~ 9, # Nestling AE
    
    TRUE ~ NA
  )
  return(colonne)
}

CH2 <- CH %>%
  as.data.frame() %>% 
  mutate_all(my2peron)

# Combinaison de toutes les colonnes en une seule colonne
df_combined <- CH2 %>%
  as.data.frame() %>% 
  mutate(combined = do.call(paste, c(., sep = ""))) %>%
  select(combined)

# Compter les occurrences de chaque ligne
my_ch_counts <- df_combined %>%
  count(combined)


my_ch_counts = my_ch_counts %>% 
  rename(ch = combined,
         my_N = n)


#save(my_ch_counts, file = "my_ch_counts_v2.Rda")



# Peron dataset -----------------------------------------------------------


peron_counts  <- read.csv("C:/Users/33763/Downloads/histo 5sites recrutement -clear.inp", header=FALSE)

peron_counts$V1 <- substr(peron_counts$V1, 1, nchar(peron_counts$V1) - 2)

peron_counts <- separate(peron_counts, V1, into = c("ch", "N"), sep = "  ", remove = FALSE) %>% 
  dplyr::select(ch, N) %>% 
  mutate(N = as.integer(N))




comp_counts = merge(peron_counts, my_ch_counts, by=("ch"), all = T) %>% 
  mutate(diff = N - my_N) %>% 
  arrange(diff)



sum(peron_counts$N)
sum(my_ch_counts$my_N)

peron_counts2 = separate(peron_counts, ch, into = as.character(1986:2005), sep = "", remove = FALSE)














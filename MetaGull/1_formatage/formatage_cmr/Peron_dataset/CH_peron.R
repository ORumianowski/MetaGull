
setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyr)
library(tidyverse)

peron_counts  <- read.csv("C:/Users/33763/Downloads/histo 5sites recrutement -clear.inp", header=FALSE)

peron_counts$V1 <- substr(peron_counts$V1, 1, nchar(peron_counts$V1) - 2)

peron_counts <- separate(peron_counts, V1, into = c("ch", "N"), sep = "  ", remove = FALSE) %>% 
  dplyr::select(ch, N) %>% 
  mutate(N = as.integer(N))

CH1 = peron_counts %>% 
  separate(., ch, into = as.character(c(0,1986:2005)), sep = "", remove = FALSE) %>% 
  select(-c(1:2))%>%
  mutate_at(vars(1:20), as.integer)


CH2 = CH1 %>%
  group_by(row_number()) %>%
  slice(rep(row_number(), N)) %>%
  ungroup() %>% 
  select(-c("N", "row_number()")) %>% 
  as.matrix()


#CH2[,1] %>% table()

peron_to_my_codification <- function(colonne) {

  colonne <- case_when(
    colonne == 0 ~ 0, # not detected
    colonne == 1 ~ 6, # breeders LR
    colonne == 2 ~ 7, # breeders MA
    colonne == 3 ~ 9, # breeders V5
    colonne == 4 ~ 8, # breeders VR
    colonne == 5 ~ 1, # Nestling LR
    colonne == 6 ~ 2, # Nestling MA
    colonne == 7 ~ 4, # Nestling V5
    colonne == 8 ~ 3, # Nestling VE
    colonne == 9 ~ 5, # Nestling AE

    TRUE ~ NA
  )
  return(colonne)
}

CH3 <- CH2 %>%
  as.data.frame() %>% 
  mutate_all(peron_to_my_codification)

n.colony = 5

y = CH3 # Capture Histories

CH_peron = CH3
save(CH_peron, file = "CH_peron_v3.Rda")


marr_peron = IPMbook::marray(y, unobs=(n.colony+1)) # convert capture history to marray # unobserved states: 10 (adults in AliveElsewhere) and 11 to 15 (prebreeders)

save(marr_peron, file = "marr_peron_v3.Rda")









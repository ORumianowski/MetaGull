setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)

path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_28032024/histoire_capture.csv"
#path_dataset_gull = "C:/Users/33763/Documents/CMRi/0_Github/Z_0_Dataset/dataset_29032024/histoire_capture.csv"

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

str(dgull0)
summary(dgull0)

dgull0 = dgull0 %>% 
  subset(., annee %in% 1986:2005)

filet = subset(dgull0, source == "filet")
#dgull = subset(dgull0, source != "filet")

dgull = dgull0

table(filet$id_site_principal)

dgull[dgull$age=="NULL", "age"] <- "A"

dgull = dgull %>%
  mutate(site_ch = if_else(id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E"), id_site_principal, "AE"))


# 1 - ID   -------------------------------------------------------------

dgull_1 = dgull %>%
  # ind. with id.
  subset(., id_individu != "NULL") %>%
  # ind. with at least one ring
  subset(., !(metal1_lue_mesure=="NULL" &  metal2_lue_mesure=="NULL" & darvic_lue_mesure=="NULL" & combinaison_lue_mesure=="NULL"))

# NB: les "PNB" n'ont pas d'identifiant (id_individu = "NULL") et sont donc éliminés par cette étape

# two individuals for one ring (metal1_lue_mesure)
id_list <- dgull_1 %>%
  subset(., metal1_lue_mesure != "NULL") %>%
  group_by(metal1_lue_mesure) %>%
  filter(n() > 1) %>%
  filter(length(unique(id_individu)) > 1) %>%
  pull(metal1_lue_mesure) %>%
  unique()

dgull_1 = dgull_1 %>%
  # remove these ind. (metal1_lue_mesure)
  subset(., !(metal1_lue_mesure  %in% id_list))

# two individuals for one ring  (metal2_lue_mesure)
id_list <- dgull_1 %>%
  subset(., metal2_lue_mesure != "NULL") %>%
  group_by(metal2_lue_mesure) %>%
  filter(n() > 1) %>%
  filter(length(unique(id_individu)) > 1) %>%
  pull(metal2_lue_mesure) %>%
  unique()


dgull_1 = dgull_1 %>%
  # remove these ind. (metal2_lue_mesure)
  subset(., !(metal2_lue_mesure  %in% id_list))


# two individuals for one ring (darvic_lue_mesure)
id_list <- dgull_1 %>%
  subset(., darvic_lue_mesure != "NULL") %>%
  group_by(darvic_lue_mesure) %>%
  filter(n() > 1) %>%
  filter(length(unique(id_individu)) > 1) %>%
  pull(darvic_lue_mesure) %>%
  unique()

dgull_1 = dgull_1 %>%
  # remove these ind. (darvic_lue_mesure)
  subset(., !(darvic_lue_mesure  %in% id_list))

#save(dgull_1, file = "/lustre/rumianowskio/dgull_1.Rda")
# load(file = "dgull_1.Rda")


# One encounter per year --------------------------------------------------

# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

# dgull_2 <- dgull_1 %>%
#   group_by(id_individu, year(date_mesure)) %>%
#   mutate(
#     # 2 - The first contact
#     selected_contact = if_else(row_number() == 1, "ok2", NA_character_)
#   ) %>%
#   mutate(
#     # 1 - The first contact with a reproductive status - rewrite on ok1 
#     selected_contact = if_else(repro_statut == "R", "ok1", selected_contact)
#   ) %>%
#   ungroup() 
# 
# dgull_2_2 = dgull_2 %>%
#   subset(.,  !is.na(selected_contact))
# 
# df %>%
#   group_by(Annee) %>%
#   arrange(Annee) %>%
#   filter(row_number() == 1 | (n() > 1 & (any(B == "ok1") | B == "ok2"))) %>%
#   slice(1) %>%
#   ungroup()

# A modifier - la je prends directement la premiere données par rapidité

# dgull_2 <- dgull_1 %>%
#   group_by(id_individu, annee) %>%
#   mutate(
#     first_contact = if_else(row_number() == 1, "ok", NA_character_)
#   ) %>%
#   ungroup() 
# 
# dgull_2_2 = dgull_2 %>%  
#   # !!!
#   subset(.,  !is.na(first_contact))

# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact
dgull_2_2 <- dgull_1 |>
  ungroup() |>
  mutate(repro_statut = ifelse(repro_statut == "R", 1, repro_statut),
         repro_statut = ifelse(repro_statut == "?", 2, repro_statut)) |>
  arrange(id_individu, annee, repro_statut, date_mesure) |>
  group_by(id_individu, annee) |>
  slice_head(n = 1)


# 3 - Incoherence - age ---------------------------------------------------

# ind. recorded as nestilngs after the first year
id_list <- dgull_2_2 %>%
  group_by(id_individu) %>%
  filter(n() > 1, any(age[-1] == "P")) %>%
  pull(id_individu) %>%
  unique()

ind_error_1 = dgull_2_2 %>%
  subset(., (id_individu  %in% id_list)) %>%
  arrange(id_individu)

dgull_3  = dgull_2_2 %>%
  subset(., !(id_individu  %in% id_list) )

#save(dgull_3, file = "/lustre/rumianowskio/dgull_3.Rda")
# load(file = "dgull_3.Rda")
# 
# # 4 - Incoherence - action ------------------------------------------------
# 
# # table(dgull_3$type_controle_simple)
# 
# id = unique(dgull_3$id_individu)
# 
# id_list_A = c()
# id_list_B = c()
# 
# for (ind in id){
#   dind = subset(dgull_3, id_individu == ind)
#   action_ind = dind[,c("type_controle_simple")]
#   if (length(action_ind)>1){
#     for (k1 in 1:(length(action_ind)-1)){
#       for (k2 in (k1+1):length(action_ind)){
#         # ringing after control
#         if(action_ind[k1] == "C" & action_ind[k2] == "B"){
#           id_list_A = c(id_list_A, ind)
#         }
#       }
#     }
#   }
# }
# 
# id_list_A = unique(id_list_A)
# 
# ind_errorA = subset(dgull_3, (id_individu  %in% id_list_A))
# 
# 
# # dgull_4 = subset(dgull_3, !(id_individu  %in% id_list_A))
# 
# dgull_4 = dgull_3
# save(dgull_4, file = "dgull_4.Rda")
# load(file = "dgull_4.Rda")
# 
# # 5 - Find the obvious age -----------------------------------------------------------

#load(file = "dgull_3.Rda")

dgull_5 = dgull_2_2

dgull_5 = dgull_5 %>%
  subset(., age %in% c("P", "A"))

#save(dgull_5, file = "dgull_5.Rda")
# load(file = "dgull_5.Rda")

# Select colonies and years ---------------------------------------------------------

dgull_final = dgull_5


# # controle adult exterieur
# g1 = dgull_final %>% 
#   subset(., !(site_ch %in% c("LR_E", "MA_E", "PC_E", "V5_E") | age == "P"))
# g1$annee %>% table()
# 
# # baguage de poussin extérieur
# g2 = dgull_final %>% 
#   subset(., (!(site_ch %in% c("LR_E", "MA_E", "PC_E", "V5_E")) & age == "P"))
# g2$annee %>% table()

# On garde les poussins bagués en extérieur
dgull_final = dgull_final %>% 
  subset(., site_ch %in% c("LR_E", "MA_E", "PC_E", "V5_E") | age == "P" | (site_ch == "AE" & type_controle_simple == "B"))


# Numbering the sites -----------------------------------------------------

dgull_final = dgull_final %>% 
  dplyr::select(id_individu,
                site_ch,
                annee,
                age)

#save(dgull_final, file = "dgull_final.Rda")



colony_int_list = unique(dgull_final$site_ch)
n.colony = length(colony_int_list)

dgull_final <- dgull_final %>%
  mutate(site= recode(site_ch, !!!setNames(as.character(as.integer(1:n.colony)), c("LR_E", "MA_E", "PC_E", "V5_E", "AE")))) %>% 
  mutate(site = as.integer(site))

#  Formatting in Capture Histories ------------------------------------------

dgull_final <- dgull_final %>% 
  dplyr::select(id_individu,
                annee,
                site,
                age) 



#combien d individu 

dgull_final %>% 
  pull(id_individu) %>% 
  unique() %>% 
  length()

#combien d adult recontact

dgull_final %>% 
  subset(.,age=="A")%>% 
  pull(id_individu) %>% 
  unique() %>% 
  length()

nrow(filet)

1367+30528

# Constants

ind_list = unique(dgull_final$id_individu) # ID
nb_ind = length(ind_list) # number of ID

year_list = min(dgull_final$annee):max(dgull_final$annee) # years with survey
nb_year = length(year_list) # number of survey years

colony_list = unique(dgull_final$site) # site surveyed
n.colony = length(colony_list)  # number of colonies

n.age.class = 3 # number of age classes in this model
ns=n.colony*n.age.class # number of state in this model

nest.states = (1:n.colony)+0*n.colony # nestling states
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class


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

      data_ij = subset(data_i, annee == year)

      age = data_ij[,"age"]
      site = data_ij[,"site"]

      breed.class = NA

      if (age == "P"){
        breed.class = 1
      }

      if (age == "A"){
        breed.class = 2
      }
      CH[i,j] =  as.integer(site + n.colony * (breed.class-1))
    }

    # First summer detected - no reproduction on first summer
    if (j>1){
      if(CH[i,j-1] %in% 1:n.colony){ # if the ind. was a nestling the year before
        CH[i,j] = 0
      }
    }
  }
}

CH[is.na(CH)] = 0

# Filter les erreurs où on controle en AE par le filet

CH <- CH %>%
  as.data.frame()  %>%
  filter(!(apply(., 1, function(x) {
    has_non_zero_before_10 <- FALSE
    for (i1 in seq_along(x)) {
      if (x[i1] == 10) {
        for (i2 in seq_along(x)) {
          if (x[i2] != 0) {
            if (i2<i1){
              has_non_zero_before_10 <- TRUE
            }
          } 
        }
      }
    }
    return(has_non_zero_before_10 )
    
    
  })))


save(CH, file = "CH_AE_filet2.Rda")

# ind_list[23621] individu problematique

CH = CH[-c(23621),]

#save(CH, file = "CH_AE.Rda")


# save(CH, file = "/lustre/rumianowskio/CH.Rda")

# load(file = "CH_AE2.Rda")

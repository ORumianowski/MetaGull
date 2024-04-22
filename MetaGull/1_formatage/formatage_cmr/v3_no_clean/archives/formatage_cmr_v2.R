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

save(dgull_1, file = "/lustre/rumianowskio/dgull_1.Rda")
# load(file = "dgull_1.Rda")


# One encounter per year --------------------------------------------------

dgull_1$annual_encounter_occasion = NA

# Anotation of the order of occurrence of intra-annual data
for (ind in unique(dgull_1$id_individu)){
  dind = subset(dgull_1, id_individu == ind)
    for (cyear in year(dind$date_mesure)){
      dind_year = subset(dind, year(date_mesure) == cyear)
      for (cdate in 1:nrow(dind_year)){
        dgull_1[(dgull_1$id_individu == ind & year(dgull_1$date_mesure) == cyear), c("annual_encounter_occasion")][cdate] = cdate
      }
    }
}

dgull_2_1 = dgull_1

save(dgull_2_1, file = "/lustre/rumianowskio/dgull_2_1.Rda")
# load(file = "dgull_2_1.Rda")


# Selection of one encounter per year
# Hierarchical criteria :
# 1 - The first contact with a reproductive status
# 2 - The first contact

dgull_2_1$selected_contact = NA

for (ind in unique(dgull_2_1$id_individu)){
  dind = subset(dgull_2_1, id_individu == ind)
    for (cyear in unique(year(dind$date_mesure))){
      dind_year = subset(dind, year(date_mesure) == cyear)

      # 1 - The first contact with a reproductive status
      mesure_repro = dind_year %>%
        filter(repro_statut == "R")  %>%
        pull(id_mesure) %>%
        head(., 1)

      if (length(mesure_repro)>0){
        dgull_2_1[dgull_2_1$id_mesure==mesure_repro,"selected_contact"] = "ok1"
      }

      # 2 - The first contact
      else{
        dgull_2_1[dgull_2_1$id_individu == ind &  year(dgull_2_1$date_mesure) == cyear & dgull_2_1$annual_encounter_occasion==1,"selected_contact"] = "ok2"

      }
    }#cyear
}#ind

dgull_2_2 = dgull_2_1

save(dgull_2_2, file = "/lustre/rumianowskio/dgull_2_2.Rda")
# load(file = "dgull_2_2.Rda")

dgull_2_3 = dgull_2_2 %>%
  subset(.,  !is.na(selected_contact))%>%
  mutate(annee = year(date_mesure))

save(dgull_2_3, file = "/lustre/rumianowskio/dgull_2_3.Rda")
# load(file = "dgull_2_3.Rda")


# 3 - Incoherence - age ---------------------------------------------------

# ind. recorded as nestilngs after the first year
id_list <- dgull_2_3 %>%
  group_by(id_individu) %>%
  filter(n() > 1, any(age[-1] == "P")) %>%
  pull(id_individu) %>%
  unique()

ind_error_1 = dgull_2_3 %>% 
  subset(., (id_individu  %in% id_list)) %>% 
  arrange(id_individu)
  
dgull_3  = dgull_2_3 %>%
  subset(., !(id_individu  %in% id_list) )

save(dgull_3, file = "/lustre/rumianowskio/dgull_3.Rda")
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

dgull_5 = dgull_3
#dgull_5 = dgull_4

dgull_5[dgull_5$age=="NULL", "age"] <- "A"

dgull_5 = dgull_5 %>%
  subset(., age %in% c("P", "A"))

save(dgull_5, file = "/lustre/rumianowskio/dgull_5.Rda")

#save(dgull_5, file = "dgull_5.Rda")
# load(file = "dgull_5.Rda")

# Select colonies and years ---------------------------------------------------------

dgull_final = dgull_5

dgull_final = dgull_final %>% 
  subset(.,id_site_principal %in% c("LR_E", "MA_E", "PC_E", "V5_E")) %>% 
  subset(., annee %in% 1986:2005)

#dgull_final = dgull_final[1:10000,]

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

# Constants

ind_list = unique(dgull_final$id_individu) # ID
nb_ind = length(ind_list) # number of ID

year_list = min(dgull_final$annee):max(dgull_final$annee) # years with survey
nb_year = length(year_list) # number of survey years

colony_list = unique(dgull_final$site) # site surveyed
n.colony = length(colony_list) + 1  # number of colonies = site surveyed + one non-surveyed

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
      breed.status = data_ij[,"repro_statut"]
      
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

save(CH, file = "CH.Rda")
save(CH, file = "/lustre/rumianowskio/CH.Rda")

# load(file = "CH.Rda")


library(tidyr)
library(tidyverse)

peron_counts  <- read.csv("C:/Users/33763/Downloads/histo 5sites recrutement -clear.inp", header=FALSE)

peron_counts$V1 <- substr(peron_counts$V1, 1, nchar(peron_counts$V1) - 2)

peron_counts <- separate(peron_counts, V1, into = c("ch", "N"), sep = "  ", remove = FALSE) %>% 
  dplyr::select(ch, N) %>% 
  mutate(N = as.integer(N))

load(file = "my_ch_counts.Rda")

my_ch_counts = my_ch_counts %>% 
  rename(ch = combined,
         my_N = n)


comp_counts = merge(peron_counts, my_ch_counts, by=("ch"), all = T) %>% 
  mutate(diff = N - my_N) %>% 
  arrange(diff)



sum(peron_counts$N)
sum(my_ch_counts$my_N)

peron_counts2 = separate(peron_counts, ch, into = as.character(1986:2005), sep = "", remove = FALSE)


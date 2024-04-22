

library(tidyverse)

B_peron =  B %>%
  as.data.frame() %>% 
  slice(c(1, 2, 4, 3, 5)) %>% 
  as.matrix()
  
save(B_peron, file = "B_peron.Rda")

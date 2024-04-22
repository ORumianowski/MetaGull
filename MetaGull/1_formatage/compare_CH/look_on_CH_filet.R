

CH10 = CH %>%
  as.data.frame() %>% 
  filter_at(vars(everything()), any_vars(. == 10))


nrow(CH)

CH %>%
  as.data.frame()  %>%
  filter(apply(., 1, function(x) {
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
    
    
  }))


nrow(CH)

CH10_2 = CH10 %>%
  filter(!(rowSums(.) == 10))

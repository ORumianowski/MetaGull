



rd = reading_effort[,21:34]


rd[is.na(rd)] = 0


b = B[1:9,21:34]


b = ifelse(b == 0, 0, 1) 

rd = ifelse(rd == 0, 0, 1) 

b == rd

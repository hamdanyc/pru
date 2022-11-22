# huluSel.R

library(dplyr)

vet.df %>% 
  filter(parlimen == "HULU SELANGOR") %>% 
  write.csv("huluselangor.txt")

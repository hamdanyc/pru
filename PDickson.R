# PDickson.r

library(pivottabler)
library(dplyr)

pru14 %>%
  filter(UNDI_PARLIMEN == "PORT DICKSON") %>% 
  select(undi_status) %>% 
  table() %>% 
  addmargins()

pru14 %>%
  filter(UNDI_PARLIMEN == "PORT DICKSON") %>%
  select(UNDI_LOKALITI, undi_status) %>% 
  table() %>% 
  addmargins() %>% 
  write.csv("portdickson.txt")

pru14 %>%
  filter(UNDI_PARLIMEN == "PORT DICKSON") %>%
  group_by(UNDI_LOKALITI) %>% 
  count(undi_status) %>% table()

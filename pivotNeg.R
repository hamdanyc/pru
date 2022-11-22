library(dplyr)
library(tidyr)

pvt <- dpi %>%
  select(NEGERI, LAHIRNEG) %>% 
  group_by(NEGERI, LAHIRNEG) %>%
  tally() %>%  # eg. summarise(n())
  spread(LAHIRNEG, n, fill = 0)

pvt <- as.matrix(pvt)

plk %>%
  group_by(neg_daftar) %>%
  select(neg_skrg) %>% 
  table() %>% 
  addmargins(1) %>% 
  addmargins(2)

plk.neg <- plk  %>%
  select(neg_daftar, neg_skrg) %>% 
  group_by(neg_daftar, neg_skrg) %>%
  tally() %>% 
  spread(neg_skrg, n, fill = 0)

pru14 %>% 
  group_by(UNDI_LOKALITI, UNDI_PARLIMEN) %>% 
  filter(UNDI_PARLIMEN %in% c("KIMANIS","LIBARAN","KENINGAU","SIPITANG")) %>% 
  tally()

pru14 %>% 
  group_by(UNDI_LOKALITI, UNDI_DUN) %>% 
  filter(UNDI_DUN %in% c("BALAKONG")) %>% 
  tally()

pru14 %>% 
  group_by(UNDI_PARLIMEN, UNDI_DUN) %>% 
  filter(UNDI_NEGERI == "SELANGOR") %>% 
  tally()
        
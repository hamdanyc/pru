library(dplyr)

plk %>% 
  left_join(pru14, by = c("noten" = "NOTEN")) %>%
  #group_by(BARU_PARLIMEN) %>%
  inner_join(airfare, by = c("BARU_PARLIMEN" = "to", "UNDI_NEGERI" = "from")) %>%
  # filter(BARU_NEGERI == "SARAWAK" | BARU_NEGERI == "SABAH") %>%
  # filter(BARU_NEGERI == "SARAWAK" | BARU_NEGERI == "SABAH", !is.na(rate)) %>%
  select(rate)

# Region: Semanjung -> Sabah/Sarawak
plk %>% 
  left_join(pru14, by = c("noten" = "NOTEN")) %>%
  left_join(airfare, by = c("BARU_PARLIMEN" = "to", "UNDI_NEGERI" = "from")) %>%
  group_by(BARU_PARLIMEN) %>% 
  filter(BARU_NEGERI == "SARAWAK" | BARU_NEGERI == "SABAH") %>%
  select("Negeri" = UNDI_NEGERI) %>% 
  table() %>%
  addmargins(1) %>% 
  addmargins(2)
  # write.csv("sbh_srwk.csv", row.names = F, quote = F)
  
# Region: Sabah/Sarawak -> Semanjung
plk %>% 
  left_join(pru14, by = c("noten" = "NOTEN")) %>%
  left_join(airfare, by = c("BARU_PARLIMEN" = "to", "UNDI_NEGERI" = "from")) %>%
  group_by(BARU_NEGERI) %>% 
  filter(UNDI_NEGERI != "SARAWAK" | UNDI_NEGERI != "SABAH") %>%
  select("Negeri" = BARU_NEGERI) %>% 
  table() %>%
  addmargins(1)

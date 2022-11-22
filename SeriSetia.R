# SeriSetia.R

# Init --------------------------------------------------------------------

library(pivottabler)
library(dplyr)

load("SeriSetia.RData")

# prk.SeriSetia <- pru14 %>% 
#   filter(UNDI_DUN == "SERI SETIA")


prk.SeriSetia %>%
  qhpvt("BARU_NEGERI","undi_status","n()",
        totals=list("BARU_NEGERI"="Keseluruhan",
                    "undi_status"="Jumlah"), na.rm=T)

# Statistik Keseluruhan ---------------------------------------------------

# Stat Status Keseluruhan
prk.SeriSetia %>%
  select(undi_status) %>% 
  table() %>% 
  addmargins()

# Stat Mengikut Psk
prk.SeriSetia %>%
  select("Pasukan"=UNDI_LOKALITI,"Status"=undi_status) %>% 
  table() %>% 
  addmargins()

# Stat Bukan Kekal
prk.SeriSetia %>%
  filter(undi_status != "KEKAL") %>% 
  select("Negeri"=BARU_NEGERI,"Status"=undi_status) %>% 
  table() %>% 
  addmargins()

save(prk.SeriSetia,file = "SeriSetia.RData")



        
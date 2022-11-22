# expAll.R

# Init --------------------------------------------------------------------

library(dplyr)
library(pivottabler)

load("pru.RData")

days <- 2
semenanjung <- c("JOHOR","KEDAH","KELANTAN","MELAKA","NEGERI SEMBILAN","PAHANG",
                 "PERAK","PERLIS","PULAU PINANG","SELANGOR","TERENGGANU",
                 "W.P KUALA LUMPUR")
sabah.sarawak <- c("SABAH", "SARAWAK", "W.P LABUAN")

# Set sample plk ----------------------------------------------------------

by.set <- plk %>% 
  filter(neg_daftar %in% c(semenanjung,"W.P LABUAN"),
         neg_skrg == "W.P KUALA LUMPUR",
         pkt %in% c("KPL","SJN","SSJN","PW II","KAPT","MEJ"))

n.sample <- 0.05
set.seed(1)
plk.set <- sample_frac(by.set, n.sample)

# Calculate ----------------------------------------------------------------

# Elaun Penginapan dan Harian
trasmi <- plk.set %>%
  left_join(TKlykn, by = "pkt") %>%
  mutate(tugas.rasmi = case_when(
    neg_daftar %in% sabah.sarawak & Zon == "Sbh_Swk" ~ (Harian + Makan + Hotel) * days,
    neg_daftar %in% semenanjung & Zon == "Semenanjung" ~ (Harian + Makan + Hotel) * days)) %>%
  group_by(khidmat) %>%
  filter(tugas.rasmi != "") %>% 
  summarise(Bil = n(), Tugas_Rasmi = sum(tugas.rasmi, na.rm = T))

# Tuntutan Perjalanan
# tmiles <- plk.set %>%
#   left_join(TKlykn, by = "pkt") %>%
#   left_join(TPjln, by = c("neg_skrg" = "dari", "neg_daftar" = "ke", "Kadar")) %>%
#   mutate(`Elaun Kenderaan` = amaun) %>%
#   group_by(khidmat) %>%
#   filter(Zon == "Semenanjung", `Elaun Kenderaan` != 0) %>%
#   summarise( Bil = n(), `Elaun Kenderaan` = sum(amaun, na.rm = T))

# Tuntutan Perjalanan Table Wide
tmiles <- plk.set %>%
  left_join(TKlykn, by = "pkt") %>%
  left_join(TPjln.Wide, by = c("neg_skrg" = "dari", "neg_daftar" = "ke")) %>%
  mutate(`Elaun Kenderaan` = case_when(Kadar == "A" ~ A,
                                       Kadar == "B" ~ B,
                                       Kadar == "C" ~ C,
                                       Kadar == "D" ~ D,
                                       Kadar == "E" ~ E)) %>% 
  group_by(khidmat) %>%
  filter(Zon == "Semenanjung", `Elaun Kenderaan` != 0) %>%
  summarise( Bil = n(), `Elaun Kenderaan` = sum(`Elaun Kenderaan`, na.rm = T))

# Tambang K. Terbang
tair <- plk.set %>%
  left_join(airport, by = "par") %>%
  left_join(airfare, by = c("neg_skrg" = "dari","dest" = "ke")) %>%
  mutate(byair = harga*2) %>% # pergi & balik
  group_by(khidmat) %>%
  summarise( Bil = n(), Penerbangan = sum(byair, na.rm = T))

expense <- cbind(trasmi,tmiles[3],tair[3]) %>%
  t() %>% 
  write.csv("expense.csv")

# Statistik ---------------------------------------------------------------

plk.set %>%
  group_by(neg_skrg) %>%
  select(neg_daftar) %>%
  table() %>% 
  addmargins(2) %>% 
  addmargins(1)

# pivot table
plk %>%
  qhpvt("neg_skrg","neg_daftar","n()",
        totals=list("neg_skrg"="Neg. Kini",
                    "neg_daftar"="Neg. Daftar"))

# Init plk ---------------------------------------------------------------

plk <- pru14 %>% 
  filter(undi_status == "PLK") %>% 
  select("noten" = NOTEN, "khidmat" = KHIDMAT, "pkt" = PKT_,
         "neg_daftar" = BARU_NEGERI, "neg_skrg" = UNDI_NEGERI,
         "par" = BARU_PARLIMEN)

# pru14 %>% 
#   filter(UNDI_NEGERI == c("JOHOR","KEDAH","KELANTAN","MELAKA","NEGERI SEMBILAN",
#                           "PAHANG","PERAK","PERLIS","PULAU PINANG",
#                           "SELANGOR","TERENGGANU","W.P KUALA LUMPUR")) %>%
#   group_by(UNDI_PARLIMEN) %>%
#   tally() %>%
#   mutate(dest = "W.P KUALA LUMPUR") %>%
#   select(UNDI_PARLIMEN,dest) %>% 
#   write.csv("air.csv",row.names = F)

# End Note - Recover Table ----------------------------------------------------------------
# refer to CalcPjln.R for TKdr_Pjln

airfare <- read.csv("airfare.csv", header = T,
                    colClasses = c("character","character","numeric"))
airport <- read.csv("airport.csv", header = T,
                    colClasses = c("character","character"))
TPjln <- read.csv("TPjln.csv", header = T,
                    colClasses = c("character","character","numeric", 
                                   "numeric","factor"))
TKdr_Pjln <- read.csv("TKdr_Pjln.csv", header = T,
                    colClasses = c("character","character","numeric"))
TKlykn <- read.csv("TKlykn.csv", header = T,
                  colClasses = c("character","factor","numeric","numeric","numeric",
                                 "factor","factor"))
TMilage <- read.csv("TMilage.csv", header = T,
                   colClasses = c("factor","numeric","numeric","numeric","numeric"))

save.image("pru.RData")


# init.R

library(readr)
library(readxl)
library(dplyr)

# init DB -----------------------------------------------------------------

kepuasan <- read_delim("C:/Users/hy/OneDrive/Project R/Kepuasan Warga/KS_warga_atm.txt", 
                       ";", escape_double = FALSE, col_types = cols(BIL = col_skip()), 
                       trim_ws = TRUE)

kpai <- read_delim("C:/Users/hy/OneDrive/Project R/kpai/integriti.txt", 
                        ";", escape_double = FALSE, col_types = cols(BIL = col_skip()), 
                        trim_ws = TRUE)

espn <- read_delim("C:/Users/hy/OneDrive/Project R/espn/KS_espn_atm.txt", 
                   ";", escape_double = FALSE, col_types = cols(BIL = col_skip(), 
                                                                JANTINA = col_skip(), KHIDMAT = col_skip(), 
                                                                NEGERI = col_skip(), PANGKAT = col_skip(), 
                                                                UNIT = col_skip()), trim_ws = TRUE)

dpi <- read_excel("s3.xlsx")

# Define Index ------------------------------------------------------------
# Kepuasan Warga ATM

motivasi <- kepuasan %>%
  select(NOTEN,S1:S30) %>% 
  mutate(
    indeks.tugas = ((S1 + S2 + S3 + S4 + S5) - 5) / 20,
    indeks.kerja = ((S6 + S7 + S8 + S9 + S10) - 5) / 20,
    indeks.kediaman = ((S11 + S12 + S13 + S14 + S15) - 5) / 20,
    indeks.motivasi = ((S16 + S17 + S18 + S19 + S20) - 5) / 20,
    indeks.sosial = ((S21 + S22 + S23 + S24 + S25) - 5) / 20,
    indeks.kerjaya = ((S26 + S27 + S28 + S29 + S30) - 5) / 20,
    motivasi.komposit = (indeks.tugas + indeks.kerja +
                         indeks.motivasi + indeks.sosial + 
                         indeks.kerjaya) / 5)

motivasi$NOTEN <- as.character(motivasi$NOTEN)

integriti <- kpai %>%
  rename(NOTEN = NOTENTERA) %>% 
  select(NOTEN,S1:S76) %>% 
  mutate(kefahaman.individu = (S1+S2+S3+S4+S5+S6+S7+S8+S9+S10-10)/40,
         kefahaman.organisasi = (S11+S12+S13+S14+S15+S16+S17+S18+S19+S20-10)/40,
         amalan.individu = (S21+S22+S23+S24+S25+S26+S27+S28+S29+S30+S31+S32+S33+S34+S35-15)/60,
         amalan.organisasi = (S36+S37+S38+S39+S40+S41+S42+S43+S44+S45+S46+S47+S48+S49+S50+S51-15)/60,
         isu.integriti = 1-(S52+S53+S54+S55+S56-5)/20, # reverse
         faktor.integriti = (S57+S58+S59+S60+S61+S62+S63+S64+S65+S66+S67+S68+S69+
                S70+S71+S72+S73+S74+S75+S76-20)/80,
         integriti.Komposit = (1.2*kefahaman.individu + 1.2*kefahaman.organisasi +
                                 1.5*amalan.individu + 1.5*amalan.organisasi +
                                 0.3*isu.integriti+0.3*faktor.integriti)/6, na.rm=TRUE)

integriti$NOTEN <- as.character(integriti$NOTEN)

sosio.politik <- espn %>% 
  select(NOTEN,S1:S36) %>% 
  mutate(Keselamatan.Ekonomi = (16-(S1+S9+S13+S16))/12,
         Keselamatan.Kerajaan = (16-(S8+S19+S3+S5))/12,
         Keselamatan.Pertahanan = (20-(S4+S2+S6+S12+S21))/15,
         Keselamatan.Politik = (16-(S20+S7+S12+S15))/12,
         Keselamatan.Sosial = (16-(S10+S11+S14+S18))/12,
         Kemudahan.Perkhidmatan = (45-(S22+S23+S24+S25+S26+S27+S28+S29+S30))/36,
         Skim.Perkhidmatan = (30-(S31+S32+S33+S34+S35+S36))/24)

sosio.politik$NOTEN <- as.character(sosio.politik$NOTEN)

# Merge Data Frame --------------------------------------------------------

# dpi and motivasi
dpi.motivasi <- dpi %>% 
  inner_join(motivasi) %>% 
  select(NOTEN:LAHIRNEG, indeks.tugas:motivasi.komposit)

# dpi and integriti
dpi.integriti <- dpi %>% 
  inner_join(integriti) %>% 
  select(NOTEN:LAHIRNEG, kefahaman.individu:integriti.Komposit)

# dpi and sosio.politik
dpi.sosio_politik <- dpi %>% 
  inner_join(sosio.politik) %>% 
  select(NOTEN:LAHIRNEG, Keselamatan.Ekonomi:Skim.Perkhidmatan)

# Save DF -----------------------------------------------------------------

save.image("pru.RData")

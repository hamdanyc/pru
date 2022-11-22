# simproc.R

# Init ----

library(dplyr)

load("pru14.RData")

# D'Hondt and Webster/Sainte-Lague Function ---
# usage Dhondt(letters[1:5], votes, 10 ) 
Dhondt <- function( candidates, votes, seats ){
  tmp <- data.frame(
    candidates = rep( candidates, each = seats ),
    scores     = as.vector(sapply( votes, function(x) x /
                                     1:seats ))
  )
  tmp <- tmp$candidates[order( - tmp$scores )] [1:seats]
  table(tmp)
} 

StLague <- function( candidates, votes, seats ){
  tmp <- data.frame(
    candidates = rep( candidates, each = seats ),
    scores     = as.vector(sapply( votes, function(x) x /
                                     1:seats * 2 ))
  )
  tmp <- tmp$candidates[order( - tmp$scores )] [1:seats]
  table(tmp)
}

# Calc FPP votes ----

fpp_undi_parlimen <- pru14_parlimen %>% 
  group_by(PARTI,`BAHAGIAN PILIHAN RAYA`,STATUS) %>%
  select(`PARTI`,`BILANGAN UNDI`) %>%
  summarise("Jlh" = sum(`BILANGAN UNDI`))

# Vote count -- winner only ----
fpp_nisbah_undi_parti <- pru14_parlimen %>% 
  select(PARTI, STATUS, `BILANGAN UNDI`) %>% 
  group_by(PARTI) %>%
  mutate(Jlh =
    case_when(
      STATUS == "MENANG" ~ `BILANGAN UNDI`,
      TRUE ~ 0
    )) %>% 
  summarise("Jlh" = sum(Jlh), "Nisbah" = round(Jlh / jlh_undi,digits = 2))

party_seat <- pru14_parlimen %>% 
  group_by(PARTI) %>% 
  summarise(calon = n())

# Calc propotion votes by party ----

undi_parlimen <- pru14_parlimen %>% 
  group_by(`BAHAGIAN PILIHAN RAYA`, PARTI) %>% 
  select(`PARTI`,`BILANGAN UNDI`) %>%
  summarise("Jlh" = sum(`BILANGAN UNDI`))

undi_parti <- pru14_parlimen %>% 
  group_by(PARTI) %>% 
  select(`PARTI`,`BILANGAN UNDI`) %>%
  summarise("Jlh" = sum(`BILANGAN UNDI`))

jlh_undi <- sum(undi_parlimen$Jlh)

nisbah_undi_parti <- undi_parti %>%
  select(PARTI, Jlh) %>% 
  group_by(PARTI) %>% 
  summarise("Jlh" = sum(Jlh), "Nisbah" = round(Jlh / jlh_undi,digits = 2))

# Merge fpp and pr table ----
undi_tbl <- party_seat %>%
  bind_cols(fpp_nisbah_undi_parti[2:3]) %>% 
  bind_cols(nisbah_undi_parti[2:3])

write.csv(undi_tbl,"undi_fpp_pr.csv")

# Compare method ----
res_dhondt <- Dhondt(nisbah_undi_parti$PARTI,nisbah_undi_parti$Jlh,222) %>% as_tibble()
res_StLague <-StLague(nisbah_undi_parti$PARTI,nisbah_undi_parti$Jlh,222) %>% as_tibble()

# Save file ----
save.image("pru14.RData")

  
  
  

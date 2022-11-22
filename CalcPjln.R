# CalcPjln.R

A <- TKdr_Pjln %>%
  mutate(amaun = case_when(
    Jarak < 501 ~ 500*0.7, 
    Jarak > 500 & Jarak < 1001  ~ 500*0.7 + (Jarak-500)*0.65,
    Jarak > 1001 & Jarak < 1701 ~ 500*0.7 + (Jarak-500)*0.65 + (Jarak-1000)*0.55,
    Jarak > 1701 ~ 500*0.7 + (Jarak-500)*0.65 + (Jarak-1000)*0.55 + (Jarak-1701)*0.5), 
    Kadar = "A")

B <- TKdr_Pjln %>%
  mutate(amaun = case_when(
    Jarak < 501 ~ 500*0.6, 
    Jarak > 500 & Jarak < 1001  ~ 500*0.6 + (Jarak-500)*0.55,
    Jarak > 1001 & Jarak < 1701 ~ 500*0.6 + (Jarak-500)*0.55 + (Jarak-1000)*0.5,
    Jarak > 1701 ~ 500*0.6 + (Jarak-500)*0.55 + (Jarak-1000)*0.5 + (Jarak-1701)*0.45), 
    Kadar = "B")

C <- TKdr_Pjln %>%
  mutate(amaun = case_when(
    Jarak < 501 ~ 500*0.5, 
    Jarak > 500 & Jarak < 1001  ~ 500*0.5 + (Jarak-500)*0.45,
    Jarak > 1001 & Jarak < 1701 ~ 500*0.5 + (Jarak-500)*0.45 + (Jarak-1000)*0.4,
    Jarak > 1701 ~ 500*0.5 + (Jarak-500)*0.45 + (Jarak-1000)*0.4 + (Jarak-1701)*0.35),
    Kadar = "C")

D <- TKdr_Pjln %>%
  mutate(amaun = case_when(
    Jarak < 501 ~ 500*0.45, 
    Jarak > 500 & Jarak < 1001  ~ 500*0.45 + (Jarak-500)*0.4,
    Jarak > 1001 & Jarak < 1701 ~ 500*0.45 + (Jarak-500)*0.4 + (Jarak-1000)*0.35,
    Jarak > 1701 ~ 500*0.45 + (Jarak-500)*0.4 + (Jarak-1000)*0.35 + (Jarak-1701)*0.3),
    Kadar = "D")

E <- TKdr_Pjln %>%
  mutate(amaun = case_when(
    Jarak < 501 ~ 500*0.4, 
    Jarak > 500 & Jarak < 1001  ~ 500*0.4 + (Jarak-500)*0.35,
    Jarak > 1001 & Jarak < 1701 ~ 500*0.4 + (Jarak-500)*0.35 + (Jarak-1000)*0.3,
    Jarak > 1701 ~ 500*0.4 + (Jarak-500)*0.35 + (Jarak-1000)*0.3 + (Jarak-1701)*0.25),
    Kadar = "E")

TPjln <- rbind(A,B,C,D,E)
TPjln.Wide <- tidyr::spread(TPjln,Kadar,amaun)

TPjln <- TPjln %>%
  mutate(Neg_Daftar = replace(Neg_Daftar,Neg_Daftar == "KUALA LUMPUR","W.P KUALA LUMPUR"),
         Neg_Kini = replace(Neg_Kini,Neg_Kini == "KUALA LUMPUR","W.P KUALA LUMPUR"))

# rename columns
# TPjln <- TPjln %>% 
#   rename("dari" = Neg_Daftar, "ke" = Neg_Kini)

write.csv(TPjln,"TPjln.csv",row.names = F)



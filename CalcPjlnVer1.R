# CalcPjlnVer1.R

library(dplyr)

kadar.table <- function(kadar,Jarak){
  r <- case_when(kadar == "a" ~ c(0.7,0.65,0.55,0.5),
                   kadar == "b" ~ c(0.60,0.55,0.5,0.45),
                   kadar == "c" ~ c(0.50,0.45,0.4,0.35),
                   kadar == "d" ~ c(0.45,0.4,0.35,0.3),
                   kadar == "e" ~ c(0.4,0.35,0.3,0.25))

  amaun <-  case_when(
    Jarak < 501 ~ Jarak*r[1],
    Jarak > 500 & Jarak < 1001  ~ Jarak*r[1] + (Jarak-500)*r[2],
    Jarak > 1001 & Jarak < 1701 ~ Jarak*r[1] + (Jarak-500)*r[2] + (Jarak-1000)*r[3],
    Jarak > 1701 ~ Jarak*r[1] + (Jarak-500)*r[2] + (Jarak-1000)*r[3] + (Jarak-1701)*r[4])
  return(round(amaun,digits=0))
}



kadar.table("a", 1248)

TPjln.Wide <- data.frame(TKdr_Pjln[1:3],
       "A"=lapply(TKdr_Pjln[3], function(x) kadar.table("a",x)),
       "B"=lapply(TKdr_Pjln[3], function(x) kadar.table("b",x)),
       "C"=lapply(TKdr_Pjln[3], function(x) kadar.table("c",x)),
       "D"=lapply(TKdr_Pjln[3], function(x) kadar.table("d",x)),
       "E"=lapply(TKdr_Pjln[3], function(x) kadar.table("e",x))
       )
names(TPjln.Wide) =c("dari","ke","Jarak","A","B","C","D","E")


# funkdr.R

library(dplyr)

xt <- function(k,Jarak){
  r <- if(k=="a") TMilage[1,2:5] else
    if(k=="b") TMilage[2,2:5] else
      if(k=="c") TMilage[3,2:5] else
        if(k=="d") TMilage[4,2:5] else
          TMilage[5,2:5]

  amaun <- ifelse(Jarak < 501,Jarak*r[1],
                  ifelse(Jarak > 500 & Jarak < 1001,Jarak*r[1] + (Jarak-500)*r[2],
                         ifelse(Jarak > 1001 & Jarak < 1701,Jarak*r[1] + (Jarak-500)*r[2] + (Jarak-1000)*r[3],
                                Jarak*r[1] + (Jarak-500)*r[2] + (Jarak-1000)*r[3]
                                + (Jarak-1701)*r[4])))
  return(amaun)
}

xt("a",501)

xdf <- TKdr_Pjln[1:3],
                         "A"=sapply(TKdr_Pjln[3], function(x) xt("a",x)),
                         "B"=sapply(TKdr_Pjln[3], function(x) xt("b",x)),
                         "C"=sapply(TKdr_Pjln[3], function(x) xt("c",x),
                         "D"=sapply(TKdr_Pjln[3], function(x) xt("d",x),
                         "E"=sapply(TKdr_Pjln[3], function(x) xt("e",x)
)
names(TPjln.Wide) =c("dari","ke","Jarak","A","B","C","D","E")


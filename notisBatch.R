# notis.R

# Packages
library(knitr)
library(rmarkdown)

# vet <- read_delim("vet.csv", ";", escape_double = FALSE,
#                   col_types = cols(IC = col_character()),
#                   trim_ws = TRUE)

vet.df <- data.frame(kp=vet$IC, nama=vet$Nama, alamat1=vet$ALAMAT1, alamat2=vet$ALAMAT2, alamat3=vet$ALAMAT3,
                      psk= vet$NamaLokaliti, parlimen = vet$NamaParlimen, dun = vet$NamaDUN, dm = vet$NamaDM,
                      negeri = vet$Negeri,stringsAsFactors=FALSE)

vet.info <- vet.df[1:5,]

# Loop nrow(vet.info)
for (i in 1:nrow(vet.info)){
  rmarkdown::render(input = "notisjust.Rmd",
                    output_format = "word_document",
                    output_file = paste("notis_", i, ".docx", sep=''),
                    output_dir = "Surat/")
}

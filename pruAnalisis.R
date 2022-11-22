# pruAnalisis.R

library(ggplot2)
library(dplyr)

# Motivasi ----------------------------------------------------------------

# The base plot

# indeks motivasi by negeri, parlimen

dpi.motivasi %>% 
  ggplot(aes(factor(NEGERI), indeks.motivasi, fill = NEGERI, outline=FALSE)) + geom_boxplot()

dpi.motivasi %>% 
  ggplot(aes(factor(NEGERI), indeks.motivasi, fill = UMURKATE)) + geom_bar(stat = "identity")

wpkl <- dpi.motivasi %>% 
  filter(NEGERI=="W.P KUALA LUMPUR")

wpkl %>% 
  ggplot(aes(factor(NAMAPARLIMEN), indeks.motivasi, fill = NEGERI, outline=FALSE)) + geom_boxplot()

wpkl %>% 
  ggplot(aes(factor(NAMAPARLIMEN), indeks.kediaman, fill = NEGERI, outline=FALSE)) + geom_boxplot()

wpkl %>% 
    ggplot(aes(factor(NAMAPARLIMEN), motivasi.komposit, fill = NEGERI, outline=FALSE)) + geom_boxplot()


# Group Different ---------------------------------------------------------

motivasi.gp <- lm(formula = indeks.motivasi ~ NEGERI, data = dpi.motivasi)

# semua negeri kecuali N. Sebilan signifikan
summary(motivasi.gp)            

# ANOVA analysis
anova(motivasi.gp)

# alternate test
motivasi.aov <- aov(formula = indeks.motivasi ~ NEGERI, data = dpi.motivasi)

# semua negeri kecuali N. Sebilan signifikan
summary(motivasi.aov) 

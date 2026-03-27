
rm(list=ls()) # ripulisce la memoria di R
df <- read.csv("~/Desktop/Corso_R/my_data/compare_2_species.csv")

# Prendiamo confidenza coi dati:
head(df) # le prime sei righe del data.frame df
tail(df) # le ultime sei righe del data.frame df


str(df)

df$Species <- as.factor(df$Species)

class(df$Species)

boxplot(body.mass ~ Species, data=df)

m1 <- lm(body.mass ~ Species, data=df)

m1


## Grafici diagnostici per il modello $m1$.
# suddividiamo la finestra grafica in due righe e due colonne:
par(mfrow=c(2, 2))
plot(m1)


## ----chap03chunk05-------------------------------------------------------------------
m0 <- lm(body.mass ~ 1, data=df)


## ----chap03chunk06-------------------------------------------------------------------
anova(m1, m0)


## ----chap03chunk08-------------------------------------------------------------------
summary(m1)


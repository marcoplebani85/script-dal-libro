## ----eval=F, tidy=F------------------------------------------------------------------
# rm(list=ls()) # ripulisce la memoria di R
# df <- read.csv("~/Desktop/Corso_R/my_data/compare_2_species.csv")


## ----chap03_chunk01, include=F, warning = F, message=F, results=F--------------------
rm(list=ls()) # ripulisce la memoria di R
df <- read.csv("~/Desktop/Corso_R/my_data/compare_2_species.csv")


## ----include=F, warning = F, message=F, results=F------------------------------------
if (!require(formatR)) install.packages('formatR'); library(formatR)
if (!require(knitr)) install.packages('knitr'); library(knitr)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80),tidy=TRUE, dpi=200)

options(width = 80)


## ----chap03chunk01b------------------------------------------------------------------
# Prendiamo confidenza coi dati:
head(df) # le prime sei righe del data.frame df
tail(df) # le ultime sei righe del data.frame df


## ----chap03chunk01bb-----------------------------------------------------------------
str(df)


## ----chap03chunk01c------------------------------------------------------------------
df$Species <- as.factor(df$Species)


## ------------------------------------------------------------------------------------
class(df$Species)


## ----chap03chunk02, warning = F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Distribuzione delle masse corporee misurate per $Species A$ e $Species B$."----
boxplot(body.mass ~ Species, data=df)


## ------------------------------------------------------------------------------------
m1 <- lm(body.mass ~ Species, data=df)


## ------------------------------------------------------------------------------------
m1


## ----chap03chunk04, tidy=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello $m1$."----
# suddividiamo la finestra grafica in due righe e due colonne:
par(mfrow=c(2, 2))
plot(m1)


## ----chap03chunk05-------------------------------------------------------------------
m0 <- lm(body.mass ~ 1, data=df)


## ----chap03chunk06-------------------------------------------------------------------
anova(m1, m0)


## ----chap03chunk08-------------------------------------------------------------------
summary(m1)


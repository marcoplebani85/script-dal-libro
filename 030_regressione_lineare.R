
rm(list=ls()) # ripulisce la memoria di R
# definiamo la working directory:
setwd("~/Desktop/Corso_R")
# carichiamo il file di dati:
df <- read.csv("my_data/regression_data.csv")
str(df)


## ----chap55chunk01, warning = F, message=F, results=F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Lunghezza algale misurata a diverse profondità."----
par(family="Times", # tipo di caratteri da usare
    bty="L" # tipo di "scatola" da tracciare attorno al grafico
    )
plot(alga.length ~ depth, data=df,
     xlab="Profondità [m]", # personalizzazione dei nomi degli assi
     ylab="Lunghezza algale [cm]",
     xlim=c(0,20), # limiti degli assi
     ylim=c(0,25)
     )


## ------------------------------------------------------------------------------------
lm1 <- lm(alga.length ~ depth, data=df)


## ----chap55chunk02, warning = F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello di regressione lineare $lm1$."----
par(mfrow=c(2,2))
plot(lm1)


## ------------------------------------------------------------------------------------
lm0 <- lm(alga.length ~ 1, data=df)


## ------------------------------------------------------------------------------------
anova(lm1, lm0)


## ------------------------------------------------------------------------------------
summary(lm1)


## ----chap55chunk03, warning = F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Lunghezza algale misurata a diverse profondità. La linea rappresenta i valori attesi secondo il modello $lm1$."----
par(mfrow=c(1,1),
    family="Times", # tipo di caratteri da usare
    bty="L" # tipo di "scatola" da tracciare attorno al grafico
    )
plot(alga.length ~ depth, data=df,
     xlab="Profondità [m]", # personalizzazione dei nomi degli assi
     ylab="Lunghezza algale [cm]",
     xlim=c(0,20), # limiti degli assi
     ylim=c(0,25)
     )
abline(lm1)


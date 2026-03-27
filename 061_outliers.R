
rm(list=ls()) # ripulisce la memoria di R
# definiamo la working directory:
setwd("~/Desktop/Corso_R")
# carichiamo il file di dati:
df <- read.csv("my_data/outlier_example.csv")
str(df)



## ----outlierplot1, fig.align="center", fig.width=5, fig.height=5, fig.cap="Lunghezza algale misurata a diverse profondità. La freccia indica un potenziale $outlier$."----
# rappresentazione dei dati:
par(bty="L", family="Times")
plot(alga.length ~ depth, data=df,
     # personalizzazione dei nomi degli assi:
     xlab="Profondità [m s.l.m.]", 
     ylab="Lunghezza algale [cm]",
     xlim=c(0,20), # limiti degli assi
     ylim=c(0,25),
     pch=21
     )

#freccia per indicare il potenziale outlier:
arrows(x0=8, y0=23, x1 = 5, y1 = 23, length=0.15, lwd=2, col="black")


## ------------------------------------------------------------------------------------
lm1 <- lm(alga.length ~ depth, data=df)


## ----outlierplot2, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello $lm1$."----
par(mfrow=c(2,2))
plot(lm1)


## ------------------------------------------------------------------------------------
df2 <- df[-3, ]


## ----outlierplot3, fig.align="center", fig.width=5, fig.height=5, fig.cap="Lunghezza algale misurata a diverse profondità. La freccia indica un potenziale $outlier$. La retta tratteggiata corrisponde alla retta di regressione calcolata sull'intero dataset. La retta continua corrisponde alla retta di regressione calcolata escludendo il potenziale $outlier$ dal dataset."----
# rappresentazione dei dati:
par(mfrow=c(1,1), bty="L", family="Times")
plot(alga.length ~ depth, data=df,
     xlab="Profondità [m]", # personalizzazione dei nomi degli assi
     ylab="Lunghezza algale [cm]",
     xlim=c(0,20), # limiti degli assi
     ylim=c(0,25),
     pch=21
     )

#freccia per indicare il potenziale outlier:
arrows(x0=8, y0=23, x1 = 5, y1 = 23, length=0.15, lwd=2)

# fit del modello di regressione su df2:
lm1 <- lm(alga.length ~ depth, data = df)
# retta di regressione stimata senza l'outlier:
abline(lm1, lwd=2, lty="dashed")

# fit del modello di regressione su df2:
lm2 <- lm(alga.length ~ depth, data = df2)
# retta di regressione stimata senza l'outlier:
abline(lm2, lwd=2)


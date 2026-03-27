
rm(list = ls()) # ripulisce la memoria di R

# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman')
library(pacman)
p_load(emmeans)
p_load(scatterplot3d) # per fare grafici a dispersione tridimensionali


## ----chap99chunk01, warning=F--------------------------------------------------------
setwd("~/Desktop/Corso_R")
# Carichiamo il file con i dati:
df <- read.csv("my_data/multi_reg_data.csv")
str(df)


## ----chap99chunk02, warning = F, message=F, results=F, fig.align="center", fig.width=6, fig.height=6, fig.cap="Grafico a dispersione tridimensionale."----
par(family="Times")
grafico3d <- scatterplot3d(x = df$depth, 
                           y = df$nutrients, 
                           z = df$alga.length, # assi
                  # etichette assi:
                  xlab="Profondità [m]",
                  ylab="Nutrienti [mg/L]",
                  zlab="Lunghezza algale [cm]",
                  pch=16,
                  # per evidenziare tridimensionalità:
                  highlight.3d=T,
                  # limiti degli assi:
                  xlim=c(0, max(df$depth)),
                  ylim=c(0, max(df$nutrients)),
                  zlim=c(50, max(df$alga.length)),
                  angle=40, # angolo di rotazione
                  type="p", # punti. Alternative: "l","h"
                  main="" # titolo (vuoto)
                  )


## ------------------------------------------------------------------------------------
lm.full <- lm(alga.length ~ depth * nutrients, data=df)
lm.additive <- lm(alga.length ~ depth + nutrients, data=df)
lm.depth <- lm(alga.length ~ depth, data=df)
lm.nutrients <- lm(alga.length ~ nutrients, data=df)
lm0 <- lm(alga.length ~ 1, data=df)


## ------------------------------------------------------------------------------------
cor.test(df$nutrients,y=df$depth)


## ----chap99chunk09, warning=F, message=F, results=F, tidy=F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Grafico diagnostico per valutare visivamente se ci sia collinearità tra i predittori. La linea rossa è una curva che approssima l'andamento locale medio dei punti."----
scatter.smooth(x=df$nutrients, y=df$depth,
    ylab="depth", xlab="nutrients",
    lpars = list(col = "red", lwd = 1, lty = 1)
    )


## ------------------------------------------------------------------------------------
anova(lm.full, lm.additive) 


## ------------------------------------------------------------------------------------
anova(lm.additive, lm.depth)
anova(lm.additive, lm.nutrients)


## ----tidy=F--------------------------------------------------------------------------
AIC(lm.full,
	lm.additive,
	lm.depth,
	lm.nutrients,
	lm0
	)


## ------------------------------------------------------------------------------------
summary(lm.additive)$coef


## ----eval=F--------------------------------------------------------------------------
# grafico3d$plane3d(lm.additive, lty.box = "solid")


## ----chap99chunk03, echo=F, warning=F, message=F, results=F, fig.align="center", fig.width=6, fig.height=6, fig.cap="Grafico a dispersione tridimensionale dei dati e delle predizioni del modello additivo $lm.additive$."----
par(family="Times")
s3d <- scatterplot3d(x = df$depth, y = df$nutrients, z = df$alga.length, 
					xlab="Profondità [m]",
          ylab="Nutrienti [mg/L]",
          zlab="Lunghezza algale [cm]",
					main="", # niente titolo
					type="p", pch=16, 
					highlight.3d=T,
					# limiti degli assi:
					xlim=c(0, max(df$depth)), 
					ylim=c(0, max(df$nutrients)), 
					zlim=c(min(df$alga.length)-25, max(df$alga.length)+5),
					angle=40
					)
lm.additive <- lm(alga.length ~ depth + nutrients, data=df)
s3d$plane3d(lm.additive, lty.box = "solid")



## ----chap99chunk04, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.full$."----
par(mfrow=c(2,2)); plot(lm.full)


## ----chap99chunk05, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.additive$."----
par(mfrow=c(2,2)); plot(lm.additive)


## ----chap99chunk06, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.depth$. Il grafico quantile-quantile indica non-normalità dei residui."----
par(mfrow=c(2,2)); plot(lm.depth) # non-normalità dei residui


## ----chap99chunk07, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.nutrients$. Il grafico quantile-quantile indica non-normalità dei residui."----
par(mfrow=c(2,2)); plot(lm.nutrients) # non-normalità dei residui


## ----chap99chunk08, echo=F, message=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm0$. Il grafico quantile-quantile è sospetto: non-normalità dei residui?"----
par(mfrow=c(2,2)); plot(lm0) # non-normalità dei residui?


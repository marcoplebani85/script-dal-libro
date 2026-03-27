## ----warning = F, message=F, results=F-----------------------------------------------
rm(list=ls()) # ripulisce la memoria di R
setwd("~/Desktop/Corso_R")


## ----include=F, warning = F, message=F, results=F------------------------------------
if (!require(formatR)) install.packages('formatR'); library(formatR)
if (!require(knitr)) install.packages('knitr'); library(knitr)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=65),tidy=TRUE, dpi=200)

options(width = 65)


## ----echo=F, message=F, results=F, warning=F-----------------------------------------
setwd("~/Desktop/Corso_R")
df <- read.delim("my_data/compare_6_species.txt")


## ------------------------------------------------------------------------------------
str(df) # struttura dei dati


## ------------------------------------------------------------------------------------
df$Species <- as.factor(df$Species)
class(df$Species)
levels(df$Species)


## ----chap44chunk01, warning = F, tidy=T, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Distribuzione delle masse corporee delle specie A, B, C, D, E, ed F."----
par(family="Times", # tipo di caratteri da usare
    # tipo di "scatola" da tracciare attorno al grafico:
    bty="L" # indica di tracciare solo gli assi 
    # anzichè un rettangolo completo
    )
boxplot(body.mass ~ Species, data=df, 
    col="white", # colore di riempimento delle "scatole"
    ylim=c(0,70), # limiti dell'asse y
    # etichette personalizzate per gli assi:
    xlab="Specie", 
    ylab="Massa corporea [Kg]"
    )


## ------------------------------------------------------------------------------------
m1 <- lm(body.mass ~ Species, data=df)


## ----chap44chunk02, warning = F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello $m1$."----
par(mfrow=c(2, 2)) # 2 righe, 2 colonne
plot(m1)


## ------------------------------------------------------------------------------------
m0 <- lm(body.mass ~ 1, data=df)

anova(m1, m0)


## ------------------------------------------------------------------------------------
summary(m1)


pwt <- pairwise.t.test(x = df$body.mass, 
            g = df$Species,
            p.adjust.method="bonferroni")
pwt


# install.packages("emmeans")
library(emmeans)


## ----tidy=F--------------------------------------------------------------------------
emmeans.test <- emmeans(m1, 
		pairwise ~ Species,
		adjust = "bonferroni"
		)
emmeans.test


## ----eval=F, warning = F, tidy=F-----------------------------------------------------
# write.csv(emmeans.test$contrasts,
#           file="pairwise_tests.csv",
#           row.names=F)


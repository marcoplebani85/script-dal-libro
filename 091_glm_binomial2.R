

rm(list=ls()) # ripuliamo la memoria di R
options(scipen=0)
# carichiamo il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
# pacchetti necessari:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)
p_load(performance)
# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")
# Carichiamo i dati in R:
df <- read.csv("my_data/egg_predation.csv")
str(df)


## ------------------------------------------------------------------------------------
df$predation.proportion <- df$egg.predation/df$brood.size


## ----glmBinom2, fig.align="center", fig.width=5, fig.height=5, fig.cap="Proporzione di uova soggette a predazione in relazione alla copertura di vegetazione."----
par(bty="L", family="Times") # parametri da applicare a plot()
plot(jitter(predation.proportion, 1) ~ jitter(vegetation.cover, 0.3),
     data=df,
     # caratteristiche dei punti:
     pch=21, cex=2, bg=grey(0.7, alpha=0.5),
     # etichette degli assi:
     ylab="Frazione di uova soggette a predazione", 
     xlab="Copertura di vegetazione [%]"
     )


## ----warning=F, tidy=F---------------------------------------------------------------
glm.egg <- glm(predation.proportion ~ vegetation.cover,
	data = df,
	# "peso" di ciascuna proporzione osservata:
	weights = brood.size, 
	family = binomial(link="logit")
	)


## ----glmBinom2diagnostics1, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Grafici diagnostici per $glm.egg$."----
# grafici diagnostici:
glm.eggResids <- simulateResiduals(glm.egg)
par(mfrow=c(1,2))
plotQQunif(glm.eggResids)
mtext(text="(a)", side=3, adj=0, line=3)
plotResiduals(glm.eggResids, quantreg = T)
mtext(text="(b)", side=3, adj=0, line=3)


## ----warning=F-----------------------------------------------------------------------
glm.egg0 <- glm(predation.proportion ~ 1,
	data = df,
	weights = brood.size, # "peso" di ciascuna proporzione osservata
	family = binomial(link="logit")
	)
anova(glm.egg, glm.egg0, test="Chisq")


## ------------------------------------------------------------------------------------
performance::r2(glm.egg)


## ----warning=F-----------------------------------------------------------------------
coef(glm.egg)


## ----message=F, warning = F, results=F-----------------------------------------------
p_load(ggeffects)
glm.egg.ggpred <- ggemmeans(glm.egg, terms=c("vegetation.cover [n=50]"))


## ----glmBinom2pred, tidy=F, message=F, warning = F, results=F, fig.align="center", fig.width=5, fig.height=4.5, fig.cap="Predizioni del modello $glm.egg$."----
plot(glm.egg.ggpred, show_data=T) +
  labs(y="Frazione di uova \n soggette a predazione",
       x="Copertura di vegetazione [%]",
       title="") +
  theme_classic() +
  theme(text = element_text(family="Times", size=13))


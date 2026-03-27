
rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
# altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa) # produce test e grafici diagnostici per GLM e mixed-effect models
p_load(performance)
# le funzioni di performance permettono di calcolare vari indici di qualità dei modelli

# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")
# Carichiamo i dati in R:
df <- read.csv("my_data/count_data2.csv")
str(df)


## ----glmPoiss2, warning=F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Variabilità del numero di uova deposte in relazione alla massa corporea della madre."----
par(family="Times", bty="L")
plot(n.progeny ~ body.mass, data=df,
     # tipo di punto, colore, dimensione:
     pch=19, col=grey(0.5, alpha=0.5), cex=1.5,
     # etichette assi:
     ylab="Numero di uova deposte", xlab="Massa corporea [g]"
     )


## ----glmPoiss2lmresid, message=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm1$."----
lm1 <- lm(n.progeny ~ body.mass, data=df)
par(mfrow=c(2,2))
plot(lm1)


## ----tidy=F--------------------------------------------------------------------------
glm1 <- glm(n.progeny ~ body.mass, data=df, 
            family=poisson(link="identity"))


## ----glmPoiss2glmresid, message=F, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Grafici diagnostici per $glm1$."----
glm1Resids <- simulateResiduals(glm1)
par(mfrow=c(1,2))
plotQQunif(glm1Resids)
mtext(text="(a)", side=3, adj=0, line=3)
plotResiduals(glm1Resids, quantreg = T)
mtext(text="(b)", side=3, adj=0, line=3)


## ------------------------------------------------------------------------------------
# modello nullo:
glm0 <- glm(n.progeny ~ 1, data=df, family=poisson(link="identity"))

# confronto tra la variabilità residua di glm1 e glm0:
anova(glm1, glm0, test="Chisq")


## ----glmPoiss2glmVSlm, message=F, warning = F, results=F, tidy=F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Valori osservati e valori attesi secondo i modelli $glm1$ (linea continua rossa, intervallo di confidenza in rosso) e $lm1$ (linea tratteggiata nera, intervallo di confidenza in grigio)."----
ggplot(df, aes(body.mass, n.progeny)) +
  geom_point(shape=19, color=grey(level=0.5, alpha=0.5), size=3) +
  geom_smooth(method = "glm", 
              method.args=list(family = poisson(link="identity")), 
              se=T, # per ottenere l'int. di conf. al 95%
              color="red", fill="red") +
  geom_smooth(method = "lm",
              se=T,
              color="black", fill="grey30", lty="dashed") +
  labs(x = "Massa corporea [g]", y = "Numero di uova deposte") +
  theme_classic() +
  theme(text = element_text(family="Times", size=13))


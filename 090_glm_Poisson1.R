

rm(list=ls()) # ripulisce la memoria di R
options(scipen=9999) # evita di esprimere numeri come potenze di 10

# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)

# altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)
# DHARMa produce test e grafici diagnostici per GLM e mixed-effect models
p_load(performance)
# le funzioni di performance permettono di calcolare vari indici di qualità dei modelli

# definiamo la directory di lavoro. In R:
setwd("~/Desktop/Corso_R")
# In RSTUDIO: menu -> Session -> Set working directory

# Carichiamo i dati in R:
df <- read.csv("my_data/count_data.csv")
str(df)



## ----glmPoiss1, warning=F, tidy=F, fig.align="center", fig.width=6.5, fig.height=4, fig.cap="Numero di uova deposte in relazione alla massa corporea della madre (a), e gli stessi dati rappresentati usando il logaritmo del numero di uova deposte + 1 (b)."----
par(mfrow=c(1,2),
    # tipo di "scatola" da tracciare attorno ai grafici:
    bty="L",
    family="Times" # font type
    )
plot(n.progeny ~ body.mass, data=df,
     # tipo di punto, colore, dimensione:
     pch=19, col=grey(0.5, alpha=0.5), cex=1.5,
     # etichette assi:
     ylab="Numero di uova deposte", 
     xlab="Massa corporea [g]"
     )
mtext(expression(paste("(", bold(a),")")), 
      side = 3, line = 0.3, adj = 0, family="Times")
plot(log(n.progeny + 1) ~ body.mass, data=df,
     pch=19, col=grey(0.5, alpha=0.5), cex=1.5,
     ylab="log(Numero di uova deposte + 1)", 
     xlab="Massa corporea [g]"
     )
mtext(expression(paste("(", bold(b),")")), 
      side = 3, line = 0.3, adj = 0, family="Times")


## ----error=T-------------------------------------------------------------------------
lm.log <- lm(log(n.progeny) ~ body.mass, data=df)


## ----glmPoiss1logresid, message=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.log$."----
lm.log <- lm(log(n.progeny+1) ~ body.mass, data=df)
# grafici diagnostici:
par(mfrow=c(2,2))
plot(lm.log)


## ----tidy=F--------------------------------------------------------------------------
glm1 <- glm(n.progeny ~ body.mass, data=df,
            family=poisson(link="log"))


## ----glmPoiss1glmresid, message=F, warning = F, results=F, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Grafici diagnostici per $glm1$."----
# simulazione dei residui attesi:
glm1Resids <- simulateResiduals(glm1)
par(mfrow=c(1,2))
# confronto tra residui osservati e quelli attesi secondo una distribuzione poissoniana:
plotQQunif(glm1Resids)
mtext(text="(a)", side=3, adj=0, line=3)
# andamento dei residui simulati rispetto ai valori medi stimati dal modello:
plotResiduals(glm1Resids, quantreg = T)
mtext(text="(b)", side=3, adj=0, line=3)


## ------------------------------------------------------------------------------------
# modello nullo:
glm0 <- glm(n.progeny ~ 1, data=df, 
            family=poisson(link="log"))

# confronto tra la variabilità residua di glm1 e glm0:
anova(glm1, glm0, test="Chisq")


## ------------------------------------------------------------------------------------
summary(glm1)


## ------------------------------------------------------------------------------------
performance::r2_mcfadden(glm1)

## ----r2mcfadden, echo=F--------------------------------------------------------------
r2mcfadden <- round(as.numeric(performance::r2_mcfadden(glm1)[2])*100,1)


## ----tidy=F--------------------------------------------------------------------------
emmeans(glm1, 
        specs = ~body.mass, 
        at=list(body.mass=c(5,10,15,20)), 
        type="response")


## ----glmPoiss1glm, message=F, warning = F, results=F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Valori osservati e valori attesi secondo il modello $glm1$."----
ggplot(df, aes(body.mass, n.progeny)) +
  geom_point(shape=19, color=grey(level=0.5, alpha=0.5), size=3) +
  geom_smooth(method = "glm", 
              method.args=list(family = "poisson"), 
              se=T, # int. conf. al 95%
              color="black") +
  labs(x = "Massa corporea [g]", y = "Numero di uova deposte") +
  theme_classic() +
  theme(text = element_text(family="Times", size=13))


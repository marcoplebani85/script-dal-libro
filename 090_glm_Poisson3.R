
rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
# altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)
# DHARMa produce test e grafici diagnostici per GLM e mixed-effect models
p_load(performance)
# le funzioni di performance permettono di calcolare vari indici di qualità dei modelli

# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")
# Carichiamo i dati in R:
df <- read.csv("my_data/count_data_2groups.csv")
str(df)


## ----glmPoiss3, message=F, warning = F, results=F, echo=F, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Variabilità del numero di uova deposte in relazione alla specie e distribuzioni gaussiane corrispondenti."----
eggs.hist <- ggplot(data=df, aes(x=n.eggs, fill=Species)) + # informazioni base
  geom_histogram(alpha = 0.4, position = "identity", breaks=c(0:20), color="black",
                 aes(y = ..density..)) + 
	labs(x = "Numero di uova deposte", y = "Frequenza", fill="Specie:") +
	scale_x_continuous(limits = c(-5, 15)) +
  scale_fill_grey(labels=c("Specie 1", "Specie 2")) +
  theme_classic() +
  theme(text = element_text(family = "Times"))

# aggiungiamo due curve gaussiane con media e varianza pari a quelle di ciascun gruppo:
eggs.hist + # grafico a cui aggiungere le curve
  stat_function(fun = dnorm, # tipo di distribuzione (Gaussiana) 
                # media e deviazione standard:
                args = list(mean = mean(df[df$Species=="Species1",]$n.eggs), 
                            sd = sd(df[df$Species=="Species1",]$n.eggs)), 
                color="black") +
  stat_function(fun = dnorm, # tipo di distribuzione (Gaussiana) 
                # media e deviazione standard:
                args = list(mean = mean(df[df$Species=="Species2",]$n.eggs), 
                            sd = sd(df[df$Species=="Species2",]$n.eggs)), 
                color="black", lty="dashed")


## ----tidy=F--------------------------------------------------------------------------
glm1 <- glm(n.eggs ~ Species, data=df, 
            family=poisson(link="log"))


## ----glmPoiss3glmresid, message=F, warning = F, results=F, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Grafici diagnostici per $glm1$."----
glm1Resids <- simulateResiduals(glm1)
par(mfrow=c(1,2))
plotQQunif(glm1Resids)
mtext(text="(a)", side=3, adj=0, line=3)
plotResiduals(glm1Resids, quantreg = T)
mtext(text="(b)", side=3, adj=0, line=3)


## ------------------------------------------------------------------------------------
glm0 <- glm(n.eggs ~ 1, data=df, family=poisson(link="log"))
anova(glm1, glm0, test="Chisq")


## ------------------------------------------------------------------------------------
emmeans(glm1,  ~ Species, type = "response")


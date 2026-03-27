
rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
# altri pacchetti necessari:
p_load(emmeans)
p_load(ggplot2)
p_load(DHARMa)
p_load(performance)
# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")
# Carichiamo i dati in R:
df <- read.csv("my_data/stress_death.csv")
str(df)



## ----glmBinom1, fig.align="center", fig.width=5, fig.height=5, fig.cap="Mortalità degli alberi in relazione allo stress idrico (0: vivo; 1: morto). Un pizzico di rumore artificiale è stato aggiunto alle coordinate delle misurazioni per evitare che si sovrapponessero completamente."----
par(bty="L", family="Times")
plot(jitter(death.01, 0.1) ~ jitter(stress.per100),
	data= df,
	pch=21, cex=2, # tipo di punto da usare
	bg=grey(0.7, alpha=0.5), # colore
	xlab="Stress idrico [%]",
	ylab="Mortalità",
	xlim=c(0,100)
	)


## ----tidy=F--------------------------------------------------------------------------
glm.binom1 <- glm(death.01 ~ stress.per100,
                  data= df,
                  family=binomial(link="logit")
                  )


## ----glmBinom1diagnostics, message=F, fig.align="center", fig.width=6.5, fig.height=4.5, fig.cap="Grafici diagnostici per $glm.binom1$."----
glm.binom1Resids <- simulateResiduals(glm.binom1)
par(mfrow=c(1,2))
plotQQunif(glm.binom1Resids)
mtext(text="(a)", side=3, adj=0, line=3)
plotResiduals(glm.binom1Resids, quantreg = T)
mtext(text="(b)", side=3, adj=0, line=3)


## ----tidy=F--------------------------------------------------------------------------
glm.binom0 <- glm(death.01 ~ 1, data= df,
	family=binomial(link="logit")
	)

anova(glm.binom1, glm.binom0, test="Chisq")


## ------------------------------------------------------------------------------------
performance::r2(glm.binom1)


## ----glmbinom1r2, echo=F, warning=F, message=F---------------------------------------
glmbinom1r2 <- round(as.numeric(r2(glm.binom1))*100, 1)


## ----warning=F-----------------------------------------------------------------------
coef(glm.binom1)


## ------------------------------------------------------------------------------------
pred.df <- emmeans(glm.binom1, # modello
    specs = ~stress.per100, # predittori da considerare
    # valori del predittore per i quali ottenere predizioni:
    at=list(stress.per100=0:100),
    type="response" # scala delle predizioni
    )
head(pred.df)




## ----glmBinom1pred, message=F, warning = F, results=F, echo=F, fig.align="center", fig.width=5, fig.height=8.5, fig.cap="Tre rappresentazioni delle predizioni del modello $glm.binom1$."----
p_load(ggpubr)
p_load(ggeffects)
glm.binom1.ggpred <- ggpredict(glm.binom1, terms=c("stress.per100"))
g1 <- plot(glm.binom1.ggpred)

glm.binom1.ggpred <- ggpredict(glm.binom1, terms=c("stress.per100 [n=50]"))
g2 <- plot(glm.binom1.ggpred)

g3 <- plot(glm.binom1.ggpred, show_data=T, # predizioni e osservazioni
     dot_alpha = 0.1, dot_size=4) +
  labs(y="Mortalità",
       x="Stress idrico [%]",
       title="") +
  theme_classic() +
  theme(text = element_text(family="Times", size=12))

ggarrange(g1, g2, g3, # grafici da rappresentare insieme
							labels = c("(a)", "(b)", "(c)"), # numerazione dei grafici
							ncol = 1, nrow = 3 # disposizione: un rigo, tre colonne
							)


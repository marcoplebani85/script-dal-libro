
rm(list = ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman":
if (!require(pacman)) install.packages('pacman'); library(pacman)
# In pacman c'è la funzione p_load() che carica pacchetti previo installazione, se necessaria. Io lo uso sempre al posto di library().

p_load(ggplot2) # per produrre grafici complessi
p_load(emmeans) # per stimare i parametri e le predizioni del modello




## ----echo = F, message=F, warning=F--------------------------------------------------
# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")

# Carichiamo i dati in R:
df <- read.csv("my_data/ANCOVA_data.csv")


## ------------------------------------------------------------------------------------
df$Species <- as.factor(df$Species)


## ----ancova01, tidy=F, message=F, results=F, warning=F, echo=T, fig.align="center", fig.width=6, fig.height=4, fig.cap="Lunghezza delle specie algali A e B a profondità di 5-21 m sotto il livello del mare."----
ggplot(data=df, aes(x=depth, y=alga.length, color=Species)) +
  geom_point() +
  labs(x = "Profondità [m]", y = "Lunghezza algale [cm]") + 
  theme_classic()


## ----message=F, warning=F------------------------------------------------------------
lm.interactive <- lm(alga.length ~ Species * depth, data=df)


## ----eval=F--------------------------------------------------------------------------
# alga.length ~ Species + depth + Species:depth


## ----message=F, warning=F------------------------------------------------------------
lm.additive <- lm(alga.length ~ Species + depth, data=df)


## ----message=F, warning=F------------------------------------------------------------
lm.only.species <- lm(alga.length ~ Species, data=df)
lm.only.depth <- lm(alga.length ~ depth, data=df)
lm.null <- lm(alga.length ~ 1, data=df)


## ------------------------------------------------------------------------------------
anova(lm.interactive, lm.additive)


## ------------------------------------------------------------------------------------
AIC(lm.interactive,
	lm.additive,
	lm.only.species,
	lm.only.depth,
	lm.null
	)


## ------------------------------------------------------------------------------------
anova(lm.only.species, lm.only.depth)


## ------------------------------------------------------------------------------------
AIC(lm.only.species, lm.only.depth)


## ----lm_interactive_R2, echo=F-------------------------------------------------------
lm_interactive_R2 <- round(summary(lm.interactive)$adj.r.squared, 3)
lm_interactive_R2percent <- lm_interactive_R2*100


## ------------------------------------------------------------------------------------
pendenze <- emtrends(lm.interactive, # modello di interesse
      pairwise ~ Species, # predittori categorici
      var = "depth") # predittore continuo
pendenze


## ------------------------------------------------------------------------------------
emmeans::test(pendenze)


## ----tidy=F--------------------------------------------------------------------------
emmeans(lm.interactive, 
			specs = ~ Species * depth,
			at = list(depth = 0)
			)


## ----tidy=F--------------------------------------------------------------------------
emmeans(lm.interactive,
        specs = ~ Species * depth,
        at = list(depth = c(5, 20))
        )


## ----warning = F, message=F, results=F-----------------------------------------------
df$Specie <- ifelse(df$Species=="Species A", "A", "B")



## ----figAncova, echo=FALSE, out.width="95%", fig.align="center", fig.cap="Dati e predizioni per il modello con interazione."----
knitr::include_graphics("~/MEGA/MEGAsync/rlunch-archive2/First steps in data analysis with R/miei_script ITA ANNOTATI/Il libro di R/FIGURE/fig_ancova.png")


## ----ancovaDiagn01, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="I grafici diagnostici per $lm.interactive$ non suggeriscono violazioni delle assunzioni."----
par(mfrow=c(2,2)) ; plot(lm.interactive) # tutto bene


## ----ancovaDiagn02, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.additive$. Il grafico $residuals$ vs $fitted$ suggerisce una relazione non-lineare tra i residui ed i valori stimati."----
par(mfrow=c(2,2)) ; plot(lm.additive) # residuals vs fitted ha l'aria terribile - non-linearità?


## ----ancovaDiagn03, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="I grafici diagnostici per $lm.only.species$ suggeriscono non-normalità ed eteroscedasticità dei residui."----
par(mfrow=c(2,2)) ; plot(lm.only.species) # residuals vs fitted ha l'aria terribile - eteroscedasticità?


## ----ancovaDiagn04, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.only.depth$. Il grafico $Scale-Location$ indica eteroscedasticità dei residui."----
par(mfrow=c(2,2)) ; plot(lm.only.depth) # Il grafico residuals vs fitted ha un'aria terribile: eteroscedasticità?


## ----ancovaDiagn05, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.null$. Il Q-Q plot indica che i residui non seguono una distribuzione di frequenza normale."----
par(mfrow=c(2,2)) ; plot(lm.null) # Il Q-Q plot mostra un possibile scostamento dalla normalità


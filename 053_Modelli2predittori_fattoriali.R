
rm(list = ls()) # ripuliamo la memoria di R

# carichiamo "pacman" per poter usare p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)

# carichiamo i pacchetti che ci serviranno:
p_load(ggplot2)
p_load(ggpubr)
p_load(emmeans)




## ----warning = F---------------------------------------------------------------------
setwd("~/Desktop/Corso_R")
df <- read.csv("my_data/2way_ANOVA_data.csv")
str(df)


## ------------------------------------------------------------------------------------
df$Species <- as.factor(df$Species)
df$Treatment <- as.factor(df$Treatment)


## ----warning = F, tidy=F, message=F, results=F---------------------------------------
basegrafico <- ggline(df, # provenienza dei dati
	x = "Treatment", y = "body.mass", color = "black",
	add=c("jitter", "mean"),
	# indicazioni su come tracciare 
	# le linee che collegano i valori medi:
	plot_type="l", linetype="Species",
	# forma, dimensione e colore dei punti:
	add.params = list(shape = 21, size=2, fill = "Species"))


## ----figanova2, warning = F, message=F, results=F, fig.align="center", tidy=F, fig.width=6, fig.height=4, fig.cap="Masse corporee di individui appartenenti alle specie A e B esposti a due diete sperimentali. Ciascun punto rappresenta una singola osservazione."----
basegrafico +
  ylim(0,105) + # limiti dell'asse y
  # etichettiamo gli assi:
	labs(y = "Massa corporea [kg]", x="Trattamento") + 
	theme_classic() +
  # per definire manualmente i colori da usare:
	scale_fill_manual("Specie:", values = c("grey45", "white")) +
  scale_linetype_discrete(name = "Specie:") +
  scale_x_discrete(labels=c("Dieta 1", "Dieta 2")) +
  # tipo e dimensione dei caratteri:
	theme(text = element_text(family = "Times")) + 
	theme(text = element_text(size = 14))


## ------------------------------------------------------------------------------------
lm.only.species <- lm(body.mass ~ Species, data=df)


## ------------------------------------------------------------------------------------
lm.interactive <- lm(body.mass ~ Species * Treatment, data=df)
lm.additive <- lm(body.mass ~ Species + Treatment, data=df)
lm.only.Treatment <- lm(body.mass ~ Treatment, data=df)
lm.null <- lm(body.mass ~ 1, data=df)


## ------------------------------------------------------------------------------------
anova(lm.interactive, lm.additive)


## ------------------------------------------------------------------------------------
anova(lm.additive, lm.only.Treatment)


## ------------------------------------------------------------------------------------
anova(lm.additive, lm.only.species)


## ------------------------------------------------------------------------------------
anova(lm.only.Treatment, lm.null)


## ------------------------------------------------------------------------------------
anova(lm.only.species, lm.null)


## ------------------------------------------------------------------------------------
AIC(lm.interactive,
    lm.additive,
    lm.only.species,
    lm.only.Treatment,
    lm.null)


## ------------------------------------------------------------------------------------
summary(lm.only.species)$adj.r.squared


## ------------------------------------------------------------------------------------
emmeans(lm.only.species, # modello di interesse
        pairwise ~ Species, # predittori per cui fare stime
        adjust = "bonferroni" # correzione per test multipli
        )


## ----anova2Diagn01, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.interactive$."----
par(mfrow=c(2,2)) ; plot(lm.interactive)


## ----anova2Diagn02, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.additive$."----
par(mfrow=c(2,2)) ; plot(lm.additive)


## ----anova2Diagn03, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.only.species$."----
par(mfrow=c(2,2)) ; plot(lm.only.species)


## ----anova2Diagn04, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.only.Treatment$."----
par(mfrow=c(2,2)) ; plot(lm.only.Treatment)


## ----anova2Diagn05, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.null$."----
par(mfrow=c(2,2)) ; plot(lm.null)


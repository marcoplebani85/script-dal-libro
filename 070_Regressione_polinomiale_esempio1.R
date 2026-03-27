
rm(list = ls()) # ripulisce la memoria di R

# carica "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
p_load(emmeans)
p_load(ggplot2)
p_load(readxl)  # per caricare dati da file Excel



## ----warning = F, message=F, results=F, tidy=F---------------------------------------
# definiamo la directory di lavoro. In R:
setwd("~/Desktop/Corso_R")
# In RSTUDIO: menu -> Session -> Set working directory
df <- as.data.frame(
  readxl::read_excel("my_data/non_linear_datasets.xlsx", sheet=1)
  )


## ------------------------------------------------------------------------------------
str(df)


## ----polyplot1, fig.align="center", fig.width=5, fig.height=5, fig.cap="Andamento di $y$ rispetto ad $x$."----
par(bty="L", family="Times")
plot(y ~ x, data = df)


## ----echo=F--------------------------------------------------------------------------
lm1 <- lm(y ~ x, data=df)


## ------------------------------------------------------------------------------------
lm.poly <- lm(y ~ poly(x, degree=2, raw=T), data=df)


## ------------------------------------------------------------------------------------
AIC(lm.poly, lm1)
anova(lm.poly, lm1)


options(scipen=0) # scipen attiva o disattiva la notazione scientifica
summary(lm.poly)


## ----polyplot4, message=F, results=F, fig.align="center", fig.width=5, fig.height=4, fig.cap="Dati e predizioni del modello di regressione polinomiale. L'area grigia rappresenta l'intervallo di confidenza del modello."----
ggplot(df, aes(x=x, y=y)) + # informazioni base
  geom_point(shape=1, size=2) +
  theme_classic() +
  # scelta del carattere di testo da usare:
  theme(text = element_text(family="Times", size=15)) +
  # predizioni del modello:
	geom_smooth(method = "lm", formula = y ~ poly(x,2), 
	            color="black", linewidth=0.5)


## ----polyplot2, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello $lm1$."----
# grafici diagnostici:
par(mfrow=c(2,2)) ; plot(lm1)


## ----polyplot3, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per il modello $lm.poly$."----
# grafici diagnostici:
par(mfrow=c(2,2)) ; plot(lm.poly)


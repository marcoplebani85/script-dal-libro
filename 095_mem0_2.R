

rm(list=ls()) # ripulisce la memoria di R
# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)
# altri pacchetti:
p_load(doBy)
p_load(emmeans)
p_load(ggplot2)
# per rappresentare le predizioni dei modelli misti in ggplot2:
p_load(ggeffects)
# per produrre test e grafici diagnostici per GLM e mixed-effect models:
p_load(DHARMa, performance, see)
p_load(lme4, lmerTest, glmmTMB) # per fittare modelli misti
p_load(MuMIn) # per stimare lo pseudo-R-quadro dei modelli misti
p_load(RCurl)

# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")

# Carichiamo i dati in R:
dd <- read.csv(text = RCurl::getURL(
"https://raw.githubusercontent.com/marcoplebani85/datasets/master/mixed_effect_data1.csv")
  )
# getURL() permette di accedere a dati online

## ----mem1, message=F, warning = F, results=F, fig.align="center", fig.width=6, fig.height=5, fig.cap="Dati e predizioni per ciascun trattamento in ciascun paese."----
# calcoliamo media e deviazione standard per ciascun trattamento in ciascun paese:
dd_smr <- summaryBy(yield ~ fertilizer * country, data=dd, FUN=c(mean, sd))
# cambiamo i nomi delle colonne di dd_smr:
names(dd_smr) <- c("fertilizer", "country", "yield", "sd")

# rappresentiamo graficamente dd_smr:
ggplot(data=dd_smr, aes(x=fertilizer, y=yield, fill=country)) +
  geom_bar(stat="identity", color="black", # istogramma
           position=position_dodge()) + 
  geom_errorbar(aes(ymin=yield-sd, ymax=yield+sd), # barre d'errore
                width=.2, position=position_dodge(.9)) +
    # etichettiamo gli assi:
  labs(x = "Tipo di fertilizzante", y = "Raccolto [Kg]") +
	scale_fill_grey(name = "Paese") + theme_classic() +
  theme(text = element_text(family="Times", size=15))


## ----warning = F, tidy=F-------------------------------------------------------------
mem1 <- lmerTest::lmer(yield ~ fertilizer + (1 | country), 
                       data = dd,
                       REML=F)


## ----mem1diagn, warning = F, fig.align="center", fig.width=6.5, fig.height=5, fig.cap="Grafici diagnostici per $mem1$ usando il pacchetto $DHARMa$."----
# grafici diagnostici usando le funzioni di DHARMa:
mem1Resids <- simulateResiduals(mem1, n=3000, use.u=F)
par(mfrow = c(1, 2))
plotQQunif(mem1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 3)
plotResiduals(mem1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 3)


## ----mem1diagn2, message=F, warning = F, results=F, tidy=F, fig.align="center", fig.width=6, fig.height=8, fig.cap="Grafici diagnostici per $mem1$ usando il pacchetto $performance$."----
performance::check_model(mem1, 
  check=c("qq", "linearity", "homogeneity", "outliers", "reqq"))


## ----warning = F, tidy=F-------------------------------------------------------------
check_heteroscedasticity(mem1)
check_homogeneity(mem1)
check_outliers(mem1)


## ------------------------------------------------------------------------------------
# modello senza effetti fissi:
mem_ranef <- lmerTest::lmer(yield ~ 1 + (1 | country), 
                       data = dd,
                       REML=F)
# modello nullo:
lm0 <- lm(yield ~ 1, data = dd)


## ------------------------------------------------------------------------------------
# confronto tramite AIC:
AIC(mem1, mem_ranef, lm0)


## ----warning = F, tidy=F-------------------------------------------------------------
mem1reml <- lmerTest::lmer(yield ~ fertilizer + (1 | country), 
                       data = dd,
                       REML=T)



summary(mem1reml)



emmeans(mem1reml, pairwise ~ fertilizer, adjust = "bonferroni")



performance::r2(mem1reml)



predizioni <- ggemmeans(model=mem1reml, terms=c("fertilizer"))


## ----mem1fixef, message=F, warning = F, results=F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Predizioni per $mem1reml$."----
plot(predizioni) + # predizioni
  # fronzoli:
  labs(x="Tipo di fertilizzante",
       y="Raccolto (Kg)",
       title="") +
  theme_classic() +
  theme(text = element_text(family="Times", size=15))


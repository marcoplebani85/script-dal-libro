
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
p_load(MuMIn) # per stimare l'R-quadro dei modelli misti
p_load(RCurl)


# definiamo la directory di lavoro:
setwd("~/Desktop/Corso_R")

# Carichiamo i dati in R:
dd <- read.csv(text = RCurl::getURL(
"https://raw.githubusercontent.com/marcoplebani85/datasets/master/mixed_effect_data3.csv")
  )
# getURL() permette di accedere a dati online


if (!require(formatR)) install.packages('formatR'); library(formatR)
if (!require(knitr)) install.packages('knitr'); library(knitr)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80),tidy=TRUE)

options(width = 80)


## ----mem3, message=F, warning = F, results=F, tidy=F, fig.align="center", fig.width=6, fig.height=5, fig.cap="Dati e predizioni secondo una regressione lineare per ciascuna località."----
ggplot(dd, aes(x = Depth, y = Alga_length, colour = Locality)) +
  geom_point() + geom_smooth(method="lm") +
  labs(x = "Profondità [m]", y = "Lunghezza algale [mm]") + # etichette assi
  theme_classic() +
  scale_colour_discrete(name = "Località") +
  theme(text = element_text(family="Times", size=15))


## ----warning = F, tidy=F-------------------------------------------------------------
mem1 <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality), 
                data = dd, REML=F)


## ----mem3diagn, warning = F, fig.align="center", fig.width=6.5, fig.height=5, fig.cap="Grafici diagnostici per $mem1$."----
# grafici diagnostici usando le funzioni di DHARMa:
mem1Resids <- simulateResiduals(mem1, n=3000, use.u=F)
par(mfrow = c(1, 2))
plotQQunif(mem1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 3)
plotResiduals(mem1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 3)


## ----mem3diagn2, warning = F, message=F, tidy=F, fig.align="center", fig.width=6, fig.height=9, fig.cap="Grafici diagnostici per $mem1$ usando il pacchetto $performance$."----
performance::check_model(mem1, 
  check=c("qq", "linearity", "homogeneity", "outliers", "reqq"))


## ------------------------------------------------------------------------------------
check_heteroscedasticity(mem1)
check_homogeneity(mem1)
check_outliers(mem1)


## ----warning = F, tidy=F-------------------------------------------------------------
mem_fixef_ri <- lmerTest::lmer(Alga_length ~ Depth + (1 | Locality), 
                data = dd, REML=F)
mem_rs <- lmerTest::lmer(Alga_length ~ 1 + (1 + Depth | Locality), 
                data = dd, REML=F)
mem_ri <- lmerTest::lmer(Alga_length ~ 1 + (1 | Locality), 
                data = dd, REML=F)
lm1 <- lm(Alga_length ~ Depth, data = dd)
lm0 <- lm(Alga_length ~ 1, data = dd)
AIC(mem1, mem_fixef_ri, mem_rs, mem_ri, lm1, lm0)


## ----tidy=F--------------------------------------------------------------------------
mem1reml <- lmerTest::lmer(Alga_length ~ Depth + (1 + Depth | Locality), 
                data = dd, REML=T)


## ----warning = F---------------------------------------------------------------------
summary(mem1reml)


## ----warning = F---------------------------------------------------------------------
performance::r2(mem1reml)


## ----mem3fixef, message=F, warning = F, results=F, tidy=F, fig.align="center", fig.width=6, fig.height=5, fig.cap="Predizioni di $mem1$."----
plot(ggemmeans(mem1reml, terms=c("Depth")), show_data=T) +
labs(y="Lunghezza algale [mm]",
  x="Profondità [m]",
  title="") +
theme_classic() +
theme(text = element_text(family="Times", size=15))


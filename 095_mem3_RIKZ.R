

rm(list = ls()) # ripulisce la memoria di R
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



RIKZ <- read.csv(text = RCurl::getURL(
"https://raw.githubusercontent.com/marcoplebani85/datasets/master/RIKZ.csv"))
# getURL() permette di accedere a dati online
str(RIKZ)
# "Exposure" in realtà è un fattore:
RIKZ$Exposure <- as.factor(RIKZ$Exposure)


## ----mem4, echo = F, message = F, warning = F, tidy=F, fig.align = "center", fig.width = 6, fig.height = 5, fig.cap = "Dati e predizioni per ciascun livello di $Exposure$ in base a modelli lineari generalizzati con distribuzione poissoniana dei residui."----
ggplot(RIKZ, aes(x = NAP, y = Richness, shape = Exposure, linetype = Exposure)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              color = "black", size = 0.5) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(1,4,2)) +
  labs(x = "Altezza (NAP) [m]",
       y = "Ricchezza di specie") +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 15))



glm1 <- glm(Richness ~ NAP*Exposure,
                family = "poisson",
                data = RIKZ)



mem1 <- glmmTMB(Richness ~ NAP*Exposure + (1 | Beach),
                family = "poisson",
                data = RIKZ, 
                REML = F)


## ----mem4diagn, warning = F, fig.align = "center", fig.width = 6.5, fig.height = 5, fig.cap = "Grafici diagnostici per $mem1$."----
# grafici diagnostici usando le funzioni di DHARMa:
mem1Resids <- simulateResiduals(mem1, n = 3000, use.u = F)
par(mfrow = c(1, 2))
plotQQunif(mem1Resids)
mtext(text = "(a)", side = 3, adj = 0, line = 3)
plotResiduals(mem1Resids, quantreg = T)
mtext(text = "(b)", side = 3, adj = 0, line = 3)



mem1_additivo <- glmmTMB(Richness ~ NAP+Exposure + (1 | Beach),
                family = "poisson",
                data = RIKZ, REML = F)
mem1_NAP <- glmmTMB(Richness ~ NAP + (1 | Beach),
                family = "poisson",
                data = RIKZ, REML = F)
mem1_Exposure <- glmmTMB(Richness ~ Exposure + (1 | Beach),
                family = "poisson",
                data = RIKZ, REML = F)
mem1_ranef <- glmmTMB(Richness ~ 1 + (1 | Beach),
                family = "poisson",
                data = RIKZ, REML = F)
glm0 <- glmmTMB(Richness ~ 1,
                family = "poisson",
                data = RIKZ, REML = F)
# confronto tramite AIC:
AIC(mem1, mem1_additivo, mem1_NAP, mem1_Exposure, mem1_ranef, glm0)



mem1additivoREML <- glmmTMB(Richness ~ NAP+Exposure + (1 | Beach),
                family = "poisson",
                data = RIKZ, REML = T)
summary(mem1additivoREML)



emmeans(mem1additivoREML, 
			specs = ~ NAP + Exposure,
			at = list(NAP = 0),
			type = "response"
			)


## ----warning = F, message = F--------------------------------------------------------
predicted.richness <- ggemmeans(mem1additivoREML, terms = c("NAP", "Exposure"))


## ----mem4fixef, echo=T, message=F, warning = F, results=F, tidy=F, fig.align = "center", fig.width = 6, fig.height = 6, fig.cap = "Predizioni del modello $mem1additivoREML$."----
plot(predicted.richness, # predizioni
 show_data=T,
 alpha=0.2, # trasparenze
 colors=c("#E7B800", "#00AFBB", "#FC4E07")) +
# fronzoli
labs(x = "Altezza (NAP) [m]",
       y = "Ricchezza di specie",
       title = "") +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 14))


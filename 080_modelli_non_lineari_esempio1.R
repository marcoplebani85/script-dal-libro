## ----warning = F, message=F, tidy=F--------------------------------------------------
rm(list=ls()) # ripulisce la memoria di R
options(scipen=9999) # evita che R esprima numeri come potenze di 10
# carichiamo il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)

# altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(readxl)  # per caricare dati da file .xlsx
p_load(nlstools) # grafici diagnostici per nls()
p_load(nls.multstart) # extra options per nls()

# definiamo la directory di lavoro. In R:
setwd("~/Desktop/Corso_R")
# In RSTUDIO: menu -> Session -> Set working directory

# Carichiamo i dati in R:
df <- as.data.frame(
  readxl::read_excel("my_data/non_linear_datasets.xlsx", sheet=3)
  )



## ----nonlin1, tidy=F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Relazione tra la biomassa e l'altezza degli alberi."----
par(bty="L", family="Times")
plot(biomass ~ tree.height, data = df, 
     xlab="Altezza [m]", ylab="Biomassa [Kg]")


## ------------------------------------------------------------------------------------
lm0 <- lm(biomass ~ 1, data=df)
lm1 <- lm(biomass ~ tree.height, data=df)
lm.poly <- lm(biomass ~ poly(tree.height, degree=2, raw=T), data=df)


## ------------------------------------------------------------------------------------
exponential <- as.formula(biomass ~ 0 + b*exp(tree.height*c))


## ----tidy=F--------------------------------------------------------------------------
nlm1 <-nls(exponential, # nome del modello
    data=df,
    # Aiutiamo l'algoritmo a trovare le stime giuste 
    # dandogli dei punti di partenza:
    start = list(b=2, c = 1)
    )


## ----nonlin1diagnnonlin, message=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $nlm1$."----
plot(nlsResiduals(nlm1), which = 0) 
# "which = 0" mostra i quattro grafici nella stessa figura


## ------------------------------------------------------------------------------------
AIC(lm.poly, nlm1)


## ------------------------------------------------------------------------------------
# sequenza di tree.height da fornire a predict():
predictions <- data.frame(tree.height=seq(0,16, by=0.01))
# predizioni del modello esponenziale:
predictions$nlm1 <- predict(nlm1, newdata = predictions)
# predizioni del modello polinomiale:
predictions$lm.poly <- predict(lm.poly, newdata = predictions)


## ----nonlin1predict, message=F, warning = F, results=F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Dati e predizioni dei modelli esponenziale e polinomiale."----
par(bty="L", family="Times")
plot(biomass ~ tree.height,
     data = df,
     xlim=c(0,16),
     cex=2.5, cex.axis=1.2, cex.lab =1.2,
     pch=16, col=grey(level=0.4, alpha=0.3),
     xlab="Altezza [m]",
     ylab="Biomassa [Kg]",
     las=1 # cambia l'orientamento dei valori degli assi
)

# predizioni del modello esponenziale:
points(x=predictions$tree.height,
       y=predictions$nlm1,
       type="l", lwd=2, col="black")

# predizioni del modello polinomiale:
points(x=predictions$tree.height,
       y=predictions$lm.poly,
       type="l", lwd=3, col="red", lty="dotted")

# legenda:
legend("topleft", # posizione
       bty="n", # no contorno
       cex=1.2,
       lwd=2,
       title="", # titolo (vuoto)
       legend=c("modello esponenziale", "modello polinomiale"), # legenda
       lty=c("solid", "dotted"), # tipo di linea in legenda
       col=c("black", "red") # colore linea in legenda
       )


## ----nonlin1diagnlm0, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm0$."----
par(mfrow=c(2,2)) ; plot(lm0)


## ----nonlin1diagnlm1, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm1$."----
par(mfrow=c(2,2)) ; plot(lm1)


## ----nonlin1diagnlmpoly, message=F, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $lm.poly$."----
par(mfrow=c(2,2)) ; plot(lm.poly)


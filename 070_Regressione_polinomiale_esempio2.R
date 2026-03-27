## ----warning = F, message=F, results=F-----------------------------------------------
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
  readxl::read_excel("my_data/non_linear_datasets.xlsx", sheet=2))
str(df)


## ----warning = F, message=F, results=F-----------------------------------------------
names(df) <- c("predittore", "risposta")


## ----polyplotb1, warning=F, fig.align="center", tidy=F, fig.width=5, fig.height=5, fig.cap="Andamento della variabile di risposta rispetto al predittore."----
par(bty="L", family="Times")
plot(risposta ~ predittore, data=df,
     xlim=c(0,20), ylim=c(5,20))


## ------------------------------------------------------------------------------------
lm0 <- lm(risposta ~ 1, data=df)
lm1 <- lm(risposta ~ predittore, data=df)
lm.poly <- lm(risposta ~ poly(predittore, degree=11, raw=T), data=df)


## ------------------------------------------------------------------------------------
dummy <- data.frame(predittore=seq(-5, 25, by=0.1))


## ----message=F, results=F, warning=F-------------------------------------------------
#predizioni per lm0:
predict.lm0 <- predict(lm0, # modello per cui generare predizioni
      # valori di "predittore" per cui stimare "risposta":
      newdata=dummy, 
      se.fit=T # per ottenere sia le stime sia i loro errori
      )
#predizioni per lm1:
predict.lm1 <- predict(lm1, newdata=dummy, type="response", se.fit=T)
#predizioni per lm.poly:
predict.poly <- predict(lm.poly, newdata=dummy, type="response", se.fit=T)


## ----polyplotb2, message=F, results=F, warning=F, tidy=F, fig.align="center", fig.width=6.5, fig.height=4, fig.cap="Andamento della variabile di risposta rispetto al predittore.  Le linee rappresentano le predizioni dei modelli $lm0$ (linea punteggiata), $lm1$ (linea tratteggiata), ed $lm.poly$ (linea continua rossa). il grafico (b) si concentra sull'intervallo della variabile di risposta compreso tra -5 e 25."----
par(mfrow=c(1,2), bty="L", family="Times")
plot(risposta ~ predittore, data=df, pch=16,
     xlim=c(0,20), ylim=c(-20,350))
# nonostante il nome, points() può anche produrre delle curve tramite l'argomento type="l":
points(x=dummy$predittore, 
       y=predict.lm0$fit, 
       type="l", lty="dotted")
points(x=dummy$predittore, 
       y=predict.lm1$fit, 
       type="l", lty="dashed")
points(x=dummy$predittore, 
       y=predict.poly$fit, 
       type="l", lty="solid", col="red", lwd=1.5)

mtext(expression(paste("(", bold(a),")")), side = 3, line = 0, adj = 0)

# ingrandiamo l'intervallo di y tra -5 e 25:
plot(risposta ~ predittore, data=df, pch=16,
     xlim=c(0,20), ylim=c(-5,25))
points(x=dummy$predittore, y=predict.lm0$fit, type="l", lty="dotted")
points(x=dummy$predittore, y=predict.lm1$fit, type="l", lty="dashed")
points(x=dummy$predittore, y=predict.poly$fit, type="l", lty="solid", col="red", lwd=1.5)

mtext(expression(paste("(", bold(b),")")), 
      side = 3, line = 0, adj = 0)


## ------------------------------------------------------------------------------------
anova(lm1, lm0)


## ------------------------------------------------------------------------------------
anova(lm.poly, lm1)


## ------------------------------------------------------------------------------------
AIC(lm0, lm1, lm.poly)




## ----warning = F, message=F, tidy=F--------------------------------------------------
rm(list=ls()) # ripulisce la memoria di R
options(scipen=9999) # evita di esprimere numeri come potenze di 10

# carica il pacchetto "pacman", che contiene la funzione p_load():
if (!require(pacman)) install.packages('pacman'); library(pacman)

# altri pacchetti:
p_load(emmeans)
p_load(ggplot2)
p_load(readxl)  # per caricare dati da file .xlsx
p_load(nlstools) # grafici diagnostici nls()
p_load(nls.multstart) # extra options per nls()

# definiamo la directory di lavoro. In R:
setwd("~/Desktop/Corso_R")
# In RSTUDIO: menu -> Session -> Set working directory

# Carichiamo i dati in R:
df <- as.data.frame(
  readxl::read_excel("my_data/non_linear_datasets.xlsx", 
                     sheet=4)
  )
str(df)


## ----nonlin2, fig.align="center", fig.width=6.5, fig.height=3, fig.cap="Andamento del livello di attività rispetto all'ora del giorno."----
gg1 <- ggplot(df, aes(x=time.hr, y=activity.index)) + # info base
  geom_point(pch=21, fill="grey", alpha=0.6, color="black", size=4) +
  theme_classic() +
  theme(text = element_text(family="Times", size=13)) +
  ylab("Livello di attività (%)") +
  xlab("Ora del giorno")
# visualizziamo il grafico appena creato:
gg1


## ----warning=F, tidy=F---------------------------------------------------------------
nlm0 <-nls(activity.index ~ aa + bb*cos(cc * time.hr),
		data=df
		)


## ----nonlin2nlm0diagn, warning=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $nlm0$."----
plot(nlsResiduals(nlm0), which = 0) # nlsResiduals() è dal pacchetto nlstools


## ------------------------------------------------------------------------------------
# serie di valori poer cui ottenere predizioni:
predictions <- data.frame(time.hr=seq(0,100))
# predizioni:
predictions$pred.nlm0 <- predict(nlm0, newdata=predictions)


## ----nonlin2nlm0pred, message=F, warning = F, tidy=F, results=F, fig.align="center", fig.width=6.5, fig.height=3, fig.cap="Andamento del livello di attività rispetto all’ora del giorno, e predizioni di $nlm0$."----
gg1 + geom_line(data=predictions, 
        aes(x=time.hr, y=pred.nlm0), 
        lwd=1 # spessore della linea
        )


## ------------------------------------------------------------------------------------
nlm1 <-nls(activity.index ~ aa + bb * cos(cc * time.hr),
           # punti di partenza per la ricerca dei parametri:
           start = list(aa = 49, bb = 41, cc = 1),
           data=df
           )


## ------------------------------------------------------------------------------------
coef(nlm1)
coef(nlm0)


## ----tidy=F--------------------------------------------------------------------------
nlm2 <- nls.multstart::nls_multstart(activity.index ~ aa + bb * cos(cc * time.hr),
     start_lower = c(aa = 45, bb = 35, cc = 0),
     start_upper = c(aa = 55, bb = 45, cc = 1),
     algorithm = "plinear",
     iter = 500,
     data=df
     )


## ----nonlin2nlm2diagn, echo=F, fig.align="center", fig.width=5, fig.height=6, fig.cap="Grafici diagnostici per $nlm2$."----
plot(nlsResiduals(nlm2), which=0)


## ----nonlin2nlm2pred, fig.align="center", fig.width=6.5, fig.height=3, fig.cap="Andamento del livello di attività rispetto all’ora del giorno, e predizioni di $nlm2$."----
# predizioni:
predictions$pred.nlm2 <- predict(nlm2, newdata=predictions)
# visualizzazione:
gg1 + geom_line(data=predictions, aes(x=time.hr, y=pred.nlm2), lwd=1)


## ------------------------------------------------------------------------------------
coef(nlm2)


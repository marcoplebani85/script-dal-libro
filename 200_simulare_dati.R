rm(list=ls()) # ripulisce la memoria di R
rnorm(n=10, mean=0, sd=1)

# simulazione di osservazioni da due distribuzioni normali:

dd <- data.frame(body.mass = c(rnorm(n=50, mean=100, sd=1),
                               rnorm(n=50, mean=110, sd=1)),
                 Species = as.factor(rep(c("A", "B"), each=50))
)

# Simulazione di dati correlati linearmente:

x1 <- c(10:20)
reps <- paste("rep", 1:5)

df <- expand.grid(predittore = x1,
                  repliche = reps)

df$risposta_media <- 10 + 3*df$predittore

df$risposta <- df$risposta_media + rnorm(n=length(df[,1]), mean=0, sd=3)

x1 <- c(1,10,20)
reps <- paste("rep", 1:3)
x2 <- c("A", "B")
df <- expand.grid(predittore1 = x1,
                  predittore2 = x2,
                  repliche = reps)

# Simulazione di due osservazioni che variano in dipendenza di un predittore lineare e di uno categorico: 

df$intercetta <- ifelse(df$predittore2=="A", 25, 30)
df$pendenza <- ifelse(df$predittore2=="A", 3, 0)

df$risposta_media <- df$intercetta + df$pendenza*df$predittore1

df$risposta <- df$risposta_media + rnorm(n=length(df[,1]), mean=0, sd=3)

x1 <- c(10:20)
reps <- paste("rep", 1:5)
df <- expand.grid(predittore = x1,
                  repliche = reps)

df$risposta_media <- exp(1 + 0.2*df$predittore)

df$risposta <- rpois(length(df[,1]), lambda = df$risposta_media)

# Simulazione di dati che variano in base ad un predittore misto ed uno random:

fertilizzanti <- LETTERS[1:2]
# Immaginiamo che lo studio sia stato replicato in vari paesi (random effect):
paesi <- c("Canada", "Costa Rica", "Egitto", "Grecia", "Italia", "Mongolia", "Portogallo")
df <- expand.grid(Fertilizzante = fertilizzanti, 
                  Paese = paesi, 
                  rep=1:5)
df$unique_ID <- paste(df$Fertilizzante, df$Paese, df$rep, sep="_")
df <- df[with(df, order(Fertilizzante, Paese)), ]

df$fixef <- ifelse(df$Fertilizzante == "A", 50, 80)

# creiamo un data.frame contenente i random effects:
re <- data.frame(Paese = unique(df$Paese))
re$ranef <- rnorm(n=length(re$Paese), mean = 0, sd = 3.5)
# "fondiamo" re e df:
dd <- merge(df, re)
# calcoliamo l'effetto medio di fixed + random effect:
dd$raccolto_medio = dd$fixef + dd$ranef

dd$raccolto <- dd$raccolto_medio + rnorm(n=length(dd[,1]), mean = 0, sd = 2.5)
# arrotondiamo al secondo decimale:
dd$raccolto <- round(dd$raccolto, 2)

# Simulazione di dati che variano in base ad un predittore misto ed uno random (2):

Localita <- LETTERS[1:8]
rep <- paste("rep", 1:10, sep="_")
Profondita <- 1:15
df <- expand.grid(Localita = Localita, Profondita = Profondita, rep = rep)
# diamo un nome univoco a ciascun campione:
df$unique_ID <- paste("loc", df$Localita, "depth", df$Profondita, df$rep, sep="_")

re <- data.frame(Localita = unique(df$Localita))
re$ri <- rnorm(n=length(re$Localita), mean = 100, sd = 20)
re$rs <- rnorm(n=length(re$Localita), mean = 3, sd = 1)
dd <- merge(df, re)
# calcoliamo l'effetto medio di fixed + random effect:
dd$lunghezza_media <- dd$ri + dd$rs*dd$Profondita

dd$lunghezza_alga <- dd$lunghezza_media + rnorm(n=length(dd[,1]), mean = 0, sd = 5)
# arrotondiamo al secondo decimale:
dd$lunghezza_alga <- round(dd$lunghezza_alga, 2)
# mettiamo le righe in ordine alfanumerico:
dd <- with(dd, dd[order(Localita, Profondita, rep), ])


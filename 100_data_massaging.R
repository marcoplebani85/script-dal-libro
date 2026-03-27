
rm(list=ls()) # ripulisce la memoria di R
dd <- data.frame(Lettera = c("B","B","A","A","C","C"),
                 x1 = runif(n=6, min=0, max=1), # valori casuali tra 0 e 1
                 x2 = runif(n=6, min=0.2, max=1.5)
                 )
dd



dd.ordered <- with(dd, dd[order(Lettera), ])
dd.ordered



dd$x1



dd[c(1,2,4),]



dd[,c(1,3)]



dd[c(1,2,4), c(1,3)]



dd[c(1,2,4), c("Lettera", "x1")]



dd1 <- subset(dd,
              subset=Lettera %in% c("A", "B"),
              select=-c(x1)
              )
dd1



subset(dd,
       subset=Lettera %in% c("A", "B") & x2 < 1,
       select=-c(x1)
       )



ee <- data.frame(Lettera = c("D","D"),
                 x1 = runif(n=2, min=0, max=1), # valori casuali tra 0 e 1
                 x2 = runif(n=2, min=0.5, max=2)
                 )
ee



dd2 <- rbind(dd, ee)
dd2



x3 <- runif(n=2, min=0, max=1)
x3



ee2 <- cbind(ee, x3)
ee2



gg <- data.frame(Lettera = c("A", "A", "B","B"),
                 Repliche = c("rep1", "rep2", "rep1","rep2"),
                 x1 = runif(n=4, min=0, max=1) # valori casuali tra 0 e 1
                 )
gg
ff <- data.frame(Lettera = c("B","B", "A", "A"),
                 Repliche = c("rep2", "rep1", "rep2","rep1"),
                 x2 = runif(n=4, min=0, max=1) # valori casuali tra 0 e 1
                 )
ff



mm <- merge(gg, ff, by=c("Lettera", "Repliche"), all=T)
mm



vv <- data.frame(Lettera = c("A","B","C","D"),
                 Tipo = c("vocale","consonante","consonante","consonante")
                 )
vv



hh <- merge(ff, vv, by=c("Lettera"), all=T)
hh



library(pacman)
p_load(tidyr)
# cambiamo il formato di gg da lungo a largo:
gg_large <- pivot_wider(data = gg, 
                        id_cols = c("Lettera"), 
                        names_from = "Repliche", 
                        values_from = "x1")
gg_large



# cambiamo il formato di gg da lungo a largo:
gg_long <- pivot_longer(data = gg_large, 
                        cols = c("rep1", "rep2"), 
                        names_to = "Repliche", 
                        values_to = "valore")
gg_long


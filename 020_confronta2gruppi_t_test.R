

rm(list=ls())
gruppo1 <- c(10.06, 9.47, 9.25, 9.72, 10.16, 9.91, 9.47, 10.57, 8.87, 9.62, 8.97, 8.92, 10.94, 10.14, 9.29, 9.00, 9.60, 9.71, 8.88, 9.26)

gruppo2 <- c(19.42, 19.29, 21.71, 20.26, 18.42, 20.61, 18.86, 20.91, 20.22, 20.44, 20.23, 20.00, 21.71, 20.70, 20.63, 19.64, 21.84, 21.25, 19.63, 19.56)



## ----chap02hist01, warning = F, tidy=F, fig.align="center", fig.width=6.5, fig.height=4, fig.cap="Distribuzioni di frequenza delle masse corporee osservate in $Popolazione 1$ e $Popolazione 2$ sulla base dei campioni raccolti."----
# definiamo una finestra grafica con una riga e due colonne:
par(mfrow=c(1,2))
hist(gruppo1)
hist(gruppo2)


## ----chap02hist02, warning = F, fig.align="center", fig.width=6.5, fig.height=4, echo=F, fig.cap="Distribuzioni di frequenza delle masse corporee osservate in $Popolazione 1$ (in grigio chiaro) e $Popolazione 2$ (in grigio scuro) rappresentate sullo stesso sistema di assi cartesiani."----
par(mfrow=c(1,1))
hist(gruppo1, col="grey90", 
	xlim=c(5, 25), # limiti dell'asse x
	xlab="massa corporea [Kg]", # etichetta dell'asse x
	ylab="Numero di individui", # etichetta dell'asse y
	main="Popolazione 1 vs. Popolazione 2" # titolo del grafico
	)

hist(gruppo2, add=TRUE, col="grey60")
# add=TRUE permette di aggiungere l'istogramma al sistema di assi esistente


## ------------------------------------------------------------------------------------
t.test(gruppo1, gruppo2) # test t di Welch


## ------------------------------------------------------------------------------------
t.test(gruppo1, gruppo2, var.equal=T) # test t di Student


## ------------------------------------------------------------------------------------
t.test(gruppo1)


## ------------------------------------------------------------------------------------
t.test(gruppo1, mu=9.5)


## ----echo=F--------------------------------------------------------------------------
pvalttest <- round(t.test(gruppo1, mu=9.5)$p.value, 3)


## ------------------------------------------------------------------------------------
prima <- gruppo1
dopo <- gruppo2


## ------------------------------------------------------------------------------------
t.test(x = prima, y = dopo, paired=TRUE)


## ----chap02paireddata, warning = F, fig.align="center", fig.width=6.5, fig.height=4, echo=F, fig.cap="Due possibili risposte della massa corporea di un gruppo di individui ad un trattamento sperimentale. Le linee uniscono la massa corporea di ciascun individuo all'inizio e alla fine del trattamento sperimentale. La massa corporea individuale media all'inizio e alla fine del trattamento sperimentale è uguale nei due casi, ma nel caso (a) tutti gli individui rispondono similmente al trattamento sperimentale, mentre nel caso (b) le risposte variano ampiamente."----
set.seed(1)
# simulate data
x1 <- round(rnorm(n=30, mean=15, sd=2), 2)
x2 <- round(rnorm(n=30, mean=16, sd=2), 2)

# arrange the data in a dataset
dd <- data.frame(ID=rep(paste("ID", seq(1,30, by=1), sep="_"),2),
 						response=c(x1,x2),
 						group=c(rep("A", 30), rep("B", 30))
 						)
# 
# 
dd$time=c(rep(0,30), rep(1, 30)) # measurements taken 15 days apart
# dd$jittime <- jitter(dd$time,.1) # just to prettify the graph
dd$jittime <- dd$time
par(mfrow=c(1,2), cex.lab=1.1)


casoA_prima <- sort(x1, decreasing = T)
casoA_dopo <- sort(x2, decreasing = T)
dd$response2 <- c(sort(x1, decreasing = T), sort(x2, decreasing = T))
plot(response2 ~ jittime, data=dd, pch=NA, xaxt = "n",
 xlab="Trattamento sperimentale", ylab="Massa corporea [Kg]", main=""
 )
arrows(dd$jittime[1:30], dd$response2[1:30], dd$jittime[31:60], dd$response2[31:60], length = 0, col= adjustcolor("red", alpha.f=.5))
points(response2 ~ jittime, data=dd)
axis(1, at = c(0, 1), labels=c("inizio", "fine"))
mtext(side=3, line=1.1, cex=1.2, adj=0, text=expression(paste("(", bold(a), ")")))

casoB_prima <- sort(x1, decreasing = F)
casoB_dopo <- sort(x2, decreasing = T)

dd$response3 <- c(sort(x1, decreasing = F), sort(x2, decreasing = T))
plot(response3 ~ jittime, data=dd, pch=NA, xaxt = "n",
 xlab="Trattamento sperimentale", ylab="Massa corporea [Kg]", main=""
 )
arrows(dd$jittime[1:30], dd$response3[1:30], dd$jittime[31:60], dd$response3[31:60], length = 0, col= adjustcolor("red", alpha.f=.5))
points(response3 ~ jittime, data=dd)
axis(1, at = c(0, 1), labels=c("inizio", "fine"))
mtext(side=3, line=1.1, cex=1.2, adj=0, text=expression(paste("(", bold(b), ")")))


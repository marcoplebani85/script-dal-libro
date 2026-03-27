## ----message=F, warning = F, results=F-----------------------------------------------
rm(list = ls()) # ripulisce la memoria di R

# carica i pacchetti:
if (!require(pacman)) install.packages('pacman'); library(pacman)


## ----include=F, warning = F, message=F, results=F------------------------------------
if (!require(formatR)) install.packages('formatR'); library(formatR)
if (!require(knitr)) install.packages('knitr'); library(knitr)

knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80),tidy=TRUE, dpi=200)

options(width = 80)


## ------------------------------------------------------------------------------------
p_load(ggplot2)


## ----warning = F---------------------------------------------------------------------
setwd("~/Desktop/Corso_R")
df_regr <- read.csv("my_data/regression_data.csv")


## ----message=F, results=F------------------------------------------------------------
pp1 <- ggplot(data=df_regr, aes(x = depth, y = alga.length)) 


## ----ggplot00, message=F, results=F, echo=F, warning = F, fig.align="center", fig.width=4, fig.height=4, fig.cap="L'oggetto $pp1$ è il livello base su cui andremo a rappresentare i nostri dati."----
pp1


## ----ggplot01, message=F, results=F, warning = F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Variabilità della lunghezza algale a diverse profondità; la linea blu è una smoothing spline il cui intervallo di confidenza è rappresentato dall'area grigia."----
pp1 + 
	geom_point() + # rappresenta i dati con uno scatterplot
	geom_smooth() # aggiunge una smoothing line


## ----ggplot02, message=F, results=F, warning = F-------------------------------------
gg <- pp1 + 
  geom_point() + # crea uno scatterplot
  geom_smooth(method = "lm", color="red") + # aggiunge retta di regressione in rosso
  theme_bw() # crea uno sfondo bianco anzichè grigio


## ----ggplot03, message=F, results=F, echo=F, warning = F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Variabilità della lunghezza algale a diverse profondità. La linea rossa è la retta di regressione associata ai dati, con l'intervallo di confidenza rappresentato in grigio."----
gg


## ----ggplot04, warning = F, message=F, results=F, fig.align="center", fig.width=4, fig.height=4, fig.cap="Variabilità della lunghezza algale a diverse profondità. La linea rossa è la retta di regressione associata ai dati, con l'intervallo di confidenza rappresentato in grigio."----
gg + 
  labs(title = " ", x = "Profondità [m s.l.m.]", y = "Lunghezza algale [cm]") + 
  # titolo (lasciato in bianco), etichette degli assi
  theme(plot.title = element_text(size = 20, hjust = 0.5), # dimensioni titolo
      axis.text.x = element_text(size = 15), # dimensioni valori asse x
      axis.text.y = element_text(size = 15), # dimensioni valori asse y
      axis.title.x = element_text(size = 15), # dimensioni etichetta dell'asse x
      axis.title.y = element_text(size = 15) # dimensioni etichetta dell'asse y
      ) + 
  geom_smooth(method = "lm", color="red") + # aggiunge retta di regressione
  theme_classic() # tema "classico" (simile a theme_bw ma senza reticolo)


## ----eval=F, warning=F---------------------------------------------------------------
# df_6sp <- read.delim("my_data/compare_6_species.txt")


## ----message=F, results=F, echo=F, warning=F-----------------------------------------
setwd("~/Desktop/Corso_R")
df_6sp <- read.delim("my_data/compare_6_species.txt")


## ----message=F-----------------------------------------------------------------------
g0 <- ggplot(df_6sp, aes(y = body.mass, x = Species))


## ----ggplot05, message=F, warning = F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Grafico scatola-e-baffi per le masse corporee osservate nelle specie A-F."----
g1 <- g0 + 
          geom_boxplot() + # grafico scatola-e-baffi
          labs(x="Specie", y = "Massa corporea [kg]") + # etichette degli assi
          theme_classic()


## ----ggplot06, message=F, results=F, warning = F, fig.align="center", fig.width=5, fig.height=5, fig.cap="Distribuzioni di frequenza delle masse corporee osservate per le specie A-F."----
g2 <- g0 + 
            geom_violin(aes(fill=Species)) + # per creare un violin plot
            theme_classic() + 
            # personalizza le etichette degli assi:
            labs(x="Specie", y = "Massa corporea [kg]") + 
            theme(legend.position = "none") # rimuove legenda


## ----ggplot7, message=F, results=F, warning = F, fig.align="center", fig.width=5, fig.height=4, fig.cap="Istogramma delle masse corporee osservate per le specie A-F."----
g3 <- ggplot(df_6sp, aes(x = body.mass, fill = Species)) + # informazioni base
  geom_histogram(alpha = 0.4, position = "identity", bins=55, color="white") +
  # personalizza le etichette degli assi:
  labs(x = "Massa corporea [kg]", y = "Numero di individui") +
  # personalizza il titolo della legenda:
  labs(fill="Specie") + 
  theme_classic()


## ----message=F, warning=F------------------------------------------------------------
p_load(ggpubr) # per organizzare grafici prodotti da ggplot


## ----ggplot08, message=F, results=F, warning = F, fig.align="center", fig.width=6.5, fig.height=8, fig.cap="Variabilità delle masse corporee osservate per le specie A-F rappresentate in forma di grafico scatola-e-baffi (a), violin plot (b), e distribuzioni di frequenza (c)."----
ggarrange(g1, g2, g3, # grafici da rappresentare insieme
        labels = c("(a)", "(b)", "(c)"), # numerazione dei grafici
        ncol = 1, nrow = 3 # disposizione: un rigo, tre colonne
        )


## ----ggplot09, message=F, results=F, warning = F, fig.align="center", fig.width=6.5, fig.height=8, fig.cap="Variabilità delle masse corporee osservate per le specie A-F rappresentate in forma di distribuzioni di frequenza (a), grafico scatola-e-baffi (b), e violin plot (c)."----
ggarrange(g3, # prima riga.
        # seconda riga:
        ggarrange(g1, g2, ncol = 2, labels = c("(b)", "(c)")), 
        nrow = 2, 
        labels = "(a)" # etichetta di g3
        )


## ----message=F, warning=F------------------------------------------------------------
p_load(ggridges)


## ----ggplot11, message=F, results=F, warning = F, fig.align="center", fig.width=6.5, fig.height=7, fig.cap="Distribuzioni di frequenza delle masse corporee osservate per le specie A-F."----
ggplot(df_6sp, aes(x = body.mass, fill = Species, y = Species)) + # informazioni base
        ggridges::geom_density_ridges(alpha = 0.4, scale = 10) +
        # personalizza le etichette degli assi:
        labs(x = "Massa corporea [kg]",
            y = "Frequenza") +
        # personalizza il titolo della legenda:
        labs(fill="Specie") +
        theme_classic()


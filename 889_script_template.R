### TITOLO
# 
# # Autore
# 
# # Descrizione del contenuto dello script
# 
# # Script iniziato in data: gg/mm/aaaa
# 
# # Ultima modifica in data: gg/mm/aaaa
# 
# rm(list=ls()) # ripulisce la memoria di R
# 
# # WORK DIRECTORY
# 
# # per ottenere la work directory corrente: getwd()
# 
# # per cambiare work directory in R: setwd(). Nel mio caso:
# setwd("~/Desktop/Corso_R/my_data")
# 
# # la tilde ("~") in questo caso è una abbreviazione usata nei sistemi Linux
# 
# # in Windows la directory va riportata nella sua interezza, assicurandosi che sia scritta con "/" invece di "\".
# 
# # in RStudio possiamo semplicemente selezionare la work directory dal menu: clicca su "Session", poi su "Set working directory"
# 
# # PER AGGIORNARE I PACCHETTI:
# 
# options(timeout=600)
# # di default, timeout=60: ovvero, se R impiega più di 60 secondi a scaricare pacchetti da internet, interrompe il processo. Fissare timeout=600 è utile per connessioni lente.
# update.packages(ask = F, type = "binary")
# 
# # INSTALLARE PACCHETTI
# 
# # RStudio identifica automaticamente i pacchetti utilizzati in uno script e, se non sono installati, chiede direttamente all'utilizzatore se sono da installare. Se questo non dovesse succedere, i pacchetti possono essere installati dalla finestra "Packages".
# 
# # in R: Menu > Packages & data > package installer, clicca su "get list", cerca il pacchetto da installare, selezionalo, seleziona anche "install dependencies" e clicca su "Install selected".
# 
# # Oppure installiamo i pacchetti direttamente dal terminale con la funzione install.packages(). Per evitare di reinstallare i pacchetti ogni volta, io uso un "if statement":
# if (!require(pacman)) install.packages('pacman')
# # Questo significa: "se il pacchetto pacman non è installato, procedi con l'installazione. A questo punto il pacchetto in questione può essere caricato con:
# library(pacman)
# # Il pacchetto pacman contiene funzioni che facilitano l'installazione e il caricamento dei pacchetti dal terminale di R. In particolare, la funzione p_load() svolge il lavoro di library(), ma si premura di installare automaticamente i pacchetti se non sono ancora stati installati. La funzione p_load svolge il lavoro di install.packages() e library() in un colpo solo.
# 
# # PACCHETTI DI USO COMUNE:
# 
# p_load(openxlsx)
# # openxlsx serve a caricare dati da file .xlsx. Per formati .xls provate readxl, che però richiede di avere installato Java.
# p_load(doBy) # per calculare summary statistics
# # per calcolare le medie marginali e i parametri dei modelli:
# p_load(emmeans)
# p_load(ggplot2) # per produrre grafici
# 
# # PACCHETTI PER PRODURRE MODELLI NON-LINEARI:
# 
# p_load(nlstools) # grafici diagnostici per nls()
# p_load(nls.multstart) # extra optionals per nls()
# 
# # PACCHETTI PER PRODURRE MODELLI MISTI:
# 
# p_load(glmmTMB)
# # glmmTMB sostituisce e supera le funzioni lm() e glm() ed i pacchetti lme4 e lmerTest
# p_load(DHARMa) # grafici diagnostici per glmmTMB
# p_load(MuMIn) # stima pseudo-R^2 per modelli misti.
# 
# # PROTOCOLLO DI ANALISI DEI DATI
# 
# # 1. definizione di un interrogativo di ricerca chiaro e statisticamente verificabile
# 
# # 2. inserimento dei dati in R, loro formattazione e preparazione per le analisi
# 
# # 3. visualizzazione e interpretazione visiva dei dati
# 
# # 4. definizione di un modello sulla base dell'interrogativo di ricerca
# 
# # 5. controllo che le assunzioni del modello siano verificate. Se non lo sono, cambiamo modello!
# 
# # 6. verifica della validità del modello confrontandolo con modelli più semplici
# 
# # 7. estrazione dei parametri del modello e del potere esplicativo del modello, visualizzazione delle predizioni del modello
# 
# # comunicazione dei risultati.


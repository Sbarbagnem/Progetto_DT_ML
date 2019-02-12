##################################### INFO #####################################
#Progetto di ML
#Progetto: waiting  prediction system
#Team:
# - Samuele Ventura, 
# - Luca Virgilio, 794866

#ML- DecisionTree

# librerie necessarie
if (!require("ggplot2"))
  install.packages("ggplot2", dependencies = T)
if (!require("reshape2")) 
  install.packages("reshape2", dependencies = T)
if (!require("rattle")) 
  install.packages("rattle", dependencies = T)
if (!require("rpart.plot")) 
  install.packages("rpart.plot", dependencies = T)
if (!require("RColorBrewer")) 
  install.packages("RColorBrewer", dependencies = T)
if (!require("ROCR"))
  install.packages("ROCR", dependencies = T)
if (!require("caret"))
  install.packages("caret", dependencies = T)

library(ggplot2)
library(reshape2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)

setwd("~/Desktop/consegna/ProgettoML")

dataset = read.csv2(file = "./dataset/finale_int.csv", header = TRUE, sep = ",")

# png("dens_attesa.png")
# ggplot(dataset, aes(Attesa)) +
#       geom_density() 
# dev.off()
# 
# temp = sort(dataset$Attesa)
# perc = quantile(temp, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.97, 0.99))

#rimuovo gli outlier. Valori oltre il 95 percentile
dataset = dataset[dataset$Attesa<quantile(dataset$Attesa, 0.95),] # perdo 4394 righe

# png("dens_attesa_dopo.png")
# ggplot(dataset, aes(Attesa)) +
#   geom_density()
# dev.off()

########### poi da togliere##########
dataset = subset(dataset, select=-c(Efficienza_sportello,Efficienza_sportello_servizio))

dataset = subset(dataset, select=-c(Chiamato_alle_ore,Stato,Operatore_numero,Sportello,Ultima_operazione_alle,
                                    Durata,Indice,media3,mediana3,L2t,L3t))


set.seed(100)
trainIndex <- createDataPartition(dataset$Attesa, p = .7, 
                                  list = FALSE, 
                                  times = 1)

trainingset <- dataset[ trainIndex,]
test  <- dataset[-trainIndex,]

png("AttesaGiornoSettimana.png")
boxplot(trainingset$Attesa ~ trainingset$Giorno_settimana, col = "coral2", 
        xlab = "GIORNO SETTIMANA", ylab = "ATTESA", 
        main="ATTESA ~ GIORNO_SETTIMANA")
dev.off()

png("AttesaServizio.png")
boxplot(trainingset$Attesa ~ trainingset$Servizio, col = "coral2", 
        xlab = "SERVIZIO", ylab = "ATTESA", 
        main="ATTESA ~ SERVIZIO")
dev.off()

png("AttesaPresenteAlle.png")
boxplot(trainingset$Attesa ~ trainingset$Presente_alle_ore, col = "coral2", 
        xlab = "PRESENTE ALLE ORE", ylab = "ATTESA", 
        main="ATTESA ~ FASCIA ORARIA")
dev.off()

png("AttesaMeseVisita.png")
boxplot(trainingset$Attesa ~ trainingset$Mese_visita, col = "coral2", 
        xlab = "MESE VISITA", ylab = "ATTESA", 
        main="ATTESA ~ MESE VISITA")
dev.off()

png("AttesaPersoneSala.png")
boxplot(trainingset$Attesa ~ trainingset$Persone_in_sala, col = "coral2", 
        xlab = "PERSONE IN SALA", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ PERSONE IN SALA")
dev.off()

png("Attesamed5.png")
boxplot(trainingset$Attesa ~ trainingset$mediana5, col = "coral2", 
        xlab = "MEDIANA 5", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ MEDIANA 5")
dev.off()

png("Attesamean5.png")
boxplot(trainingset$Attesa ~ trainingset$media5, col = "coral2", 
        xlab = "MEDIA 5", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ MEDIA 5")
dev.off()

png("AttesaLt.png")
boxplot(trainingset$Attesa ~ trainingset$Lt, col = "coral2", 
        xlab = "Lt", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ LT")
dev.off()

png("AttesaEfficeSportelli.png")
boxplot(trainingset$Attesa ~ trainingset$Efficienza_sportelli, col = "coral2", 
        xlab = "EFFICIENZA SPORTELLI", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ EFFICIENZA SPORTELLI")
dev.off()

png("AttesaEfficeSportelliServizio.png")
boxplot(trainingset$Attesa ~ trainingset$Efficienza_sportelli_servizio, col = "coral2", 
        xlab = "EFFICIENZA SPORTELLI SERVIZIO", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ EFFICIENZA SPORTELLI SERVIZIO")
dev.off()

png("AttesaOrdGiornServ.png")
boxplot(trainingset$Attesa ~ trainingset$Ordine_giorno_servizio, col = "coral2", 
        xlab = "ORDINE GIORNO SERVIZIO", ylab = "ATTESA", outline=FALSE, range = 0.01,
        main="ATTESA ~ ORDINAMENTO GIORNO SERVIZIO")
dev.off()

png("AttesaSportelloAttivi.png")
boxplot(trainingset$Attesa ~ trainingset$Sportelli_attivi, col = "coral2", 
        xlab = "SPORTELLI ATTIVI", ylab = "ATTESA", 
        main="ATTESA ~ SPORTELLI ATTIVI")
dev.off()

png("AttesaMediaP.png")
boxplot(dataset$Attesa ~ dataset$MediaP, col = "coral2", 
        xlab = "MEDIA PRECIPITAZIONI", ylab = "ATTESA", 
        main="ATTESA ~ MEDIA PRECIPITAZIONI")
dev.off()

##################################### INFO #####################################
#Progetto di ML
#Progetto: waiting  prediction system
#Team:
# - Samuele Ventura,
# - Luca Virgilio, 794866

# librerie necessarie
if (!require("graph"))
  install.packages("graph", dependencies = T)
library("graph")
#graph plotting package
if (!require("Rgraphviz"))
  install.packages("Rgraphviz", dependencies = T)
library("Rgraphviz")
if (!require("readxl"))
  install.packages("readxl", dependencies = T)
library(readxl)
if (!require("chron"))
  install.packages("chron", dependencies = T)
library(chron)
if (!require("ggplot2"))
  install.packages("ggplot2", dependencies = T)
library(ggplot2)
if (!require("plyr"))
  install.packages("plyr", dependencies = T)
library(plyr)
if (!require("ggplot2"))
  install.packages("ggplot2", dependencies = T)
library("ggplot2")
#correlation packages
if (!require("corrplot"))
  install.packages("corrplot", dependencies = T)
library("corrplot")
if (!require("ggcorrplot"))
  install.packages("ggcorrplot", dependencies = T)
library("ggcorrplot")
if (!require("dplyr"))
  install.packages("dplyr", dependencies = T)
library("dplyr")
if (!require("stringr"))
  install.packages("stringr", dependencies = T)
library("stringr")

setwd(paste0("C:/Users/luca/Desktop/progettoML"))

data = read.csv2(file = "./Dataset/TuPassi.csv", header = TRUE, sep = ";")

# divido giorno in giorno e mese
data$Giorno =  as.Date(data$Giorno, format = "%d/%m/%Y")
data$Giorno_visita = format(data$Giorno, "%d")
data$Mese_visita = format(data$Giorno, "%m")
data$Mese_visita = as.integer(data$Mese_visita)
data$Giorno_visita = as.integer(data$Giorno_visita)

# creo colonna che indica il giorno della settimana [0-6] partendo da domenica
data$Giorno_settimana = as.POSIXlt(data$Giorno)$wday
data = data[,-1]
data$Servizio = as.integer(data$Servizio)

#definisco il formato degli orari
data$Presente_alle_ore = strptime(data$Presente_alle_ore, format = "%H:%M:%S")
data$Chiamato_alle_ore = strptime(data$Chiamato_alle_ore, format = "%H:%M:%S")
data$Ultima_operazione_alle = strptime(data$Ultima_operazione_alle, format = "%H:%M:%S")

# calcolo attesa effettiva
data$Attesa = as.numeric(difftime(data$Chiamato_alle_ore,data$Presente_alle_ore, units = "mins"))
data$Attesa = as.integer(data$Attesa)

# calcolo durata del Servizio allo sportello
data$Durata = as.numeric(difftime(data$Ultima_operazione_alle,data$Chiamato_alle_ore, units = "sec"))

# grafico della distribuzione dell'attesa per tagliare via dei valori
h<-hist(data$Attesa, col="red", xlab="Attesa",
        main="Istogramma attesa con curva")
xfit<-seq(min(data$Attesa),max(data$Attesa),length=40)
yfit<-dnorm(xfit,mean=mean(data$Attesa),sd=sd(data$Attesa))
yfit <- yfit*diff(h$mids[1:2])*length(data$Attesa)
lines(xfit, yfit, col="blue", lwd=2)

# tolgo ora di chi si presenta alle 6, prima che aprano sportelli (potremmo togliere anche le 6?)
data = data[data$Presente_alle_ore>=6,]

# elimino attributo prenotato
# elimino attesa_in_sec, perche' attesa rispetto totem
data = data[,c(-4,-5)]
################################### DISCRETIZZAZIONE #############################################
#verifico la stabilitï¿½ dei servizi:

helpService=array(numeric(), c(23,12))
for(i in 1:12){
  temp=data[data$Mese_visita==i,]
  for(j in 1:23){
    helpService[j,i]=sum(temp$Servizio==j)
  }
}
rm(temp)
#i servizi non sono stabili e cambiano nel tempo. in particolarmente rilevanti sono
#servizio accettazione privata (tempo minore) e prenotazione privata, quindi
#utilizzo solo i mesi in cui questo il servizio è stabilizzato: da ottobre
data=data[data$Mese_visita>9,]


# discretizzo stato
data$Stato = as.character(data$Stato)
data$Stato[data$Stato == "LAVORATO"] = "1"
data$Stato[data$Stato == "CHIAMATO"] = "2"
data$Stato[data$Stato == "ANTICIPATO"] = "3"
data$Stato = as.integer(data$Stato)
##################discretizzo i servizi

#definisco un data_v2 dove discretizzo i servizi
data_v2=data

# Servizio 1:accettazione ssn
# Servizio 2:accettazione privato
# Servizio 3:prioritï¿½
# Servizio 4:solventi
# Servizio 5:ritiro esami
# Servizio 6:prenotazione ssn
# Servizio 7: prenotazione privata
# Servizio 8: prelievi
# Servizio 9: altro

temp = data_v2$Servizio

temp[data_v2$Servizio %in% c(1,22,17)] = 1
temp[data_v2$Servizio==2] = 2
temp[data_v2$Servizio %in% c(11,12,13,14,15,16)] = 3
temp[data_v2$Servizio %in% c(19,20)] = 4
temp[data_v2$Servizio==18] = 5
temp[data_v2$Servizio==9] = 6
temp[data_v2$Servizio==8] = 7
temp[data_v2$Servizio==6] = 8
temp[data_v2$Servizio %in% c(3,4,5,7,10,18,21,23)] = 9

data_v2$Servizio=temp
rm(temp)

prova=data_v2[,c("Servizio","Attesa")]
temp=data[,c("Servizio","Attesa")]
#verifico la differenza di correlazione tra campo servizio discretizzato e non
res1=cor(temp)
res2=cor(prova)
#dimostra che funziona meglio senza discretizzare i valori del servizio

#################################### stima di altri parametri #######################################
# calcolo colonna indice e ordino
data$Indice = c(1:nrow(data))
data = data[with(data, order(data$Mese_visita, data$Giorno_visita, data$Presente_alle_ore)),]


#calcolo le persone presenti in sala all'arrivo del paziente e quelle presenti
# in diversi istanti temporali precedenti (t, 2t, 3t) idetificate da Lt,L2t,L3t
data$Persone_in_sala = rep(0, nrow(data))
data$Lt = rep(0, nrow(data))
data$L2t = rep(0, nrow(data))
data$L3t = rep(0, nrow(data))
#assumiamo come istanza temporale t=10 min
t=as.difftime(tim = 10, units = "mins")



data1 = data[,c("Mese_visita","Giorno_visita","Presente_alle_ore","Chiamato_alle_ore","Persone_in_sala","Indice", "Lt", "L2t", "L3t")]

# sistemo i formati dei dati temporali
data1$Presente_alle_ore = format(data1$Presente_alle_ore, format = "%H:%M:%S")
data1$Chiamato_alle_ore = format(data1$Chiamato_alle_ore, format = "%H:%M:%S")
data1$Presente_alle_ore =as.POSIXct(data1$Presente_alle_ore, format = "%H:%M:%S")
data1$Chiamato_alle_ore =as.POSIXct(data1$Chiamato_alle_ore, format = "%H:%M:%S")

# ciclo per ogni mese
for (q in c(unique(data1$Mese_visita))){

  dummy1 = data1[data1$Mese_visita==q,]

  # ciclo per ogni giorno
  for (j in c(unique(dummy1$Giorno_visita))) {

    dummy2 = dummy1[dummy1$Giorno_visita==j,]

    # ciclo per ogni riga
    for (i in c(2: nrow(dummy2))) {

      indice = dummy2$Indice[i]

      bol = (dummy2$Presente_alle_ore < dummy2$Presente_alle_ore[i]) &
        (dummy2$Chiamato_alle_ore > dummy2$Presente_alle_ore[i])
      bol1 = (dummy2$Presente_alle_ore < (dummy2$Presente_alle_ore[i]-t)) &
        (dummy2$Chiamato_alle_ore > (dummy2$Presente_alle_ore[i]-t))
      bol2 = (dummy2$Presente_alle_ore < (dummy2$Presente_alle_ore[i]-2*t)) &
        (dummy2$Chiamato_alle_ore > (dummy2$Presente_alle_ore[i]-2*t))
      bol3 = (dummy2$Presente_alle_ore < (dummy2$Presente_alle_ore[i]-3*t)) &
        (dummy2$Chiamato_alle_ore > (dummy2$Presente_alle_ore[i]-3*t))
      data$Persone_in_sala[data$Indice == indice] = sum(bol == TRUE)
      data$Lt[data$Indice == indice] = sum(bol1 == TRUE)
      data$L2t[data$Indice == indice] = sum(bol2 == TRUE)
      data$L3t[data$Indice == indice] = sum(bol3 == TRUE)

    }
  }
}

data$Persone_in_sala[data$Persone_in_sala == -1] = 0

rm(dummy1,dummy2,data1)

final=data

# calcolo altri parametri
# calcolo la media e la mediana degli ultimi 3 e 5 pazienti precedenti (pazienti vengono
#suddivisi per servizio)
data2=data[,c("Mese_visita","Giorno_visita","Presente_alle_ore","Chiamato_alle_ore","Servizio", "Attesa", "Indice")]

data$mediana3=rep(0,nrow(data))
data$mediana5=rep(0,nrow(data))
data$media3=rep(0,nrow(data))
data$media5=rep(0,nrow(data))

# ciclo per ogni mese
for (q in c(unique(data2$Mese_visita))){

  dummy1 = data2[data2$Mese_visita==q,]

  # ciclo per ogni giorno
  for (j in c(unique(dummy1$Giorno_visita))) {

    dummy2 = dummy1[dummy1$Giorno_visita==j,]

    # ciclo per ogni servizio
    for (i in c(unique(dummy2$Servizio))) {

      temp=dummy2[dummy2$Servizio==i,]

      for (k in (2:nrow(temp))){

        if(nrow(temp)>1){
          indice = temp$Indice[k]

          z = k - 1

          while(z>0 & temp$Chiamato_alle_ore[z] > temp$Presente_alle_ore[k]){
            z = z - 1
            if(z==0){
              break
            }
          }

          if (z==0){
          }else
            if (z==1 | z==2){
              data$mediana3[data$Indice==indice]=(temp$Attesa[z])
              data$mediana5[data$Indice==indice]=(temp$Attesa[z])
              data$media3[data$Indice==indice]=(temp$Attesa[z])
              data$media5[data$Indice==indice]=(temp$Attesa[z])
            }else
              if (z==3 | z==4){
                p=z - 2
                data$mediana3[data$Indice==indice]=as.integer(median(temp$Attesa[p:z]))
                data$mediana5[data$Indice==indice]=as.integer(median(temp$Attesa[p:z]))
                data$media3[data$Indice==indice]=as.integer(mean(temp$Attesa[p:z]))
                data$media5[data$Indice==indice]=as.integer(mean(temp$Attesa[p:z]))
              }else
                if(z>4){
                  p= z - 2
                  p2=z - 4
                  data$mediana3[data$Indice==indice]=as.integer(median(temp$Attesa[p:z]))
                  data$mediana5[data$Indice==indice]=as.integer(median(temp$Attesa[p2:z]))
                  data$media3[data$Indice==indice]=as.integer(mean(temp$Attesa[p:z]))
                  data$media5[data$Indice==indice]=as.integer(mean(temp$Attesa[p2:z]))
                }
        }
      }
    }
  }
}
##### PARAMETRO: NUMERO DI PERSONE SERVITE DALLO SPORTELLO NEI 10 MINUTI PRECEDENTI
data$Efficienza_sportello = rep(0, nrow(data)) # calcolo persone servite nei 10 minuti precedenti dallo stesso sportello
data$Efficienza_sportelli = rep(0, nrow(data)) # calcolo persone servite nei 10 minuti precedenti da tutti gli sportelli
data$Efficienza_sportello_servizio = rep(0, nrow(data)) # calcolo persone servite nei 10 minuti precedenti con stesso servizio
data$Efficienza_sportelli_servizio = rep(0, nrow(data)) # calcolo persone servite nei 10 minuti precedenti con stesso servizio e tutti sportelli

temp = data

temp = temp[with(temp, order(temp$Mese_visita, temp$Giorno_visita, temp$Presente_alle_ore)),]

temp = temp[,c("Mese_visita","Giorno_visita","Presente_alle_ore", "Chiamato_alle_ore","Sportello","Servizio", "Indice","Efficienza_sportello",
               "Efficienza_sportelli","Efficienza_sportello_servizio","Efficienza_sportelli_servizio")]

for (i in c(unique(temp$Mese_visita))) {

  dummy_mese = temp[temp$Mese_visita == i,]

  for (j in c(unique(dummy_mese$Giorno_visita))) {

    dummy_giorno = dummy_mese[dummy_mese$Giorno_visita == j,]

    for (k in (2:nrow(dummy_giorno))) {

      indice = dummy_giorno$Indice[k]

      bol = (dummy_giorno$Presente_alle_ore[k] - dummy_giorno$Chiamato_alle_ore) %in% c(0:600)

      s =  sum(bol == TRUE)

      temp$Efficienza_sportelli[temp$Indice == indice] = s

      bol = bol & (dummy_giorno$Servizio[k] == dummy_giorno$Servizio)

      s =  sum(bol == TRUE)

      temp$Efficienza_sportelli_servizio[temp$Indice == indice] = s
    }

    for (q  in c(unique(dummy_giorno$Sportello))) {

      dummy_sportello = dummy_giorno[dummy_giorno$Sportello==q,]

      for (k in (1:nrow(dummy_sportello))) {

        indice = dummy_sportello$Indice[k]

        bol = (dummy_sportello$Presente_alle_ore[k] - dummy_sportello$Chiamato_alle_ore) %in% c(0:600)

        s =  sum(bol == TRUE)

        temp$Efficienza_sportello[temp$Indice == indice] = s

        bol = bol & (dummy_sportello$Servizio[k] == dummy_sportello$Servizio)

        s =  sum(bol == TRUE)

        temp$Efficienza_sportello_servizio[temp$Indice == indice] = s
      }
    }
  }
}

data$Efficienza_sportello = temp$Efficienza_sportello
data$Efficienza_sportelli = temp$Efficienza_sportelli
data$Efficienza_sportelli_servizio = temp$Efficienza_sportelli_servizio
data$Efficienza_sportello_servizio = temp$Efficienza_sportello_servizio

rm(dummy_giorno, dummy_mese, dummy_sportello,temp)



##### PARAMETRO: AGGIUNTA INDICE ORDINE D'ARRIVO PER GIORNO E PER GIORNO&SERVIZIO
data$Ordine_giorno = rep(0,nrow(data))
data$Ordine_giorno_servizio = rep(0,nrow(data))

data = data[with(data, order(data$Mese_visita, data$Giorno_visita, data$Presente_alle_ore)),]
temp = data[,c("Mese_visita","Giorno_visita","Servizio", "Ordine_giorno", "Ordine_giorno_servizio")]

for (i in c(unique(temp$Mese_visita))) {

  dummy_mese = temp[temp$Mese_visita==i,]

  for (j in c(unique(dummy_mese$Giorno_visita))) {

    bool = temp$Giorno_visita==j & temp$Mese_visita==i

    n = sum(bool == TRUE)

    temp$Ordine_giorno[bool] = c(1:n)

    dummy_giorno = dummy_mese[dummy_mese$Giorno_visita==j,]

    for (q in c(unique(dummy_giorno$Servizio))) {

      bool1 = bool & temp$Servizio==q

      n = sum(bool1 == TRUE)

      temp$Ordine_giorno_servizio[bool1] = c(1:n)

    }
  }
}

data$Ordine_giorno = temp$Ordine_giorno
data$Ordine_giorno_servizio = temp$Ordine_giorno_servizio


rm(dummy1,dummy2,temp, data2)
# ora che non mi servono piï¿½, discretizzo gli attributi temporali alle sole ore
data$Presente_alle_ore=as.POSIXct(data$Presente_alle_ore)
data$Chiamato_alle_ore=as.POSIXct(data$Chiamato_alle_ore)
data$Ultima_operazione_alle=as.POSIXct(data$Ultima_operazione_alle)
data$Presente_alle_ore = as.numeric(format(data$Presente_alle_ore, format = "%H"))
data$Chiamato_alle_ore = as.numeric(format(data$Chiamato_alle_ore, format = "%H"))
data$Ultima_operazione_alle = as.numeric(format(data$Ultima_operazione_alle, format = "%H"))

# STIMA SPORTELLI ATTIVI
data = data[with(data, order(data$Mese_visita, data$Giorno_visita, data$Presente_alle_ore)),]
data$Sportelli_attivi = rep(0, nrow(data))

# ciclo sui mesi
for (q in c(unique(data$Mese_visita))){

  data_mese = data[data$Mese_visita==q,]

  # ciclo sui giorni
  for (j in c(unique(data_mese$Giorno_visita))) {

    data_giorno = data_mese[data_mese$Giorno_visita == j,]

    # ciclo sulle fascie orarie
    for (i in c(unique(data_giorno$Chiamato_alle_ore))) {

      indice = data_giorno$Indice[data_giorno$Chiamato_alle_ore==i] # indici elementi in fascia oraria i

      dummy = data_giorno[data_giorno$Chiamato_alle_ore == i,]

      data$Sportelli_attivi[data$Indice %in% indice] = length(table(dummy$Sportello))
    }
  }
}

rm(data_giorno,data_mese,dummy)
##################################################################################################

# grafico sull'attesa rispetto a sportelli attivi
boxplot(data$Attesa ~ data$Sportelli_attivi, col = "coral2",
        xlab = "SPORTELLI ATTIVI", ylab = "ATTESA",
        main="DISTRIBUZIONE ATTESA PER SPORTELLI ATTIVI")

# grafico su sportelli attivi rispetto alle persone in sala, si vede come salgono persone salgono sportelli attivi
temp = data
temp$Persone_in_sala[temp$Persone_in_sala %in% c(0:50)] = 1
temp$Persone_in_sala[temp$Persone_in_sala %in% c(51:100)] = 2
temp$Persone_in_sala[temp$Persone_in_sala %in% c(101:150)] = 3
temp$Persone_in_sala[temp$Persone_in_sala %in% c(151:200)] = 4
temp$Persone_in_sala[temp$Persone_in_sala > 200] = 5

# grafico che mostra frequenza rispetto a numero persone
hist(temp$Persone_in_sala, xlab = "PERSONE IN ATTESA", ylab = "FREQUENZA",
     main="FREQUENZA NUMERO PERSONE IN SALA")

boxplot(temp$Sportelli_attivi ~ temp$Persone_in_sala, col = "coral2",
        xlab = "PERSONE IN ATTESA", ylab = "SPORTELLI ATTIVI",
        main="DISTRIBUZIONE PERSONE IN SALA PER SPORTELLI ATTIVI")

rm(temp)
#escludo persone arrivate prima delle 6 del mattino
data = data[data$Presente_alle_ore>5,]

write.csv(data, file = "./Dataset/finale.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

data_p1=data[,c(1:15)]
data_p2=data[,c(11, 16:28)]
#### correlation
res = cor(data_p1)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/corr_expolation1.png", width = 1024)
corrplot(res, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
dev.off()

#### correlation
res2 = cor(data_p2)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/corr_exploration2.png", width = 1024)
corrplot(res2, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
dev.off()

##################################### INFO #####################################
#Progetto di ML
#Progetto: waiting  prediction system
#Team:
# - Samuele Ventura, 793060
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
  install.packages("stringr", dependencies=T)
library("stringr")



setwd(paste0("C:/Users/luca/Desktop/progettoML"))

# leggo dataset
data = read.csv2(file = "./Dataset/finale.csv", header = TRUE, sep = ",")
precipitazioni = read.csv2(file = "./Dataset/Precipitazioni.csv", header = TRUE, sep = ",")
festivi = read.csv2(file = "./Dataset/Feste.csv", header = TRUE, sep = ";")

############################ integrazione dataset festivi ###########################################################

data2 = data[,c("Mese_visita","Giorno_visita","Presente_alle_ore","Indice")]

# aggiungo dati per indicare vicinanza feste
names(festivi)[1] = "Giorno"

festivi$Giorno =  as.Date(festivi$Giorno, format = "%d/%m/%Y")
festivi$Mese_visita = as.integer(format(festivi$Giorno, "%m"))
festivi$Giorno_visita = as.integer(format(festivi$Giorno, "%d"))
festivi=festivi[festivi$Mese_visita>9,]
data2 = merge(data2, festivi, by=c("Mese_visita","Giorno_visita"), all=TRUE)
data2 = subset(data2, select=c(-Giorno))
data2= na.omit(data2)
rm(festivi)
########################################## aggiunta dati precipitazioni ###############################
names(precipitazioni) = c("Id_sensore","Data","Valore")
temp = as.data.frame(str_split_fixed(precipitazioni$Data, " ", 2))
colnames(temp) = c("Giorno","Ora")
precipitazioni$Giorno = temp$Giorno
precipitazioni$Ora = temp$Ora
rm(temp)
precipitazioni = precipitazioni[,3:5]


precipitazioni$Giorno =  as.Date(precipitazioni$Giorno, format = "%Y/%m/%d")
precipitazioni$MeseRilevazione = as.integer(format(precipitazioni$Giorno, "%m"))
precipitazioni$GiornoRilevazione = as.integer(format(precipitazioni$Giorno, "%d"))

precipitazioni$Ora = as.integer(format(as.POSIXct(precipitazioni$Ora,format="%H:%M"),"%H"))

# solo precipitazioni nella fascai 6-18
precipitazioni = precipitazioni[precipitazioni$Ora %in% c(6:18),]

precipitazioni = precipitazioni[,-2]

# calcolo precipitazioni medie per fascia oraria
precipitazioni = precipitazioni[with(precipitazioni,order(precipitazioni$MeseRilevazione,
                                                          precipitazioni$GiornoRilevazione,
                                                          precipitazioni$Ora)),]

precipitazioni$Valore = as.character(precipitazioni$Valore)
precipitazioni$Valore = as.double(precipitazioni$Valore)
precipitazioni_medie_per_fascia = unique(precipitazioni[c("MeseRilevazione", "GiornoRilevazione","Ora")])
precipitazioni_medie_per_fascia$MediaP = as.double(rep(0,nrow(precipitazioni_medie_per_fascia)))

for (i in c(1:nrow(precipitazioni_medie_per_fascia))) {

  dummy_mese = precipitazioni$MeseRilevazione==precipitazioni_medie_per_fascia$MeseRilevazione[i]

  dummy_giorno = precipitazioni$GiornoRilevazione==precipitazioni_medie_per_fascia$GiornoRilevazione[i]

  dummy_fascia = precipitazioni$Ora==precipitazioni_medie_per_fascia$Ora[i]

  temp = precipitazioni[dummy_mese & dummy_giorno & dummy_fascia,]

  m = mean(temp$Valore)

  precipitazioni_medie_per_fascia[i,]$MediaP = m
}

rm(precipitazioni,temp)

# merge data e precipitazioni medie per fascia oraria giornaliera
colnames(data2)[1] = "Mese"
colnames(data2)[2] = "Giorno"
colnames(data2)[3] = "Ora"

colnames(precipitazioni_medie_per_fascia)[1] = "Mese"
colnames(precipitazioni_medie_per_fascia)[2] = "Giorno"
colnames(precipitazioni_medie_per_fascia)[3] = "Ora"

data2 <- precipitazioni_medie_per_fascia %>% right_join(data2, by=c("Mese","Giorno","Ora"))

rm(precipitazioni_medie_per_fascia)
#elimino le colonne già presenti nel dataset di partenza
data2=subset(data2, select=c(-Giorno,-Mese,-Ora))
#eseguo un merge dei dataset
data = merge(data2, data, by=c("Indice"), all=TRUE)
rm(data2)

#divido il dataset per disegnare meglio il grafico di correlazione
data_p1=data[,c(1:10,13)]
data_p2=data[,c(11:21)]
data_p3=data[,c(13,22:length(data))]

data_p3$media3=as.numeric(as.character(data_p3$media3))
data_p3$media5=as.numeric(as.character(data_p3$media5))

write.csv(data, file = "./Dataset/finale_int.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

#### correlation
res = cor(data_p1)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/integrata1.png", width = 1024)
corrplot(res, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
dev.off()

res2 = cor(data_p2)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/integrata2.png", width = 1024)
corrplot(res2, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
dev.off()
res3 = cor(data_p3)
col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
png(filename = "Correlation/integrata3.png", width = 1024)
corrplot(res3, method="color", col=col(200),
         type="upper", order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)
dev.off()

rm(data_p1)
rm(data_p2)
rm(data_p3)


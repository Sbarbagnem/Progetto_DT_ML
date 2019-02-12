##################################### INFO #####################################
#Progetto di ML
#Progetto: waiting  prediction system
#Team:
# - Samuele Ventura, 793060 
# - Luca Virgilio, 794866

#ML- randomForest

# librerie necessarie
if (!require("randomForest"))
  install.packages("randomForest", dependencies = T)
library("randomForest")
if (!require("caret"))
  install.packages("caret", dependencies = T)
library("caret")

setwd(paste0("C:/Users/luca/Desktop/progettoML"))

dataset = read.csv2(file = "./Dataset/finale_int.csv", header = TRUE, sep = ",")

#rimuovo gli outlier. Valori oltre il 99 percentile
dataset = dataset[dataset$Attesa<quantile(dataset$Attesa, 0.99),]

############################################
#per una migliore computazione tolgo gli attributi fortemente correlati o che non posso utilizzare 
#nella predizione come n sportello e n operatore
dataset = subset(dataset, select=-c(Chiamato_alle_ore,Stato,Operatore_numero,Sportello,Ultima_operazione_alle,
                                    Durata,Indice,mediana3,media3,L2t,L3t,Efficienza_sportello,Efficienza_sportello_servizio))


##############
#per numeri random. in modo da poter ripetere la computazione
set.seed(101)
#creazione del trainset [solo indici]
train = sample (1:nrow(dataset), (0.70 * nrow(dataset)))
# parametri prestabiliti
rf.dataset = randomForest(Attesa~., data = dataset, subset = train)

rf.dataset

saveRDS(rf.dataset, file="../forest_cor1.rds")
#plot-randoforest easy
plot(rf.dataset, type="l", main="Random Forest regression")

#stima parametro mtry
oob.err = double(9)
test.err = double(9)
for(mtry in 1:9){
  fit = randomForest(Attesa~., data = dataset, subset=train, mtry=mtry, ntree = 300)
  oob.err[mtry] = fit$mse[300]
  pred = predict(fit, dataset[-train,])
  test.err[mtry] = with(dataset[-train,], mean( (Attesa-pred)^2 ))
}

# converto da metrica MSE --> RMSE
test.err = sqrt(test.err)
oob.err = sqrt(oob.err)
# salvo i risultati
error_reg = data.frame(oob.err, test.err)
save(error_reg, file="./forest_reg_err.rds")


#plot mtry n-features
matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b",
        ylab="RMSE", xlab = "mtry", main="Tune parameter Mtry")
legend("topright", legend = c("OOB error", "Test error"), pch = 23, col = c("red", "blue"))



#singola computazione random forest
set.seed(77)
train = sample (1:nrow(dataset), (0.70 * nrow(dataset)))
rf.reg = randomForest(Attesa~., data = dataset, subset = train, mtry=7, ntree=300)
rf.reg
#calcolato in RMSE basato su OOB... da predict del test esce 15!!!
res_reg = sqrt(rf.reg$mse[300])
saveRDS(rf.reg, file="../forest_cor2.rds")
saveRDS(res_reg, file="../res_reg.rds")

############# 10 fold cross validation ###################
test_res = rep(0, 10)
#shuffle the data
data_s <- dataset[sample(nrow(dataset)),]
#Create 5 equally size folds
folds = cut(seq(1, nrow(data_s)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_s[testIndexes, ]
  trainData <- data_s[-testIndexes, ]
  #Use the test and train data per addestrare e testare il modello
  rf_temp <-randomForest(Attesa~., data = trainData, ntree=300, mtry=7)
  p1 = predict(rf_temp, testData)
  test_res[i] = sqrt(mean((testData$Attesa-p1)^2))
  }

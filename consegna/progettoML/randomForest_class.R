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
if (!require("ROCR"))
  install.packages("ROCR", dependencies = T)
library(ROCR)

setwd(paste0("C:/Users/luca/Desktop/progettoML"))

dataset = read.csv2(file = "./dataset/finale_int.csv", header = TRUE, sep = ",")

#rimuovo gli outlier. Valori oltre il 95 percentile
dataset = dataset[dataset$Attesa<quantile(dataset$Attesa, 0.95),]
############################
# discretizzo attesa 

# <=10 --> 1
# 11-20 --> 2
# 21-30 --> 3
# >30 --> 4

dataset$Attesa[dataset$Attesa <= 10] = 1
dataset$Attesa[dataset$Attesa %in% c(11:20)] = 2
dataset$Attesa[dataset$Attesa %in% c(21:30)] = 3
dataset$Attesa[dataset$Attesa >30] = 4

#mostramo il numero di istanze per classificazione
table(dataset$Attesa)

dataset$Attesa <- as.factor(dataset$Attesa)

############################################
#per una migliore computazione tolgo gli attributi fortemente correlati o che non posso utilizzare 
#nella predizione come n sportello e n operatore
dataset = subset(dataset, select=-c(Chiamato_alle_ore,Stato,Operatore_numero,Sportello,Ultima_operazione_alle,
                       Durata,Indice,mediana3,media3,L2t,L3t,Efficienza_sportello,Efficienza_sportello_servizio))

#prima esecuzione di random forest
rf.dataset = randomForest(Attesa~., data = dataset)
rf.dataset
feature_s=importance(rf.dataset)
#stima paremetro tree
plot1 = plot(rf.dataset)
png(filename = "Image/forest_class_01.png", width = 1024)
plot(rf.dataset)
dev.off()
#oltre i 300 alberi non si ottiene un miglioramento

#stima parametro mtry
t <- tuneRF(dataset[,-8], dataset[,8], stepFactor = 0.5,
            plot=TRUE, ntreeTry = 300, trace = TRUE, 
            improve=0.05)

#fatures selection
varImp(rf.dataset)
#png(filename = "Image/forest_class_03.png", width = 1024)
varImpPlot(rf.dataset)
#dev.off()
varUsed(rf.dataset)

#nuova esecuzione 70-30
set.seed(77)
indexes = sample(1:nrow(dataset),(0.70 * nrow(dataset)), replace = F)
test =dataset[-indexes,]
train=dataset[indexes,]
rf <-randomForest(Attesa~., data = train, ntree=300, mtry=4)
t_pred_base <- predict(rf, test)
c = confusionMatrix(p1,test$Attesa)

#partial dependence Plot per mostrare i problemi di classificazione
partialPlot(rf, train, med5,"2"  )


###roc
aucs = c()
#empty plot
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate', xlab='False Positive Rate', 
     main = "ROC for Random Forest (trainset: 70%, testset: 30%)", 
     sub = "1 class vs all the others classes", bty='n')
for(category in 1:4) {
  #calcolate prediction as 1 vs all, so hold 1 class and set it as 1 and 
  # the others to 0:
  testset.cnt = as.numeric(test$Attesa)
  tot = length(t_pred_base)
  temp_pred = rep(1, tot)
  i=1
  for(i in 1:tot) {
    #set to 0 other class in prediction and hold as 1 only prediction = category
    if(t_pred_base[i] != category) {
      temp_pred[i] = 0
    }
    #set to 0 other class in testset and hold as 1 only cnt = category
    if(testset.cnt[i] != category) {
      testset.cnt[i] = 0
    }
  }
  pred = ROCR::prediction(temp_pred, testset.cnt)
  #calcolate roc curve
  tpr_rocr = performance(pred, measure = "tpr","fpr")
  roc.x = unlist(tpr_rocr@x.values)
  roc.y = unlist(tpr_rocr@y.values)
  lines(roc.y ~ roc.x, col=category+1, lwd=2)
  #calcolate auc
  rocr = performance(pred, measure = "auc", x.measure = "cutoff")
  rocr = unlist(slot(rocr, "y.values"))
  aucs[category] = rocr
}
auc = mean(aucs) #means of every auc
lines(x=c(0,1), c(0,1))
grid(NULL, NULL, lty = 5)
legend("bottomright", col = c(2, 3, 4, 5), pch = 1, lwd = 1, title = "Attesa: ",
       legend = c(paste0("<=10"," AUC:", 
                         as.character(round(aucs[1], digits = 2))), 
                  paste0("11-20"," AUC:", 
                         as.character(round(aucs[2], digits = 2))),
                  paste0("21-30"," AUC:", 
                         as.character(round(aucs[3], digits = 2))),
                  paste0(">30"," AUC:", 
                         as.character(round(aucs[4], digits = 2)))))
legend(x = 0.25, y = 0.15, bty = "n", 
       legend = paste0("AUC mean: ", as.character(round(auc, digits = 2))))


#################### function ###########################################
#function to calculate accuracy of a confusion matrix
calc_accuracy = function(cm) {
  return(sum(diag(cm))/sum(cm))
}

#function to calculate precision of a confusion matrix
calc_precision = function(cm) {
  i = 1
  p = 0
  tot = nrow(cm)
  for(i in 1:tot) {
    p = p + cm[i,i]/sum(cm[,i])
  }
  return(p/tot)
}

#function to calculate recall of a confusion matrix
calc_recall = function(cm) {
  i = 1
  p = 0
  tot = nrow(cm)
  for(i in 1:tot) {
    p = p + cm[i,i]/sum(cm[i,])
  }
  return(p/tot)
}

#function to calculate f-measure of a confusion matrix
calc_fmeasure = function(precision, recall) {
  return((2 * (precision * recall)) / (precision + recall))
}


##########################10-fold cross validation##################################
accuracies_res = rep(0, 10)
precisions_res = rep(0, 10)
recalls_res = rep(0, 10)
fmeasures_res = rep(0, 10)

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
  rf_temp <-randomForest(Attesa~., data = trainData, ntree=300, mtry=4)
  p1 = predict(rf_temp, testData)
  c = confusionMatrix(p1,testData$Attesa) 
  #calcolo delle metriche di performance
  accuracies_res[i]=calc_accuracy(c$table)
  precisions_res[i] = calc_precision(c$table)
  recalls_res[i] =  calc_precision(c$table)
  fmeasures_res[i]= calc_fmeasure(precisions_res[i],recalls_res[i])
} 

  
result = data.frame(accuracies_res, precisions_res, recalls_res, fmeasures_res)
names(result) = c("Accuratezza", "Precisione", "Recall", "Fmeasure")
save(result, file="./res_forest_clas.rds")

# Create line chart of 10-fold cross validation
xrange = c(1:10)
yrange = range(c(0:1))

# set up the plot 
png("Plot10k.png")
plot(xrange, yrange, type="n", xlab="fold",
     ylab="performance" ) 
colors <- rainbow(4) 
linetype <- c(1:4) 
plotchar <- c(1:4)

# add lines accuracy
lines(c(1:10), accuracies_res, type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1]) 
# add lines accuracy
lines(c(1:10), precisions_res, type="b", lwd=1.5, lty=linetype[2], col=colors[2], pch=plotchar[2])
# add lines accuracy
lines(c(1:10), recalls_res, type="b", lwd=1.5, lty=linetype[3], col=colors[3], pch=plotchar[3])
# add lines accuracy
lines(c(1:10), fmeasures_res, type="b", lwd=1.5, lty=linetype[4], col=colors[4], pch=plotchar[4])

grid(NULL, NULL, lty = 5)

legend("bottomright", col = colors, pch = plotchar, lwd = 1.5, lty = linetype,
       legend = c(paste0("accuracy"),
                  paste0("precision"),
                  paste0("recall"),
                  paste0("fmeasure")),
       title = "Performance")

# add a title and subtitle 
title("Performance per 10-cross validation for Random Forest")
dev.off()
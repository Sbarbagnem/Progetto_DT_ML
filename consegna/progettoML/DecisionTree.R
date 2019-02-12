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

# discretizzo attesa 

# <=10 --> 1
# 11-20 --> 2
# 21-30 --> 3
# >30 --> 4

dataset$Attesa[dataset$Attesa <= 10] = 1
dataset$Attesa[dataset$Attesa %in% c(11:20)] = 2
dataset$Attesa[dataset$Attesa %in% c(21:30)] = 3
dataset$Attesa[dataset$Attesa >30] = 4

########### poi da togliere##########
dataset = subset(dataset, select=-c(Efficienza_sportello,Efficienza_sportello_servizio))

dataset = subset(dataset, select=-c(Chiamato_alle_ore,Stato,Operatore_numero,Sportello,Ultima_operazione_alle,
                       Durata,Indice,media3,mediana3,L2t,L3t))


################ DECISION TREE ##############
set.seed(100)
trainIndex <- createDataPartition(dataset$Attesa, p = .7, 
                                  list = FALSE, 
                                  times = 1)

trainingset <- dataset[ trainIndex,]
test  <- dataset[-trainIndex,]

# DT1: decision tree with defaults parameters and all attributes (accuratezza 0.63)
decisionTree_base = rpart(Attesa ~ ., data=trainingset, method="class")

fancyRpartPlot(decisionTree_base, sub = "")

t_pred_base = predict(decisionTree_base, test, type="class")
confMat_base <- table(test$Attesa, t_pred_base)

base_accuracy <- sum(diag(confMat_base))/sum(confMat_base)
base_precision <- diag(confMat_base) / rowSums(confMat_base)
base_recall <- diag(confMat_base) / colSums(confMat_base)
base_F_Measure = 2 * base_precision * base_recall / (base_precision + base_recall)

plotcp(decisionTree_base)
printcp(decisionTree_base)

# DT2: decision tree with custom parameters and all attributes (sale accuratezza a 0.66 ma troppo splittato)
decisionTree_base = rpart(Attesa ~ ., data=trainingset, method="class",cp =0.001)

fancyRpartPlot(decisionTree_base, sub = "")

t_pred_base = predict(decisionTree_base, test, type="class")
confMat_base <- table(test$Attesa, t_pred_base)

base_accuracy <- sum(diag(confMat_base))/sum(confMat_base)
base_precision <- diag(confMat_base) / rowSums(confMat_base)
base_recall <- diag(confMat_base) / colSums(confMat_base)
base_F_Measure = 2 * base_precision * base_recall / (base_precision + base_recall)

plotcp(decisionTree_base)
printcp(decisionTree_base)

# DT3: decision tree with best compromise of readibility and accuracy (accuracy 0.63 e un nodo in meno)
# decisionTree_base = rpart(Attesa ~ ., data=trainingset, method="class", cp = 0.001) # valore che migliora leggibilita' e perdo 0.01 di accuracy
decisionTree_base = prune(decisionTree_base, cp = 0.012) 
fancyRpartPlot(decisionTree_base, sub="")

t_pred_base = predict(decisionTree_base, test, type="class")
confMat_base <- table(test$Attesa, t_pred_base)

base_accuracy <- sum(diag(confMat_base))/sum(confMat_base)
base_precision <- diag(confMat_base) / rowSums(confMat_base)
base_recall <- diag(confMat_base) / colSums(confMat_base)
base_F_Measure = 2 * base_precision * base_recall / (base_precision + base_recall)

plotcp(decisionTree_base)
printcp(decisionTree_base)

######### ROC #######

#ROC curves for both the models

#OBS: Use one vs all approach to do ROC curve on a multiclass classifier.
#Consider one class like 1 and other class like 0, do it for all class and
# compare the results

#ROC curve for bnet
#plot all the single curve together to compare them
#array for auc means

aucs = c()
#empty plot
png("RocDT3.png")
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate', xlab='False Positive Rate', 
     main = "ROC for DT3 (trainset: 70%, testset: 30%)", 
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
legend("bottomright", col = c(2, 3, 4, 5), pch = 1, lwd = 1, title = "Attesa:",
       legend = c(paste0("<=10"," AUC:", 
                         as.character(round(aucs[1], digits = 2))), 
                  paste0("11-20"," AUC:", 
                         as.character(round(aucs[2], digits = 2))),
                  paste0("21-30"," AUC:", 
                         as.character(round(aucs[3], digits = 2))),
                  paste0(">30"," AUC:", 
                         as.character(round(aucs[4], digits = 2)))))
legend(x = 0.25, y = 0.15, bty = "n", 
       legend = paste0("AUC mean: ", as.character(round(auc, digits = 4))))
dev.off()

############# 10-K FOLD VALIDATION #########
set.seed(42)
#Randomly shuffle the data
data = dataset[sample(nrow(dataset)),]
#Create 10 equally size folds
folds = cut(seq(1, nrow(dataset)), breaks=10, labels=FALSE)
#Create accuracies array
accuracies = rep(0, 10)
precisions = rep(0, 10)
recalls = rep(0, 10)
fmeasures = rep(0, 10)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes = which(folds == i, arr.ind = TRUE)
  testData = dataset[testIndexes, ]
  trainData = dataset[-testIndexes, ]
  #now train the model with cp = the cp used for the pruned tree before
  model_5r = rpart(Attesa ~ media5 + mediana5 + Servizio + Presente_alle_ore, 
                   data=trainData, method="class", cp = 0.012)
  #predict the models
  pred = predict(model_5r, testData, type="class")
  #check accuracy, precision, recall and fmeasure
  cm = table(testData$Attesa, pred)
  accuracy = sum(diag(cm))/sum(cm)
  precision = diag(cm) / rowSums(cm)
  recall = diag(cm) / colSums(cm)
  fmeasure = 2 * precision * recall / (precision + recall)
  #add accuracy, precision, recall and fmeasure to the relative list
  accuracies[i] = accuracy
  precisions[i] = precision
  recalls[i] = recall
  fmeasures[i] = fmeasure
}

#mean of accuracies, precisions, recalls and fmeasures
mean_accuracy = mean(accuracies)
mean_precision = mean(precisions)
mean_recalls = mean(recalls)
mean_fmeasure = mean(fmeasures)

temp = data.frame(fold = (1:10), accuracy = accuracies, precision  = precisions, recall = recalls, fmeasure = fmeasures)

# Create line chart of 10-fold cross validation
xrange = range(temp$fold)
yrange = range(c(0:1))

# set up the plot 
png("Plot10k.png")
plot(xrange, yrange, type="n", xlab="fold",
     ylab="performance" ) 
colors <- rainbow(4) 
linetype <- c(1:4) 
plotchar <- c(1:4)

# add lines accuracy
lines(c(1:10), accuracies, type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1]) 
# add lines accuracy
lines(c(1:10), precisions, type="b", lwd=1.5, lty=linetype[2], col=colors[2], pch=plotchar[2])
# add lines accuracy
lines(c(1:10), recalls, type="b", lwd=1.5, lty=linetype[3], col=colors[3], pch=plotchar[3])
# add lines accuracy
lines(c(1:10), fmeasures, type="b", lwd=1.5, lty=linetype[4], col=colors[4], pch=plotchar[4])

grid(NULL, NULL, lty = 5)

legend("bottomright", col = colors, pch = plotchar, lwd = 1.5, lty = linetype,
       legend = c(paste0("accuracy"),
                  paste0("precision"),
                  paste0("recall"),
                  paste0("fmeasure")),
        title = "Performance")

# add a title and subtitle 
title("Performance per 10-cross validation for DT3")
dev.off()
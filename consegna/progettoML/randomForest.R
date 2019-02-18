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
if (!require("plotrix"))
  install.packages("plotrix", dependencies = T)
library("plotrix")

setwd("~/Desktop/Progetto_DT_ML/consegna/progettoML")

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
test = dataset[-train,]
rf.reg = randomForest(Attesa~., data = dataset, subset = train, mtry=7, ntree=300)
rf.reg
pred = predict(rf.reg, test)
temp = sqrt(mean((test$Attesa-pred)^2))
#pred e test$Attesa

png("Taylor.png")
taylor.diagram.modified(test$Attesa, pred, pch=19,pos.cor=TRUE, sd.arcs =TRUE, ref.sd = TRUE, col="red", pcex = 3, text = "Random Forest")
dev.off()

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

taylor.diagram.modified <- function (ref, model, add = FALSE, col = "red", pch = 19, pos.cor = TRUE, 
                                     xlab = "", ylab = "", main = "Taylor Diagram", show.gamma = TRUE, 
                                     ngamma = 3, gamma.col = 8, sd.arcs = 0, ref.sd = FALSE, sd.method = "sample", 
                                     grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1, 
                                     normalize = FALSE, mar = c(5, 4, 6, 6), text, ...) 
{
  grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
                      1)
  R <- cor(ref, model, use = "pairwise")
  if (is.list(ref)) 
    ref <- unlist(ref)
  if (is.list(model)) 
    ref <- unlist(model)
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = TRUE)
    devx <- x - meanx
    ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
                                                  subn))
    return(ssd)
  }
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  maxsd <- 1.5 * max(sd.f, sd.r)
  oldpar <- par("mar", "xpd", "xaxs", "yaxs")
  if (!add) {
    if (pos.cor) {
      if (nchar(ylab) == 0) 
        ylab = "Standard deviation"
      par(mar = mar)
      plot(0, xlim = c(0, maxsd), ylim = c(0, maxsd), xaxs = "i", 
           yaxs = "i", axes = FALSE, main = main, xlab = xlab, 
           ylab = ylab, type = "n", cex = cex.axis, ...)
      if (grad.corr.lines[1]) {
        for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                                               gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3, lwd = 1.2, col = "green")
      }
      segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                                                0))
      axis.ticks <- pretty(c(0, maxsd))
      axis.ticks <- axis.ticks[axis.ticks <= maxsd]
      axis(1, at = axis.ticks, cex.axis = cex.axis)
      axis(2, at = axis.ticks, cex.axis = cex.axis)
      if (sd.arcs[1]) {
        if (length(sd.arcs) == 1) 
          sd.arcs <- axis.ticks
        for (sdarc in sd.arcs) {
          xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
          ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
          lines(xcurve, ycurve, col = "gray", lty = 3, lwd = 1.2)
        }
      }
      if (show.gamma[1]) {
        if (length(show.gamma) > 1) 
          gamma <- show.gamma
        else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
        if (gamma[length(gamma)] > maxsd) 
          gamma <- gamma[-length(gamma)]
        labelpos <- seq(45, 70, length.out = length(gamma))
        for (gindex in 1:length(gamma)) {
          xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + 
            sd.r
          endcurve <- which(xcurve < 0)
          endcurve <- ifelse(length(endcurve), min(endcurve) - 
                               1, 105)
          ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
          maxcurve <- xcurve * xcurve + ycurve * ycurve
          startcurve <- which(maxcurve > maxsd * maxsd)
          startcurve <- ifelse(length(startcurve), max(startcurve) + 
                                 1, 0)
          lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                # col = gamma.col)
                  col = "red", lwd = 3)
          if (xcurve[labelpos[gindex]] > 0) 
            boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                         gamma[gindex], border = FALSE)
        }
      }
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
      lines(xcurve, ycurve)
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                 maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                 0.97 * maxsd)
      par(xpd = TRUE)
      if (ref.sd) {
        xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
        ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
        lines(xcurve, ycurve, col= "violet", lwd= 3)
      }
      points(sd.r, 0, cex = pcex, pch = pch, col = "violet")
      text(sd.r + 2,  2, labels="Osservato", cex = 0.7, pos=1, offset = 0.5)
      text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
             1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                                                       0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                                                                                  0.95, 0.99), cex = cex.axis)
      text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315, 
           cex = cex.axis)
      segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                 maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                 0.98 * maxsd)
      segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                 maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                 0.99 * maxsd)
    }
    else {
      x <- ref
      y <- model
      R <- cor(x, y, use = "pairwise.complete.obs")
      E <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
      xprime <- x - mean(x, na.rm = TRUE)
      yprime <- y - mean(y, na.rm = TRUE)
      sumofsquares <- (xprime - yprime)^2
      Eprime <- sqrt(sum(sumofsquares)/length(complete.cases(x)))
      E2 <- E^2 + Eprime^2
      if (add == FALSE) {
        maxray <- 1.5 * max(sd.f, sd.r)
        plot(c(-maxray, maxray), c(0, maxray), type = "n", 
             asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
             xlab = xlab, ylab = ylab, main = main, cex = cex.axis)
        discrete <- seq(180, 0, by = -1)
        listepoints <- NULL
        for (i in discrete) {
          listepoints <- cbind(listepoints, maxray * 
                                 cos(i * pi/180), maxray * sin(i * pi/180))
        }
        listepoints <- matrix(listepoints, 2, length(listepoints)/2)
        listepoints <- t(listepoints)
        lines(listepoints[, 1], listepoints[, 2])
        lines(c(-maxray, maxray), c(0, 0))
        lines(c(0, 0), c(0, maxray))
        for (i in grad.corr.lines) {
          lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                                                       i^2)), lty = 3)
          lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                                                        i^2)), lty = 3)
        }
        for (i in grad.corr.full) {
          text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                         i^2), i, cex = 0.6)
          text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                                                          i^2), -i, cex = 0.6)
        }
        seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
        for (i in seq.sd) {
          xcircle <- sd.r + (cos(discrete * pi/180) * 
                               i)
          ycircle <- sin(discrete * pi/180) * i
          for (j in 1:length(xcircle)) {
            if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
              points(xcircle[j], ycircle[j], col = "darkgreen", 
                     pch = ".")
              if (j == 10) 
                text(xcircle[j], ycircle[j], signif(i, 
                                                    2), cex = 0.5, col = "darkgreen")
            }
          }
        }
        seq.sd <- seq.int(0, maxray, length.out = 5)
        for (i in seq.sd) {
          xcircle <- (cos(discrete * pi/180) * i)
          ycircle <- sin(discrete * pi/180) * i
          if (i) 
            lines(xcircle, ycircle, lty = 3, col = "blue")
          text(min(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "blue")
          text(max(xcircle), -0.03 * maxray, signif(i, 
                                                    2), cex = 0.5, col = "blue")
        }
        text(0, -0.08 * maxray, "Standard Deviation", 
             cex = 0.7, col = "blue")
        text(0, -0.12 * maxray, "Centered RMS Difference", 
             cex = 0.7, col = "darkgreen")
        points(sd.r, 0, pch = 22, bg = "darkgreen", cex = 1.1)
        text(0, 1.1 * maxray, "Correlation Coefficient", 
             cex = 0.7)
      }
      S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
    }
  }
  points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = "orange", 
         cex = pcex)
  text(sd.f * R - 0.5, sd.f * sin(acos(R)) + 0.7,  #the line to add
       labels=text, cex = 0.7, pos=3, offset = 0.5) 
  invisible(oldpar)
  # legend()
}


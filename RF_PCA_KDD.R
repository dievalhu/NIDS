# Random Forest
# Training Stage
library(randomForest)
library(tictoc)
path1="~/KDDTrain.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=";"))
nids1$V42 <- (nids1$V42=="anomaly")*1  #anomaly is 1

path="~/KDDPCATrain.csv" 
nids = as.data.frame(read.csv(file=path, header=TRUE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(20:37),drop=FALSE]
tic("time_training")
rf <- randomForest(nids1$V42 ~ ., nids, ntree=100)
toc()

#TEST
path2="~/KDDTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=";"))
nids2$V42 <- (nids2$V42=="anomaly")*1  #anomaly is 1

path3="~/KDDPCATest.csv" 
nids3 = as.data.frame(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(20:37),drop=FALSE]
rf_pred = predict(rf, newdata = nids3)
rf_pred = round(rf_pred)
rf_actual = nids2$V42


# Confusion Matrix and Results
library(caret)
xtab <- table(rf_pred,rf_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

# Random Forest
# Training Stage
library(randomForest)
library(tictoc)
path="~/KDDTrain.csv"
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";")) 
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(17),drop=FALSE]
nids$V42 <- (nids$V42=="anomaly")*1  #anomaly is 1
tic("time_training")
rf <- randomForest(nids$V42 ~ ., nids, ntree=100)
toc()

#TEST
path="~/KDDTest.csv" 
nids1 = as.data.frame(read.csv(file=path, header=FALSE, sep=";"))
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(17),drop=FALSE]
nids1$V42 <- (nids1$V42=="anomaly")*1  #anomaly is 1
rf_pred = predict(rf, newdata = nids1[-38])
rf_pred = round(rf_pred)
rf_actual = nids1$V42

# Confusion Matrix and Results
library(caret)
xtab <- table(rf_pred,rf_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

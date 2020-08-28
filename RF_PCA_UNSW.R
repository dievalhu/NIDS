# Support Vector Machine
# Training Stage
library(e1071)
library(caTools)
library(readr)
library(tictoc)
path="~/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
path1="~/UNSWPCATrain.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=TRUE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(17:37),drop=FALSE]
tic("time_training")
rf <- randomForest(nids$V45 ~ ., nids1, ntree=100)
toc()

#TEST
path2="~/UNSWTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=","))
path3="~/UNSWPCATest.csv" 
nids3 = as.data.frame(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(17:37),drop=FALSE]
rf_pred = predict(rf, newdata = nids3)
rf_pred = round(rf_pred)
rf_actual = nids2$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(rf_pred,rf_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

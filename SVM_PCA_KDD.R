# Support Vector Machine
# Training Stage
library(e1071)
library(caTools)
library(readr)
library(tictoc)
path1="~/KDDTrain.csv" 
nids1 = as.matrix(read.csv(file=path1, header=FALSE, sep=";"))
y = nids1[,42] 
y = (y=="anomaly")*1 #anomaly is 1

path="~/KDDPCATrain.csv" 
nids = as.matrix(read.csv(file=path, header=TRUE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(20:37),drop=FALSE]
tic("time_training")
model <- svm(y ~ ., data = nids,type = 'C-classification', kernel = 'radial')
toc()

#TEST
path2="~/KDDTest.csv" 
nids2 = as.matrix(read.csv(file=path2, header=FALSE, sep=";"))
y2 = nids2[,42] 
y2 = (y2=="anomaly")*1 #anomaly is 1

path3="~/KDDPCATest.csv" 
nids3 = as.matrix(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(20:37),drop=FALSE]
svm_pred = predict(model, type = 'response', newdata = nids3)
svm_actual = y2

# Confusion Matrix and Results
library(caret)
xtab <- table(svm_pred,svm_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

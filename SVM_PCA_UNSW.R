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
model <- svm(nids$V45 ~ ., data = nids1,type = 'C-classification', kernel = 'radial')
toc()

#TEST
path2="~/UNSWTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=","))
path3="~/UNSWPCATest.csv" 
nids3 = as.data.frame(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(17:37),drop=FALSE]
svm_pred = predict(model, type = 'response', newdata = nids3)
svm_actual = nids2$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(svm_pred,svm_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

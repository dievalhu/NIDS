# Support Vector Machine
# Training Stage
library(e1071)
library(caTools)
library(readr)
path="C:/Users/Diego/Desktop/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(23),drop=FALSE]
nids <- nids[,-(34),drop=FALSE]
nids <- nids[,-(38),drop=FALSE]
model <- svm(nids$V45 ~ ., data = nids,type = 'C-classification', kernel = 'radial')

#TEST
path1="C:/Users/Diego/Desktop/UNSWTest.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38),drop=FALSE]
svm_pred = predict(model, type = 'response', newdata = nids1[-38])
svm_actual = nids1$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(svm_pred,svm_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

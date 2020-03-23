# Support Vector Machine
# Training Stage
library(e1071)
library(caTools)
library(readr)
path="C:/Users/Diego/Desktop/KDDTrain.csv"
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=";")) 
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(17),drop=FALSE]
nids$V42 <- (nids$V42=="anomaly")*1  #anomaly is 1
model <- svm(nids$V42 ~ ., data = nids,type = 'C-classification', kernel = 'radial')

#TEST
path="C:/Users/Diego/Desktop/KDDTest.csv" 
nids1 = as.data.frame(read.csv(file=path, header=FALSE, sep=";"))
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(17),drop=FALSE]
nids1$V42 <- (nids1$V42=="anomaly")*1  #anomaly is 1
svm_pred = predict(model, type = 'response', newdata = nids1[-38])
svm_actual = nids1$V42

# Confusion Matrix and Results
library(caret)
xtab <- table(svm_pred,svm_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

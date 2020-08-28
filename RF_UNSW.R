# Random Forest
# Training Stage 
library(randomForest)
library(tictoc)
path="~/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(23),drop=FALSE]
nids <- nids[,-(34),drop=FALSE]
nids <- nids[,-(38),drop=FALSE]
tic("time_training")
rf <- randomForest(nids$V45 ~ ., nids, ntree=100)
toc()

# Test Stage 
path1="~/UNSWTest.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38),drop=FALSE]
rf_pred = predict(rf, newdata = nids1[-38])
rf_pred = round(rf_pred)
rf_actual = nids1$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(rf_pred,rf_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive = '0')
print(m)
# Artificial Neural Network
# Training Stage 
library(neuralnet)
library(tictoc)
path="~/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(23),drop=FALSE]
nids <- nids[,-(34),drop=FALSE]
nids <- nids[,-(38),drop=FALSE]
tic("time_training")
ann <- neuralnet(nids$V45 ~ ., nids, hidden = c(1))
toc()

# Test Stage 
path1="~/UNSWTest.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38),drop=FALSE]
output <- compute(ann, nids1[ , c("V2","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25","V26","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V40","V41","V42","V43")])
ann_pred = round(output$net.result)
ann_actual = nids1$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(ann_pred,ann_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive = '0')
print(m)
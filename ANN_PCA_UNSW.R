# Artificial Neural Network
# Training Stage 
library(neuralnet)
library(tictoc)
path="~/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
path1="~/UNSWPCATrain.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=TRUE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(17:37),drop=FALSE]
tic("time_training")
ann <- neuralnet(nids$V45 ~ ., nids1, hidden = c(1),threshold = 0.1)
toc()

# Test Stage 
path2="~/UNSWTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=","))
path3="~/UNSWPCATest.csv" 
nids3 = as.data.frame(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(17:37),drop=FALSE]
output <- compute(ann, nids3[ , c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16")])
ann_pred = round(output$net.result)
ann_actual = nids2$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(ann_pred,ann_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive = '0')
print(m)

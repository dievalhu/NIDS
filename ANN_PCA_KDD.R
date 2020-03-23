# Artificial Neural Network
# Training Stage 
library(neuralnet)
path0="C:/Users/Diego/Desktop/KDDTrain.csv" 
nids0 = as.matrix(read.csv(file=path0, header=FALSE, sep=";"))
y = nids0[,42] 
y = (y=="anomaly")*1 #anomaly is 1
nids <- read.csv("C:/Users/Diego/Desktop/KDDPCATrain.csv", header=TRUE, sep=",")
nids <- nids[,-(1),drop=FALSE]
x1 = nids[,1] 
x1 = as.numeric(x1) 
x2 = nids[,2] 
x2 = as.numeric(x2) 
x3 = nids[,3] 
x3 = as.numeric(x3) 
x4 = nids[,4] 
x4 = as.numeric(x4) 
x5 = nids[,5] 
x5 = as.numeric(x5) 
x6 = nids[,6] 
x6 = as.numeric(x6) 
x7 = nids[,7] 
x7 = as.numeric(x7) 
x8 = nids[,8] 
x8 = as.numeric(x8) 
x9 = nids[,9] 
x9 = as.numeric(x9) 
x10 = nids[,10] 
x10 = as.numeric(x10) 
x11 = nids[,11] 
x11 = as.numeric(x11) 
x12 = nids[,12] 
x12 = as.numeric(x12) 
x13 = nids[,13] 
x13 = as.numeric(x13) 
x14 = nids[,14] 
x14 = as.numeric(x14) 
x15 = nids[,15] 
x15 = as.numeric(x15) 
x16 = nids[,16] 
x16 = as.numeric(x16)
x17 = nids[,17] 
x17 = as.numeric(x17) 
x18 = nids[,18] 
x18 = as.numeric(x18) 
x19 = nids[,19] 
x19 = as.numeric(x19) 
ntrain = cbind.data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,y)
ann <- neuralnet(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19, ntrain, hidden = c(1), threshold = 0.1)

# Test Stage 
path2="C:/Users/Diego/Desktop/KDDTest.csv" 
nids2 = as.matrix(read.csv(file=path2, header=FALSE, sep=";"))
yt = nids2[,42] 
yt = (yt=="anomaly")*1 #anomaly is 1
nids1 <- read.csv("C:/Users/Diego/Desktop/KDDPCATest.csv", header=TRUE, sep=",")
nids1 <- nids1[,-(1),drop=FALSE]
x1t = nids1[,1] 
x1t = as.numeric(x1t) 
x2t = nids1[,2] 
x2t = as.numeric(x2t) 
x3t = nids1[,3] 
x3t = as.numeric(x3t) 
x4t = nids1[,4] 
x4t = as.numeric(x4t) 
x5t = nids1[,5] 
x5t = as.numeric(x5t) 
x6t = nids1[,6] 
x6t = as.numeric(x6t) 
x7t = nids1[,7] 
x7t = as.numeric(x7t) 
x8t = nids1[,8] 
x8t = as.numeric(x8t) 
x9t = nids1[,9] 
x9t = as.numeric(x9t) 
x10t = nids1[,10] 
x10t = as.numeric(x10t) 
x11t = nids1[,11] 
x11t = as.numeric(x11t) 
x12t = nids1[,12] 
x12t = as.numeric(x12t) 
x13t = nids1[,13] 
x13t = as.numeric(x13t) 
x14t = nids1[,14] 
x14t = as.numeric(x14t) 
x15t = nids1[,15] 
x15t = as.numeric(x15t) 
x16t = nids1[,16] 
x16t = as.numeric(x16t)
x17t = nids1[,17] 
x17t = as.numeric(x17t) 
x18t = nids1[,18] 
x18t = as.numeric(x18t) 
x19t = nids1[,19] 
x19t = as.numeric(x19t) 
ntest = cbind.data.frame(x1t,x2t,x3t,x4t,x5t,x6t,x7t,x8t,x9t,x10t,x11t,x12t,x13t,x14t,x15t,x16t,x17t,x18t,x19t,yt)
output <- compute(ann, ntest[ , c("x1t","x2t","x3t","x4t","x5t","x6t","x7t","x8t","x9t","x10t","x11t","x12t","x13t","x14t","x15t","x16t","x17t","x18t","x19t")])
ann_pred = round(output$net.result)
ann_actual = ntest$yt

# Confusion Matrix and Results
library(caret)
xtab <- table(ann_pred,ann_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive = '0')
print(m)
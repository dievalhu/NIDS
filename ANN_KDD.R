# Artificial Neural Network
# Training Stage 
library(neuralnet)
library(tictoc)
nids<- read.csv("~/KDDTrain.csv", header=FALSE, sep=";")
y = nids[,42] 
y = (y=="anomaly")*1 #anomaly is 1
x1 = nids[,1] 
x1 = as.numeric(x1) 
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
x21 = nids[,21] 
x21 = as.numeric(x21) 
x22 = nids[,22] 
x22 = as.numeric(x22) 
x23 = nids[,23] 
x23 = as.numeric(x23) 
x24 = nids[,24] 
x24 = as.numeric(x24) 
x25 = nids[,25] 
x25 = as.numeric(x25) 
x26 = nids[,26] 
x26 = as.numeric(x26) 
x27 = nids[,27] 
x27 = as.numeric(x27) 
x28 = nids[,28] 
x28 = as.numeric(x28) 
x29 = nids[,29] 
x29 = as.numeric(x29) 
x30 = nids[,30] 
x30 = as.numeric(x30) 
x31 = nids[,31] 
x31 = as.numeric(x31) 
x32 = nids[,32] 
x32 = as.numeric(x32) 
x33 = nids[,33] 
x33 = as.numeric(x33) 
x34 = nids[,34] 
x34 = as.numeric(x34) 
x35 = nids[,35] 
x35 = as.numeric(x35) 
x36 = nids[,36] 
x36 = as.numeric(x36) 
x37 = nids[,37] 
x37 = as.numeric(x37) 
x38 = nids[,38] 
x38 = as.numeric(x38) 
x39 = nids[,39] 
x39 = as.numeric(x39) 
x40 = nids[,40] 
x40 = as.numeric(x40) 
x41 = nids[,41] 
x41 = as.numeric(x41) 
ntrain = cbind.data.frame(x1,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,y)
tic("time_training")
ann <- neuralnet(y ~ x1+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+x41, ntrain, hidden = c(1))
toc()

# Test Stage 
nids1<- read.csv("~/KDDTest.csv", header=FALSE, sep=";")
yt = nids1[,42] 
yt = (yt=="anomaly")*1 #anomaly is 1
x1t = nids1[,1] 
x1t = as.numeric(x1t) 
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
x21t = nids1[,21] 
x21t = as.numeric(x21t) 
x22t = nids1[,22] 
x22t = as.numeric(x22t) 
x23t = nids1[,23] 
x23t = as.numeric(x23t) 
x24t = nids1[,24] 
x24t = as.numeric(x24t) 
x25t = nids1[,25] 
x25t = as.numeric(x25t) 
x26t = nids1[,26] 
x26t = as.numeric(x26t) 
x27t = nids1[,27] 
x27t = as.numeric(x27t) 
x28t = nids1[,28] 
x28t = as.numeric(x28t) 
x29t = nids1[,29] 
x29t = as.numeric(x29t) 
x30t = nids1[,30] 
x30t = as.numeric(x30t) 
x31t = nids1[,31] 
x31t = as.numeric(x31t) 
x32t = nids1[,32] 
x32t = as.numeric(x32t) 
x33t = nids1[,33] 
x33t = as.numeric(x33t) 
x34t = nids1[,34] 
x34t = as.numeric(x34t) 
x35t = nids1[,35] 
x35t = as.numeric(x35t) 
x36t = nids1[,36] 
x36t = as.numeric(x36t) 
x37t = nids1[,37] 
x37t = as.numeric(x37t) 
x38t = nids1[,38] 
x38t = as.numeric(x38t) 
x39t = nids1[,39] 
x39t = as.numeric(x39t) 
x40t = nids1[,40] 
x40t = as.numeric(x40t) 
x41t = nids1[,41] 
x41t = as.numeric(x41t) 
ntest = cbind.data.frame(x1t,x5t,x6t,x7t,x8t,x9t,x10t,x11t,x12t,x13t,x14t,x15t,x16t,x17t,x18t,x19t,x21t,x22t,x23t,x24t,x25t,x26t,x27t,x28t,x29t,x30t,x31t,x32t,x33t,x34t,x35t,x36t,x37t,x38t,x39t,x40t,x41t,yt)
output <- compute(ann, ntest[ , c("x1t","x5t","x6t","x7t","x8t","x9t","x10t","x11t","x12t","x13t","x14t","x15t","x16t","x17t","x18t","x19t","x21t","x22t","x23t","x24t","x25t","x26t","x27t","x28t","x29t","x30t","x31t","x32t","x33t","x34t","x35t","x36t","x37t","x38t","x39t","x40t","x41t")])
ann_pred = round(output$net.result)
ann_actual = ntest$yt

# Confusion Matrix and Results
library(caret)
xtab <- table(ann_pred,ann_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive = '0')
print(m)
# Correlation Matrix
library("corrplot") 

# PCA Training
path1 = "C:/Users/Diego/Desktop/UNSWTrain.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38:39),drop=FALSE]
corr1 <- cor(nids1) 
corrplot(corr1,tl.cex=0.5) #plot

# Barlett Test
bt1 = bartlett.test(nids1[, 1:37])

# Kaiser, Meyer, Olkin - KMO
#install.packages("REdaS")
library("REdaS")
kmo1 = KMOS(nids1[, 1:37])

# PCA
pca_train = prcomp(nids1, center = TRUE, scale. = TRUE) 
summary(pca_train)

# Principal Components 
comp_train = predict(pca_train, newdata=tail(nids1, dim(nids1)[1]))

# Export New Training Dataset
write.csv(comp_train, file = "C:/Users/Diego/Desktop/UNSWPCATrain.csv")

# PCA Test
path2 = "C:/Users/Diego/Desktop/UNSWTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=","))
nids2 <- nids2[,-(1),drop=FALSE]
nids2 <- nids2[,-(2:4),drop=FALSE]
nids2 <- nids2[,-(23),drop=FALSE]
nids2 <- nids2[,-(34),drop=FALSE]
nids2 <- nids2[,-(38:39),drop=FALSE]
corr2 <- cor(nids2) 
corrplot(corr2,tl.cex=0.5) #plot

# Barlett Test
bt2 = bartlett.test(nids2[, 1:37])

# Kaiser, Meyer, Olkin - KMO
#install.packages("REdaS")
library("REdaS")
kmo2 = KMOS(nids2[, 1:37])

# PCA
pca_test = prcomp(nids2, center = TRUE, scale. = TRUE) 
summary(pca_test)

# Principal Components 
comp_test = predict(pca_test, newdata=tail(nids2, dim(nids2)[1]))

# Export New Training Dataset
write.csv(comp_test, file = "C:/Users/Diego/Desktop/UNSWPCATest.csv")

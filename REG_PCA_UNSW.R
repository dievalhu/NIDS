# Logistic Regression
# Training Stage 
path="C:/Users/Diego/Desktop/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
path1="C:/Users/Diego/Desktop/UNSWPCATrain.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=TRUE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(17:37),drop=FALSE]
nids_sol = glm(nids$V45 ~ .,family=binomial(link="logit"),data = nids1)

# Test Stage 
path2="C:/Users/Diego/Desktop/UNSWTest.csv" 
nids2 = as.data.frame(read.csv(file=path2, header=FALSE, sep=","))
path3="C:/Users/Diego/Desktop/UNSWPCATest.csv" 
nids3 = as.data.frame(read.csv(file=path3, header=TRUE, sep=","))
nids3 <- nids3[,-(1),drop=FALSE]
nids3 <- nids3[,-(17:37),drop=FALSE]
y_estimt <- predict(nids_sol,newdata=subset(nids3,select=c(1:16)),type='response')

# Logistic Regression Equation 
maxit = max(y_estimt) 
minit = min(y_estimt) 
tresholdt = (minit+maxit)/2 # treshold decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #vector of treshold decision
reg_pred = (y_estimt > tresholdt)*1 
reg_actual = nids2$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(reg_pred,reg_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)
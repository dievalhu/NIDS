# Logistic Regression
# Training Stage 
path="C:/Users/Diego/Desktop/UNSWTrain.csv" 
nids = as.data.frame(read.csv(file=path, header=FALSE, sep=","))
nids <- nids[,-(1),drop=FALSE]
nids <- nids[,-(2:4),drop=FALSE]
nids <- nids[,-(23),drop=FALSE]
nids <- nids[,-(34),drop=FALSE]
nids <- nids[,-(38),drop=FALSE]
nids_sol = glm(nids$V45 ~ .,family=binomial(link="logit"),data = nids)

# Test Stage 
path1="C:/Users/Diego/Desktop/UNSWTest.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38),drop=FALSE]
y_estimt <- predict(nids_sol,newdata=subset(nids1,select=c(1:37)),type='response')

# Logistic Regression Equation 
maxit = max(y_estimt) 
minit = min(y_estimt) 
tresholdt = (minit+maxit)/2 # treshold decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #vector of treshold decision
reg_pred = (y_estimt > tresholdt)*1 
reg_actual = nids1$V45

# Confusion Matrix and Results
library(caret)
xtab <- table(reg_pred,reg_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)
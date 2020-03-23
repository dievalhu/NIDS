# Logistic Regression
# Training Stage 
path1="C:/Users/Diego/Desktop/KDDTrain.csv" 
nids1 = as.matrix(read.csv(file=path1, header=FALSE, sep=";"))
y = nids1[,42] 
y = (y=="anomaly")*1 #anomaly is 1

path="C:/Users/Diego/Desktop/KDDPCATrain.csv" 
nids = as.matrix(read.csv(file=path, header=TRUE, sep=","))
x1 = nids[,2] 
x1 = as.numeric(x1) 
x2 = nids[,3] 
x2 = as.numeric(x2) 
x3 = nids[,4] 
x3 = as.numeric(x3) 
x4 = nids[,5] 
x4 = as.numeric(x4) 
x5 = nids[,6] 
x5 = as.numeric(x5) 
x6 = nids[,7] 
x6 = as.numeric(x6) 
x7 = nids[,8] 
x7 = as.numeric(x7) 
x8 = nids[,9] 
x8 = as.numeric(x8) 
x9 = nids[,10] 
x9 = as.numeric(x9) 
x10 = nids[,11] 
x10 = as.numeric(x10) 
x11 = nids[,12] 
x11 = as.numeric(x11) 
x12 = nids[,13] 
x12 = as.numeric(x12) 
x13 = nids[,14] 
x13= as.numeric(x13) 
x14 = nids[,15] 
x14 = as.numeric(x14) 
x15 = nids[,16] 
x15 = as.numeric(x15) 
x16 = nids[,17] 
x16 = as.numeric(x16) 
x17 = nids[,18] 
x17 = as.numeric(x17) 
x18 = nids[,19] 
x18 = as.numeric(x18) 
x19 = nids[,20] 
x19 = as.numeric(x19) 

nids_sol = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19,family=binomial(link="logit"),
               control = list(maxit = 500)) 

# Test Stage 
path="C:/Users/Diego/Desktop/KDDPCATest.csv" 
nids = as.matrix(read.csv(file=path, header=TRUE, sep=","))
x1t = nids[,2] 
x1t = as.numeric(x1t) 
x2t = nids[,3] 
x2t = as.numeric(x2t) 
x3t = nids[,4] 
x3t = as.numeric(x3t) 
x4t = nids[,5] 
x4t = as.numeric(x4t) 
x5t = nids[,6] 
x5t = as.numeric(x5t) 
x6t = nids[,7] 
x6t = as.numeric(x6t) 
x7t = nids[,8] 
x7t = as.numeric(x7t) 
x8t = nids[,9] 
x8t = as.numeric(x8t) 
x9t = nids[,10] 
x9t = as.numeric(x9t) 
x10t = nids[,11] 
x10t = as.numeric(x10t) 
x11t = nids[,12] 
x11t = as.numeric(x11t) 
x12t = nids[,13] 
x12t = as.numeric(x12t) 
x13t = nids[,14] 
x13t = as.numeric(x13t) 
x14t = nids[,15] 
x14t = as.numeric(x14t) 
x15t = nids[,16] 
x15t = as.numeric(x15t) 
x16t = nids[,17] 
x16t = as.numeric(x16t) 
x17t = nids[,18] 
x17t = as.numeric(x17t) 
x18t = nids[,19] 
x18t = as.numeric(x18t) 
x19t = nids[,20] 
x19t = as.numeric(x19t) 

y_estimt = exp(nids_sol$coefficients[1]+
nids_sol$coefficients[2]*x1t+
nids_sol$coefficients[3]*x2t+
nids_sol$coefficients[4]*x3t+
nids_sol$coefficients[5]*x4t+
nids_sol$coefficients[6]*x5t+
nids_sol$coefficients[7]*x6t+
nids_sol$coefficients[8]*x7t+
nids_sol$coefficients[9]*x8t+
nids_sol$coefficients[10]*x9t+
nids_sol$coefficients[11]*x10t+
nids_sol$coefficients[12]*x11t+
nids_sol$coefficients[13]*x12t+
nids_sol$coefficients[14]*x13t+
nids_sol$coefficients[15]*x14t+
nids_sol$coefficients[16]*x15t+
nids_sol$coefficients[17]*x16t+
nids_sol$coefficients[18]*x17t+
nids_sol$coefficients[19]*x18t+
nids_sol$coefficients[20]*x19t)/ 
(1+exp(nids_sol$coefficients[1]+
nids_sol$coefficients[2]*x1t+
nids_sol$coefficients[3]*x2t+
nids_sol$coefficients[4]*x3t+
nids_sol$coefficients[5]*x4t+
nids_sol$coefficients[6]*x5t+
nids_sol$coefficients[7]*x6t+
nids_sol$coefficients[8]*x7t+
nids_sol$coefficients[9]*x8t+
nids_sol$coefficients[10]*x9t+
nids_sol$coefficients[11]*x10t+
nids_sol$coefficients[12]*x11t+
nids_sol$coefficients[13]*x12t+
nids_sol$coefficients[14]*x13t+
nids_sol$coefficients[15]*x14t+
nids_sol$coefficients[16]*x15t+
nids_sol$coefficients[17]*x16t+
nids_sol$coefficients[18]*x17t+
nids_sol$coefficients[19]*x18t+
nids_sol$coefficients[20]*x19t)) 

y_estimt[is.nan(y_estimt)] <- 1

# Logistic Regression Equation 
maxit = max(y_estimt) 
minit = min(y_estimt) 
tresholdt = (minit+maxit)/2 # treshold decision
treshold_1t = rep(c(tresholdt),each=length(y_estimt)) #vector of treshold decision
reg_pred = (y_estimt > tresholdt)*1 
path2="C:/Users/Diego/Desktop/KDDTest.csv" 
nids2 = as.matrix(read.csv(file=path2, header=FALSE, sep=";"))
yt = nids2[,42] 
yt = (yt=="anomaly")*1 #anomaly is 1
reg_actual = yt

# Confusion Matrix and Results
library(caret)
xtab <- table(reg_pred,reg_actual)
m <- confusionMatrix(xtab[2:1,2:1],positive='0')
print(m)

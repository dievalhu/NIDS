# Multidimensional Scaling
path1 = "~/UNSWTest.csv" 
nids1 = as.data.frame(read.csv(file=path1, header=FALSE, sep=","))
nids1 <- nids1[,-(1),drop=FALSE]
nids1 <- nids1[,-(2:4),drop=FALSE]
nids1 <- nids1[,-(23),drop=FALSE]
nids1 <- nids1[,-(34),drop=FALSE]
nids1 <- nids1[,-(38),drop=FALSE]

# Sampling 10% of total data
le = round(dim(nids1)[1]*0.1)
on = which(nids1$V45 %in% c(1))
m1 = sample(on,(round(le*0.55))) #proportion of ones in the test datased
ze = which(nids1$V45 %in% c(0))
m2 = sample(ze,(round(le*0.45))) #proportion of zeros in the test datased
m = c(m1,m2)
ran = sample(m,length(m))
nids2 <- nids1[ran,]
d = dist(nids2[,1:37], method = "euclidean")
fit = cmdscale(d,eig=TRUE, k=2) # 2 dimensions
x = fit$points[,1] 
y = fit$points[,2]

#Class Identification
clase = as.factor(nids2$V45)
plot(x, y, col=c("red","blue")[clase], cex=.5, xlab="Dimension 1",ylab="Dimension 2")
legend(x = -1.8e+9, y = 1e+9, legend=c("Normal", "Anormal"), col=c("red", "blue"), lty = 1, cex=0.5, pch = 1, horiz=TRUE, box.lty=0, bty="n")

# Clustering: K-Means
grupos = kmeans(nids2[,1:37],2)
g1 = grupos$cluster
g2 = grupos$size
plot(x,y,col=c("red","blue")[g1], cex=.5, xlab="Dimension 1", ylab="Dimension 2")
legend(x = -1.8e+9, y = 1e+9, legend=c("Grupo 1", "Grupo 2"), col=c("red", "blue"), lty = 1, cex=0.5, pch = 1, horiz=TRUE, box.lty=0, bty="n")

# Clustering Evaluation: K-Means
library(aricode)
AMI_e = AMI(nids2$V45+1,g1)
ARI_e = ARI(nids2$V45+1,g1)
library(cluster)
si = silhouette(g1,d)

# Clustering: DHC
library("dendextend")
hc = hclust(d, method = "complete" )
clus2 = cutree(hc, 2)
dend = as.dendrogram(hc)
dend = color_branches(dend, 2)
colors = c("red", "blue")
plot(dend, fill = colors[clus2], cex = 0.1, xlab="Instancias-Cluster", ylab="Distancia" )

# Clustering Evaluation: DHC
AMI_e1 = AMI(nids2$V45+1,clus2)
ARI_e1 = ARI(nids2$V45+1,clus2)
library(cluster)
si1 = silhouette(clus2,d)

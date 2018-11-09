# Need normalization before clustering

# 1. value clustering - Kmeans
library(MASS)
data(Boston)
names(Boston)

zBoston <- scale(Boston[, -14])
clusters <- kmeans(zBoston, 4)
names(clusters)

table(clusters$cluster, dnn="cluster number")
round(clusters$centers[, c(1,5,6,7,8,13)], 2)
boxplot(Boston$medv ~ clusters$cluster, main="Boston Housing",
        xlab="clusters", ylab="medv", ylim=c(0,60))
clusters$totss
clusters$tot.withinss

clusters <- kmeans(zBoston, 4, nstart = 10)
clusters$tot.withinss

# 2. Variable Clustiering 
zBoston <- scale(Boston[, -14])
t.zBoston <- t(zBoston)
v.clusters <- kmeans(t.zBoston, 4)
v.clusters$cluster

round(cor(zBoston[, v.clusters$cluster == 1]), 2)
round(cor(zBoston[, v.clusters$cluster == 2]), 2)
round(cor(zBoston[, v.clusters$cluster == 3]), 2)
round(cor(zBoston[, v.clusters$cluster == 4]), 2)

# 3. Multivariate Analysis
pca.1 <- princomp(zBoston[, v.clusters$cluster==1], cor=T)
biplot(pca.1, scale=0, cex=0.8, xlab="First Dim", ylab="Second Dim",
       xlim=c(-5, 5), ylim = c(-5, 5), main = "Boston Housing",
       col=c('gray', 'red'))
plot(pca.1$scores[,1:2], xlab="First Dim", ylab="Second Dim",
     xlim=c(-5,5), ylim=c(-5,5), main="Boston Housing",
     col=c("#FF000077", "#00FF0077", "#0000FF77", "#FF00FF77")[clusters$cluster],
     pch=c(15, 16, 22, 21)[clusters$cluster])       

library(nnet)
round(cor(zBoston %*% class.ind(v.clusters$cluster)), 2)

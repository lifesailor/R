# Decision Tree

# 1. Data
library(rpart)
library(rattle.data)
data(weather)
attach(weather)

# 2.Decision Tree
colnames(weather)
tree.1 <- rpart(RainTomorrow ~ ., data = weather[,c(3:22, 24)])
plot(tree.1, main='weather', margin=0.025)
text(tree.1, use.n = TRUE, cex=0.8, col="blue")

# train data
pred.1 <- predict(tree.1, type="prob")[,2]
cut <- sort(pred.1)[301]
pred.1a <- ifelse(pred.1 >= cut, "Yes", "No")
table(pred.1a, weather$RainTomorrow, dnn=c("predicted", "observed"))

# test data
data(weatherAUS)
weather.2 <- subset(weatherAUS, Location=="Canberra")
weather.2a <- weather.2[(366+1):(366+365),]
pred.2a <- ifelse(predict(tree.1, 
                          newdata = weather.2a, 
                          type = "prob")[,2] >= cut, "Yes", "No")
table(pred.2a, weather.2a$RainTomorrow, dnn=c("predicted", "observed"))

# 3. regression tree
library(MASS)
data(Boston)
names(Boston)

hist(Boston$medv, main="Boston Housing", xlab="Median Price")
medv.1 <- sqrt(Boston$medv)

library(rpart)
tree.1 <- rpart(medv.1 ~ . , data=Boston[,-14])
plot(tree.1, margin=0.1, main="Boston Housing")
text(tree.1, digits=2, use.n=F, cex=0.8, col="blue")

plot(medv.1 ~ predict(tree.1), xlab="fitted", ylab="sqrt medv",
     xlim=c(2,8), ylim=c(2,8), main="regression tree")
mse.1 <- mean((medv.1 - predict(tree.1))^2)
mse.0 <- mean((medv.1 - mean(medv.1))^2)
r.squared <- 1 - mse.1/mse.0
round(c(mse.0, mse.1), 3)
round(r.squared, 3)

# test
n <- nrow(Boston)
indices <- sample(1:n, n, replace = T)
train <- Boston[indices,]
test <- Boston[-indices,]
tree.2 <- rpart(medv.1[indices] ~ . , data=train[,-14])
mse.2 <- mean((medv.1[indices] - predict(tree.2, newdata=test))^2)
round(mse.2)
round(mse.2, 3)



# 4. Random Forest
library(rattle.data)
data(weather)
names(weather)

# set.seed(123)
V <- is.na(weather) 
m <- apply(V, 2, sum)
weather.1 <- weather
for (j in 1:ncol(weather)) 
  weather.1[V[,j],j] <- sample(weather[!V[,j],j],m[j],replace=TRUE)

library(randomForest)
forest.1 <- randomForest(RainTomorrow ~ ., data=weather.1[,c(3:22,24)], importance=TRUE)
cut <- sort(forest.1$votes[,2])[301]
pred.1 <- ifelse(forest.1$votes[,2] >= cut, "Yes", "No")
table(pred.1, weather$RainTomorrow, dnn=c("Predicted","Observed"))
varImpPlot(forest.1, cex=0.8, main="weather random forest")

data(weatherAUS)
weather.2 <- subset(weatherAUS, Location=="Canberra") 
weather.2a <- weather.2[(366+1):(366+365),]
# set.seed(1234)
V <- is.na(weather.2a) 
m <- apply(V, 2, sum)
weather.2aa <- weather.2a
for (j in 1:ncol(weather.2a)) weather.2aa[V[,j],j] <- sample(weather.2a[!V[,j],j],m[j],replace=TRUE)

pred.2a <- predict(forest.1, newdata=weather.2aa[,c(3:22,24)], type="vote") 
pred.2aa <- ifelse(pred.2a[,2] >= cut, "Yes", "No")
table(pred.2aa, weather.2a$RainTomorrow, dnn=c("predicted","observed"))

# 5. Boston
library(MASS)
data(Boston)
Boston$medv <- sqrt(Boston$medv)

library(randomForest)
forest.1 <- randomForest(medv ~ ., varImp=TRUE, data=Boston)
forest.1
plot(Boston$medv ~ forest.1$predicted, xlab="predicted", ylab="observed", 
     xlim=c(2,8), ylim=c(2,8), main="Boston Housing")
mean((Boston$medv - forest.1$predicted)^2)

varImpPlot(forest.1, main="Boston Housing")

n <- nrow(Boston)
indices <- sample(1:n, n, replace=T)
train <- Boston[indices,]
test <- Boston[-indices,]
forest.2 <- randomForest(medv ~ ., data=train)
mse.2 <- mean((test$medv - predict(forest.2, newdata=test))^2)
round(mse.2, 3)
# end

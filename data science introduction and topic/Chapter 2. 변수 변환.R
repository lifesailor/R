# Chapter 2. Variable Transform

library(dplyr)
library(ggplot2)
library(MASS)

# Dataset A. Mammals
data("mammals")
str(mammals)
attach(mammals)

# logtransform
opar <- par(mfrow=c(2,2))
hist(body, nclass=10, main="62 mammals")
hist(brain, nclass = 10, main="62 mammals")
hist(log(body, 2), class=10, main="62 mammals")
hist(log(brain, 2), class=10, main="62 mammals")

opar <- par(mfrow=c(1,1))
plot(brain ~ body, main="62 mammals")
plot(log(brain, 2) ~ log(body, 2), main="62 mammals")
cor(brain, body, method = "spearman")
cor(log(brain, 2), log(body, 2), method = "spearman")

# Dataset B. Insurance
data("Insurance")
str(Insurance)
head(Insurance)


attach(Insurance)
Ratio <- Claims / Holders

# sqrt transform
# 산포를 일정하게 한다.
boxplot(Ratio~Group, main="Insurance")
boxplot(sqrt(Ratio) ~ Group, main="Insurance(sqrt transformed)")
boxplot(Ratio^0.33 ~ Group, main="Insurance(cube root transformed)")

# Box-Cox Transformation
install.packages("car")
install.packages("qunatreg")
install.packages("data.table", type="binary")

library(car)
library(MASS)
data(mammals)
summary(powerTransform(mammals$body))
summary(powerTransform(mammals$brain))
summary(powerTransform(mammals))

# 양의 변수가 오른쪽으로 길게 퍼지는 분포는 제곱근, 로그 변환등을 적용하여 분포가 대칭힌 형태로 바꾼다.
# 양의 변수를 집단 간 비교하는 상황에서는 산포를 일정하게 한다.


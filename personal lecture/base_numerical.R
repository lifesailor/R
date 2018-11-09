# Data Science: Introduction and Topcis.
library(ggplot2)
library(dplyr)

# DataSet A. ChosunKings
chosun_kings <- read.table("/Users/sailyourlife/Desktop/Study/R/Data Science Introduction and Topic/chosun_kings.txt", header=TRUE)

# See Datax
str(chosun_kings)
glimpse(chosun_kings)

# Chapter 1. Numeric Data
attach(chosun_kings)

par(mfrow=c(1,1))
hist(life, xlab="life", right=FALSE, main="chosun_kings")
hist(period, xlab="period", right=FALSE, main="chosun_kings")
hist(period, xlab="period", right=FALSE, nclass=10, main="chosun_kings")

opar <- par(mfrow=c(2,2))
hist(chosun_kings$life)
boxplot(chosun_kings$life)
qqnorm(chosun_kings$life)
qqline(chosun_kings$life)

median(life)
median(period)
summary(chosun_kings)

# not robust
round(c(mean(life), sd(life)), 1)

# robust - not normal distribution
q.life <- quantile(life, prob=c(0.25, 0.5, 0.75))
round(unname(c(q.life[2], (q.life[3] - q.life[1]) / 1.35)), 1)

# 2. dplyr, ggplot
opar <- par(mfrow=c(2,2))

chosun_kings %>%
  ggplot(aes(life)) +
  geom_histogram(bins = 10) 

chosun_kings %>%
  ggplot(aes(life)) + 
  geom_freqpoly(bins = 10)

chosun_kings %>%
  ggplot(aes(life)) + 
  geom_density()



# Dataset B. Project Score
project <- read.csv("/Users/sailyourlife/Desktop/Study/R/Data Science Introduction and Topic/project_scores.csv")
str(project)
summary(project)

# boxplot
q <- quantile(project$score, prob=c(0.25, 0.5, 0.75))
lower <- q[1] - 1.5 * (q[3] - q[1])
upper <- q[3] + 1.5 * (q[3] - q[1])

project$score[project$score < lower | project$score > upper]
boxplot(project$score, horizontal = TRUE, ylim=c(0,30), main="project score")

project %>%
  ggplot(aes("", score)) + 
  geom_boxplot()


# One categorial, One Numerical
# DataSet C. Boston House Price
library(MASS)

data("Boston")
names(Boston)
attach(Boston)

par(mfrow=c(1,1))
hist(medv, main="Boston House Price")
Boston %>% ggplot(aes(medv)) +
  geom_histogram() +
  ggtitle("Boston House Price")

dis.1 <- cut(dis, 
             breaks=quantile(dis, prob=c(0, 0.5, 1.0)), 
             labels=c("inner", "outer"))

Boston <- Boston %>% mutate(dis_cut = cut(
  dis, breaks=quantile(dis, probs=c(0,0.5,1.0)), labels=c("inner","outer")
))
summary(Boston)

Boston %>% ggplot(aes(dis_cut, medv)) +
  geom_boxplot()

prob <- c(0.0, 0.25, 0.5, 0.75, 1)
lapply(tapply(medv, dis.1, quantile, prob=prob), round, 1)
boxplot(medv ~ dis.1, main="Boston House Price")

# two histograms 
library(lattice)
histogram(~ medv | dis.1, main="Boston House Price")

lstat.1 <- cut(lstat, quantile(lstat, probs = c(0.0, 0.25, 0.75, 1)), labels = c("low", "middle", "high"))
boxplot(medv ~ lstat.1, width=c(1,2,1), main="Boston House Price")
boxplot(log(medv, 2) ~ lstat.1, width=c(1,2,1), main="Boston House Price")



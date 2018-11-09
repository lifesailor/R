rm(list = ls())

# 1d logistic regression
library(rattle.data)
data(weather);
str(weather);
attach(weather)

mod.1 <- glm(RainTomorrow ~ Cloud3pm, family = "binomial")
mod.1
summary(mod.1)
head(mod.1$fitted.values)

pred.1 <- ifelse(rank(mod.1$fitted.values, ties.method = "average") <= 300, 0, 1)
pred.1a <- factor(pred.1, label=c("no rain", "rain"))
table(pred.1a, RainTomorrow)

# 2d logistic regression
mod.2 <- glm(RainTomorrow ~ Cloud3pm + Pressure3pm, family = 'binomial')
pred.2 <- ifelse(rank(mod.2$fitted.values, ties.method = "average") <= 300, 0, 1)
pred.2a <- factor(pred.2, labels=c("no rain", "rain"))
table(pred.2a, RainTomorrow)

# feature selection by AIC
mod.3 <- glm(RainTomorrow ~ Cloud3pm + Pressure3pm + Sunshine + Humidity3pm, family = "binomial")
library(MASS)
stepAIC(mod.3, direction = "both", k=log(nrow(weather)))

# gam
library(mgcv)
mod.3a <- gam(RainTomorrow ~ s(Pressure3pm, k=4) + s(Sunshine, k=4), family="binomial")
summary(mod.3a)
plot(mod.3a, pages=1)

rm(list = ls())

# 1d Regression
data(cars)
str(cars)
attach(cars)

plot(dist ~ speed, xlim=c(0,30), main="cars")
abline(lm(dist ~ speed), col='red', lty='dotted')

mod.1 <- lm(dist ~ speed)
mod.1
summary(mod.1)

# Multiple Regression
install.packages("mosaic")
install.packages("mosaicData")
library(mosaic)

data(SAT)
str(SAT)
attach(SAT)

mod.2 <-lm(sat ~ salary)
summary(mod.2)

dev.new(width=8, height=4)
par(mfrow = c(1,2))
plot(frac, salary, main="SAT")
plot(frac, sat, mani="SAT")

mod.3 <-lm(sat ~ salary + frac)
summary(mod.3)

# feature selection
library(MASS)
mod.4 <- lm(sat ~ ., data=SAT[, c(4,5,2,3,8)])
summary(mod.4)
step <- stepAIC(mod.4, direction = 'both', k = log(nrow(SAT)))
summary(step)

# 3. Smoothing - lowess
par(mfrow=c(1,1))
plot(dist ~ speed, xlim=c(0,30), main="cars")
lines(lowess(dist ~ speed), col = 'blue')

# 4. GAM
library(mgcv)
mod.5 <- gam(sat ~ s(salary, k=4) + s(frac, k=4))
plot(mod.5)
mod.6 <- gam(sat ~ s(salary, k=10) + s(frac, k=10))
plot(mod.6)



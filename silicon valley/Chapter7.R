library(ggplot2)
library(dplyr)

mpg <- tbl_df(mpg)
mpg
head(mpg)
str(mpg)
summary(mpg)

glimpse(mpg)
head(mpg)
summary(mpg)

# 1. One numerical variable
summary(mpg$hwy)
length(mpg$hwy)

# base package
opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)

# ggplot
p1 <- mpg %>%
  ggplot(aes(hwy)) + 
  geom_histogram()

p2 <- mpg %>%
  ggplot(aes(hwy)) +
  geom_freqpoly()

p3 <- mpg %>%
  ggplot(aes(hwy)) +
  geom_density()

library(ggpubr)
ggarrange(p1, p2, p3, ncol=2, nrow=2)

qqnorm(mpg$hwy)
qqline(mpg$hwy)


# ttest
install.packages('pander')
library(pander)
panderOptions('round', 5)
panderOptions('digits', 5)

hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9

t.test(hwy, mu=mu0, alternative = 'greater')
help(t.test)
t.test(hwy, mu=mu0, conf.level = 0.95)

mpg %>% ggplot(aes(manufacturer, hwy))+ geom_boxplot()

#robust
c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))



# 2. One Categorical Variable
set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels=c(0,1), labels=c("no", "yes"))
x

table(x)
xtabs(~x, x)
prop.table(table(x))
barplot(table(x))

library(ggplot2)
library(dplyr)
table(x) 

xframe <- data.frame(x)
xframe %>% ggplot(mapping = aes(x)) +
  geom_bar()

# hypothesis
binom.test(x=length(x[x=='yes']), n = length(x), p=0.5)

# increase sample number
binom.test(x=5400, n=10000)
n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96*sqrt(1/(4*n)), 4))
curve(1.96*sqrt(1/(4*x)), 10, 10000, log='x')
grid()



# 3. Explanatory Variable, Response Variable
# 3-1) real x, real y
mpg %>% ggplot(aes(cty, hwy)) + geom_jitter() + geom_smooth(method='lm')

# correlation
cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method='kendall'))
with(mpg, cor(cty, hwy, method='spearman'))

# regression
(hwy_lm <- lm(hwy ~ cty, data=mpg))
summary(hwy_lm)

predict(hwy_lm)
resid(hwy_lm)
predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)))
predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)), se.fit = TRUE)

# hypothesis testing
class(hwy_lm)
opar <- par(mfrow = c(2,2), oma = c(0,0,1.1,0))
plot(hwy_lm, las = 1)
?plot.lm

# Robust Regression
install.packages("MASS")
library(MASS)
library(dplyr)
library(ggplot2)
glimpse(stack.loss)
?stack.loss
glimpse(stackloss)

# y
lqs(stack.loss ~ ., data=stackloss)
lm(stack.loss ~ ., data=stackloss)

# LOESS
# Locaaly weighted scatterplot smoothing

plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~ displ, data=mpg)
mpg_lo
summary(mpg_lo)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth()


# 4. x: categorical, real: y
mpg %>% ggplot(aes(class, hwy)) +
  geom_boxplot()

(hwy_lm2 <- lm(hwy ~ class, data=mpg))
summary(hwy_lm2)
opar <- par(mfrow=c(2,2), oma=c(0,0, 1.1,0))
plot(hwy_lm2, las=1)
par(opar)


# 5. numeral x, categorical y
chall <- read.csv(file = '../Desktop/jungyoon/R_Practice/R_Silicon_Valley/challenger.csv')
chall <- tbl_df(chall)
glimpse(chall)

chall %>% ggplot(aes(temperature, distress_ct)) +
  geom_point()
chall %>% ggplot(aes(factor(distress_ct), temperature)) + 
  geom_boxplot()

chall_glm <- 
  glm(cbind(distress_ct, o_ring_ct - distress_ct) ~ temperature,
      data=chall, family = 'binomial')
summary(chall_glm)

predict(chall_glm, data.frame(temperature=30))
predict(chall_glm, data.frame(temperature=30), type='response')

logistic <- function(x){ exp(x) / (exp(x) + 1)}
plot(c(20, 85), c(0, 1), type="n", xlab="temperature")
plot(c(20, 85), c(0, 1), type="n", xlab="temperature", ylab="prob")
tp <- seq(20, 85, 1)
chall_glm_pred <- 
  predict(chall_glm, data.frame(temperature=tp),
          se.fit = TRUE)
lines(tp, logistic(chall_glm_pred$fit))
lines(tp, logistic(chall_glm_pred$fit - 1.96 * chall_glm_pred$fit), lty=2)
lines(tp, logistic(chall_glm_pred$fit + 1.96 * chall_glm_pred$fit), lty=2)
abline(v=30, lty=2, col='blue')


# Chapter 3. Categorical Data

# Dataset A. Titanic
data(Titanic)
str(Titanic)

N <- sum(Titanic); N
round(apply(Titanic, 4, sum)/N, 3)

Tab.1 <- apply(Titanic, c(1,4), sum)
Tab.1m <- addmargins(Tab.1) # sum
Tab.1m
round(Tab.1m[,1:3]/Tab.1m[,3], 3)

# 2차원 데이터
mosaicplot(~ Class + Survived, col=c("gray","red"), off=0, data=Titanic)
mosaicplot(~ Sex + Survived, col=c("gray", "red"), off=0, data=Titanic)
mosaicplot(~ Class + Sex + Survived, col=c("gray", "red"), off=c(10,0,0),
           dir=c("v","v","h"), data=Titanic)           


# DataSet B. UCBAdmission
data("UCBAdmissions")
str(UCBAdmissions)
mosaicplot(~ Gender + Admit, off=0, col=c("red", "gray"), data=UCBAdmissions)
mosaicplot(~ Dept + Gender + Admit, off=0, col=c("red", "gray"), 
           dir=c("v","v","h"), data=UCBAdmissions)

ucb <- UCBAdmissions
ucb <- as.data.frame(ucb)
str(ucb)

ucb %>% 
  ggplot(aes())+
  geom_tile(aes(Dept, Gender, fill=Freq))

# DataSet C. Weather
install.packages("rattle.data")
library(rattle.data)
data(weather)
attach(weather)
library(lattice)

histogram(~ Humidity3pm | RainTomorrow)

# cut and categorize humidity
humidity.c <- cut(Humidity3pm, 
                  quantile(Humidity3pm,
                           prob = c(0.0, 0.25, 0.5, 0.75, 1.0),
                           include.loweset=TRUE)) 
mosaicplot(~ humidity.c + RainTomorrow, off = 0, col = c('gray', 'blue'), main='weather')  

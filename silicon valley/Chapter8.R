# Chapter 8 
# binomial deviacne - 이항편차 
binomial_deviance <- function(y_obs, yhat){
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1 - epsilon, 1 - epsilon, yhat)
  
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs / yhat))
  b = ifelse(y_obs == 1, 0, (1 - y_obs) * log((1 - y_obs) / (1 - yhat)))
  return(2*sum(a+b))
}

# R로 분석할 수 있는 데이터 사이즈 크기: 어림잡아 n * p <= 1billion
# 이보다 크면 python 사용을 고려하고 batch 처리를 고려한다.
# 8.3 분류분석 예제
install.packages(c('dplyr',
                   'ggplot2',
                   'ISLR',
                   'MASS',
                   'glmnet',
                   'randomForest',
                   'gbm',
                   'rpart',
                   'boot'))
library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)

adult <- read.csv("../Desktop/jungyoon/R_Practice/R_Silicon_Valley/adult.data.txt",
                  header = F,
                  strip.white = T)
names(adult) <- c('age',
                  'workclass',
                  'fnlwgt',
                  'education',
                  'education_num',
                  'matrital_status',
                  'occupation',
                  'relationship',
                  'race',
                  'sex',
                  'capital_gain',
                  'capital_loss',
                  'hours_per_week',
                  'native_country',
                  'wage')
glimpse(adult)
summary(adult)
levels(adult$wage)

levels(adult$race)
adult$race[1:5]
levels(adult$sex)
adult$sex[1:5]

x <- model.matrix(~ race + sex + age, adult)
x_orig <- adult %>% 
  dplyr::select(sex, race, age)
x_mod <- model.matrix(~ sex + race + age, adult)

x <- model.matrix(~ . - wage, adult)
dim(x)

# 8.4 훈련, 검증, 테스트세트의 구분
set.seed(1601)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n * .6)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)

length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx,]
validation <- adult[validate_idx,]
test <- adult[test_idx,]

# 8.5 시각화
training %>%
  ggplot(aes(age, fill=wage)) + 
  geom_density(alpha=0.5)

# 로지스틱과 LASSO의 한계: 선형성을 가정한다.
training %>%
  filter(race %in% c('Black', 'White')) %>%
  ggplot(aes(age, fill = wage)) +
  geom_density(alpha = 0.5) +
  ylim(0, 0.1) + 
  facet_grid(race ~ sex, scales = 'free_y')

# 해결방법
# 1) 랜덤포레스트나 gbm 같은 비모수 방법을 사용
# 2) 로지스틱이나 라쏘같은 선형모형을 사용하되 범주화된 설명 변수를 사용한다.
training %>%
  ggplot(aes(education_num, fill=wage)) + 
  geom_bar()

ad_glm_full <- glm(wage ~ ., data=training, family = binomial)
ad_glm_full
summary(ad_glm_full)

alias(ad_glm_full)

# 예측
predict(ad_glm_full, newdata = adult[1:5,], type = 'response')
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full,  newdata = validation, type='response')
library(gridExtra)

p1 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(y_obs, yhat_lm, group=y_obs,
                 fill=factor(y_obs))) +
      geom_boxplot()


p2 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=0.5)

grid.arrange(p1, p2, ncol=2)
binomial_deviance(y_obs, yhat_lm)

install.packages("ROCR")

library(ROCR)
par(mfrow = c(1,1))
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = 'tpr', x.measure = 'fpr')
plot(perf_lm, col='black', main='ROC Curve for GLM')
abline(0, 1)
performance(pred_lm, "auc")@y.values[[1]]

rm(list = ls())

# The Goal: To make people understand who don't know statistics.
# Use color and size well

# 1. One Variable 
#   1) Categorical: Bar plot
#   2) Numerical: Histogram

# 2. Two Variable
#   1) One Categorical, One Numerical - Boxplot
#   2) Two Numerical - Scatterplot
#   3) Two Categorical - Mosaicplot, Heatmap, Bubbleplot

library(dplyr)
df <- read.csv('~/Desktop/Github/Data-Science-and-Programming/Data Analysis/Data Visualization/HR_comma_sep.csv')
glimpse(df)

df$left <- as.factor(df$left)
df$promotion_last_5years <- as.factor(df$promotion_last_5years)
df$Work_accident <- as.factor(df$Work_accident)

colnames(df)[7] <- 'target'
colnames(df)[9] <- 'department'
df <- tbl_df(df)
glimpse(df)


# 1-1) One Categorical - Bar plot
df %>% ggplot(aes(department, fill=as.factor(target))) +
  geom_bar(position = 'dodge')

# 1-2) One Numerical - Histogram
df %>%
  ggplot(aes(satisfaction_level)) +
  geom_histogram(col='red') # border

df %>% 
  ggplot(aes(satisfaction_level, fill=as.factor(target))) +
  geom_histogram()

df %>% 
  ggplot(aes(satisfaction_level, fill=as.factor(target))) +
  geom_histogram(position = 'dodge')

df %>%
  ggplot(aes(satisfaction_level, fill=as.factor(target))) +
  geom_density(alpha=0.5) + 
  labs(fill = "TARGET") +
  ggtitle("Histogram of satisfaction level") + 
  xlab("satisfaction level") + 
  ylab("pdf") +
  theme_bw()

# 2-1) One Categorical, One Numerical - Boxplot, Violin plot
df %>% ggplot(aes(department, satisfaction_level, color=salary, fill=salary)) +
  geom_boxplot(alpha=0.5)

df %>% ggplot(aes(department, satisfaction_level, color=salary, fill=salary)) +
  geom_violin(alpha=0.5)

# 2-2) Two Numerical - scatter
df %>% 
  sample_n(2000) %>%
  ggplot(aes(satisfaction_level, average_montly_hours, col=target)) +
  geom_point(aes(size=time_spend_company)) 

df %>% 
  sample_n(2000) %>%
  ggplot(aes(satisfaction_level, average_montly_hours, col=target)) +
  geom_point(aes(size=time_spend_company)) +
  geom_smooth(method='loess')

df %>%
  sample_n(2000) %>%
  ggplot(aes(satisfaction_level, average_montly_hours, col=target)) +
  geom_point()

# 2-3) two categorical - bubble plot, heatmap
library(plyr)
df_1 <- ddply(df, 
              .(salary, department), summarise,
              mean = mean(satisfaction_level),
              max = max(satisfaction_level))

df_1 %>% ggplot(aes(department, salary)) + 
  geom_point(aes(size=max), shape = 1) +
  geom_point(aes(size=mean))

df_1 %>% 
  ggplot(aes()) +
  geom_tile(aes(x=salary, y=department, fill=mean))

df_1 %>%
  ggplot(aes()) + 
  geom_bar(aes(salary, mean), stat='identity', fill='black')



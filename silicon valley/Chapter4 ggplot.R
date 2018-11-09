install.packages("gapminder")

library(gapminder)
library(dplyr)
library(sqldf)

# Data Exploration
head(gapminder)
tail(gapminder)
glimpse(gapminder)
sqldf('select count(*) from gapminder')
gapminder %>%
  select(gdpPercap, lifeExp)
summary(gapminder)
cor(gapminder$lifeExp, gapminder$gdpPercap)

# R Basic plot
oper = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap)
hist(log10(gapminder$gdpPercap), nclass=50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(oper)

# if i apply log, then correlation increases.
cor(gapminder$lifeExp, log10(gapminder$gdpPercap))

# R ggplot
install.packages("ggplot2")
library(ggplot2)
install.packages('ggpubr')
library(ggpubr)

oper <- par(mfrow=c(2,2))
g1 <- gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()
g2 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
g3 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
g4 <- gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() + scale_x_log10() + geom_smooth()

ggarrange(g1, g2, g3, g4, ncol=2, nrow=2)

df <- data.frame(
  gp = factor(rep(letters[1:3], each=10)),
  y = rnorm(30)
)

glimpse(df)

## ggplot example
ds <- df %>% group_by(gp) %>% summarize(mean=mean(y), sd=sd(y))
ds

ggplot() + 
  geom_point(data=df, aes(x=gp, y=y)) +
  geom_point(data=ds, aes(x=gp, y=mean), 
             colour='red', 
             size=3) +
  geom_errorbar(data=ds, aes(x=gp, y=mean, ymin=mean-sd, ymax=mean+sd),
                          colour='red',
                          width=0.4)
             

glimpse(diamonds)

# One Continuous Variable
p1 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
p2 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
p3 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly() + scale_x_log10()
p4 <- gapminder %>% ggplot(aes(x=gdpPercap)) + geom_density() + scale_x_log10()

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2)

# One Categorical Variable
diamonds %>% ggplot(aes(cut)) + geom_bar()
table(diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut))*100, 1)

diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round(n/sum(n) *100, 1))

# Two Continous Variable
diamonds %>%
  ggplot(aes(carat, price)) + geom_point()
diamonds %>%
  ggplot(aes(carat, price)) + geom_point(alpha=0.1)
mpg %>% 
  ggplot(aes(cyl, hwy)) + geom_jitter()
mpg %>%
  ggplot(aes(cyl, hwy)) + geom_point()

pairs(diamonds %>% sample_n(1000))

# One Continous One Categorical
mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
glimpse(mpg)
unique(mpg$class)

mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=0.5)
mpg %>% mutate(class=reorder(class, hwy, median)) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=0.5)
mpg %>% mutate(class =factor(class, levels=
                 c("2seater", "subcompact", "compact", "midsize", "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot()
mpg %>% mutate(class=factor(class, levels=c("2seater", "subcompact", "compact", "midsize", 
                                            "minivan", "suv", "pickup"))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5) + coord_flip()

# Two Categorical varibles
glimpse(data.frame(Titanic))
Titanic
xtabs(Freq ~ Class + Sex +Age + Survived, data.frame(Titanic))

?Titanic
Titanic
p <- par(mfrow=c(1,1))
mosaicplot(Titanic, main='Survival on the Titanic', color=TRUE)
apply(Titanic, c(3, 4), sum)
round(prop.table(apply(Titanic, c(3,4), sum), margin = 1), 3)

t2 = data.frame(Titanic)
t2 %>% group_by(Sex) %>%
  summarize(n = sum(Freq),
            survivors=sum(ifelse(Survived=='Yes', Freq, 0))) %>%
  mutate(rate_survival = survivors/n)

# Additional Tip 1
gapminder %>% filter(year == 2007) %>%
  ggplot(aes(gdpPercap, lifeExp)) + 
  geom_point(aes(size=pop, col=continent)) + scale_x_log10() + ggtitle("Gapminder data for 2007")

# Additional Tip 2
gapminder %>% 
  ggplot(aes(year, lifeExp, group=country)) + 
  geom_line()

gapminder %>%
  ggplot(aes(year, lifeExp, group=country, col=continent)) +
  geom_line()

gapminder %>%
  ggplot(aes(year, lifeExp, group=country)) +
  geom_line() + 
  facet_wrap(~ continent)

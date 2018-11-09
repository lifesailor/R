#1. read.csv
install.packages("dplyr")
boston <- read.table("C:/Users/sailyourlife/Desktop/jungyoon/R_Practice/R_Silicon_Valley/housing.data.txt")
dplyr::glimpse(boston)

library(dplyr)
glimpse(boston)

names(boston) <- c('crim',
                   'zn',
                   'indus',
                   'chas',
                   'nox',
                   'rm',
                   'age',
                   'dis',
                   'rad',
                   'tax',
                   'ptratio',
                   'black',
                   'lstat',
                   'medv'
)

glimpse(boston)
class(boston)
plot(boston)
summary(boston)



#2. read excel
install.packages("readxl")
library(readxl)



#3. sql
install.packages("sqldf")
library(sqldf)

sqldf("select * from iris")
sqldf("select count(*) from iris")
sqldf("select Species, count(*), avg('Sepal Length') from iris group by 'Species' ")
sqldf("select Species, 'Sepal.Length', 'Sepal.Width'
       from iris
       where 'Sepal.Length' < 4.5
       order by 'Sepal.Width'")
head(sqldf("select * from iris"))

library(dplyr)
df1 <- data_frame(x = c(1,2), y=2:1)
df2 <- data_frame(x = c(1,3), a=10, b="a")

sqldf("select * from df1 inner join df2
      on df1.x = df2.x")

sqldf("select * from df1 left join df2 on df1.x = df2.x")



#4. R base package
install.packages("gapminder")
library(gapminder)

# choose row and column
gapminder[gapminder$country=='Korea, Rep.', c('pop', 'gdpPercap')]

# choose row
gapminder[gapminder$country == 'Korea, Rep.',]
gapminder[gapminder$year == 2007,]
gapminder[gapminder$country == 'Korea, Rep.' & gapminder$year==2007,]
gapminder[1:10,]
head(gapminder, 10)

# sort row
gapminder[order(gapminder$year, gapminder$country),]

# choose column
gapminder[, c('pop', 'gdpPercap')]
gapminder[, 1:3]

# change column name
f2 = gapminder
names(f2)
names(f2)[6] = 'gdp_per_cap'

gapminder

# new column
f2 = gapminder
f2$total_gdp = f2$pop * f2$gdpPercap
f2

# statistics
median(gapminder$gdpPercap)
apply(gapminder[, 4:6], 2, mean)
summary(gapminder)


#5. dplyr 
i2 <- tbl_df(iris)
i2

glimpse(i2)

iris %>% head(10)

#filter
filter(gapminder, country=='Korea, Rep.')
filter(gapminder, year==2007)
filter(gapminder, country=='Korea, Rep.' & year==2007)

gapminder %>% filter(country=='Korea, Rep.')
gapminder %>% filter(year == 2007)
gapminder %>% filter(country=='Korea, Rep.' & year==2007)

#arange
arrange(gapminder, year, country)
gapminder %>% arrange(year, country)

#select
select(gapminder, pop, gdpPercap)
gapminder %>% select(pop, gdpPercap)

#mutate
gapminder %>%
  mutate(total_gdp = pop * gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap,
         lgrk = le_gdp_ratio * 100)

#summarize
gapminder %>% 
  summarize(n_obs = n(),
            n_countries = n_distinct(country),
            n_years = n_distinct(year),
            med_gdpc = median(gdpPercap),
            max_gdppc = max(gdpPercap))
#sample
sample_n(gapminder, 10)
gapminder %>% sample_n(10)

#distinct
gapminder %>% select(country) %>% distinct()

#groupby
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent)%>%
  summarize(median((lifeExp)))

#chaining
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(lifeExp = median(lifeExp)) %>%
  arrange(-lifeExp)

#join
df1 %>% inner_join(df2)
df1 %>% left_join(df2, by='a')
df1 %>% right_join(df2)
df1 %>% full_join(df2)

df1 %>% union_all(df2)

sqldf('select * from df1 union select * from df2')

#exercise
gapminder %>% 
  group_by(country) %>%
  summarize(mean)

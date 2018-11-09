# statistical inference
install.packages("BHH2")
library(BHH2)
data(shoes.data)
shoes.data

# Dataset: shoes.data
attach(shoes.data)
diff <- matB - matA
dotplot(diff)
mean(diff)

# Frequentist Interval
t.test(diff, alternative = 'greater')

# Bayes Interval
qbeta(0.025, 6+1, 4+1)
qbeta(0.975, 6+1, 4+1)

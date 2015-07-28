setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
d <- read.csv('data/cleanData.csv')  # Read the data in

library(mclust)

#f1 questions (a-g)
df <- data.frame(d$f1a, d$f1b, d$f1c, d$f1d, d$f1e, d$f1f, d$f1g)
df <- na.omit(df)
m <- Mclust(df)
print(m)  #finds six clusters
summary(m)  


#def space behavior
curr <- data.frame(d$f2a1, d$f2b1, d$f2c1, d$f2d1, d$f2e1, d$f2f1, d$f2g1)
curr <- na.omit(curr)
mCurr <- Mclust(curr)
print(mCurr)  #finds 9 clusters
summary(mCurr)
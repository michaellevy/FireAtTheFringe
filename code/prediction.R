setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
source("code/dataCleaning.R")
d = read.csv('data/cleanData.csv')  # Read the data in

#compute current DS behavior
dsBehavior = names(d)[grepl("f2.1", names(d))]
#d$sumDSBehavior = apply(d[dsBehavior], 1, 
#                        function(x) sum(x, na.rm = TRUE))

# Create a new dataframe removed a few variables that can't be used for prediction

# F2xx questions need to be removed (1's are the dependent variable, 
# 2's (future behavior) are too close, I think)
d2 = d[, !grepl("^f2.2", names(d))]

# a5xx are involvement of home modification/construction.
# These could be informative, but missing lots of entries, so for now removing
d2 = d2[, !grepl("^a5.", names(d2))]

# Some misc weird variables
d2 = subset(d2, select = -(c(address, state, id, b1, i2b, i2c)))

# Some sub-question in income and ethinicity that were rarely answered
d2 = subset(d2, select = -c(j5c, j6c, j5a, j6a))

# Where did you live before moving here has too many levels
d2 = d2[, !grepl("b3.", names(d2))]

length(unique(substr(d2$zip, 1, 5)))  # 11 unique zips, 10 cities, so these are likely too colinear
d2 = subset(d2, select = -zip)

# Merge a8 (lot size) answer in sqft and acres and remove the other
d2$a8acres = ifelse(is.na(d2$a8acres) & !is.na(d2$a8sqfeet),
                    d2$a8sqfeet / 43560,
                    d2$a8acres)
d2 = subset(d2, select = -a8sqfeet)

sum(complete.cases(d2)) / nrow(d2)
# Still have missing data on the majority of rows.

# 2015-07-27: Missingingness on 71% of rows, but much of it in Qs we don't want
# to use anyway, so go ahead and clear those out:
rid = grep("a5|^b([4-9]|10|11)|^(c|d|e|f)1|^f(3|4|5)|^g|^i|^j(4|5|7|8)", names(d2))
d2 = d2[, -rid]

plot(d2$age, d2$j2)  # same so remove j2
d2 = d2[, -which(names(d2) == "j2")]

# Maybe delete two rows with missing response variables:
# d2 = d2[apply(d2, 1, function(x) sum(is.na(x[grep("^f", names(x))]))) == 0, ]

# Impute. Will want to do something more principled eventually, for now just use KNN
library(DMwR)
dImp = knnImputation(d2, k = 10, scale = TRUE)

# 2015-07-27: Predict each seperately
#####################################
library(randomForest)
set.seed(7890)
dvs = names(dImp)[grep("^f2", names(dImp))]
iv = names(dImp)[!1:ncol(dImp) %in% dvs]
tmpRF = lapply(dvs, function(y)
  randomForest(y ~ ., data = dImp, type = classification))
rfs = lapply(dvs, function(y) randomForest(y ~ iv, data = dImp))
lapply(rfs, summary)

# Let's try some prediction!
# Using random forests
library(randomForest)
set.seed(123)
# Reserve 1/3 of the data for testing
trainingRows = sample(1:nrow(dImp), nrow(dImp) * .67) 
rf1 = randomForest(sumDSBehavior ~ ., data = dImp, subset = trainingRows,
                   importance = TRUE)
rf1Yhat = predict(rf1, newdata = dImp[-trainingRows, ])
mean((rf1Yhat - dImp[-trainingRows, "sumDSBehavior"])^2)  # Test MSE
varImpPlot(rf1, n.var = 10)  # What are the key var's in the dataset for predicting DS Behav?

# Using boosted trees
library(gbm)
set.seed(90210)
D = 2
lam = .1
boost1 = gbm(sumDSBehavior ~ ., data = dImp[trainingRows, ],
             distribution = "gaussian", n.trees = 5e3,
             interaction.depth = D, shrinkage = lam)
head(summary(boost1))  # Note the plot probably won't display all the variable names
plot(boost1, i = "city")
plot(boost1, i = "f3e1")  # f3b1 U-shaped too. Weird.
boost1Yhat = predict(boost1, newdata = dImp[-trainingRows, ], n.trees = 5e3)
mean((boost1Yhat - dImp[-trainingRows, "sumDSBehavior"])^2)  # Worse than RF

# Let's try tuning the tuning parameters
boostedModels = list()
boostedModels = 
  lapply(1:4, function(D) {
    lapply(10^(-1:-4), function(lam) {
      structure(
        gbm(sumDSBehavior ~ ., data = dImp[trainingRows, ],
            distribution = "gaussian", n.trees = 5e3,
            interaction.depth = D, shrinkage = lam),
        "param" = paste0("D=", D, " lambda=", lam))
    })
  })
boostedModels = unlist(boostedModels, recursive = FALSE)

# Calculate MSE for each model on test data
boostedMSEs = 
  sapply(boostedModels, function(x) {
    mod = attr(x, "param")
    yhat = predict(x, newdata = dImp[-trainingRows, ], n.trees = 5e3)
    MSE = mean((yhat - dImp[-trainingRows, "sumDSBehavior"])^2)
    list(mod, MSE)
  }) 

bestModIndex = which.min(boostedMSEs[2, ])
boostedMSEs[, bestModIndex]  # Beats RF by .35
bestBoosted = boostedModels[[bestModIndex]] # d=3, lam=.001
plot(bestBoosted)
head(summary(bestBoosted))
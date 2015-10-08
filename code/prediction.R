setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
# or
setwd(file.path('~', 'Dropbox', 'FireAtTheFringe'))  # Set working directory

if(FALSE) { # This only needs to be read once. Just run the following read.csv
            # line once you have the cleaned and merged data.
    source("code/dataCleaning.R")
    d = read.csv('data/cleanData.csv')  # Read the data in
    source("code/addNeighborsDSBehavior.R")
    dists = read.csv("data/withDist.csv")
    dists = subset(dists, select = c("ID", "near_cnf", "near_03f"))
    d = merge(d, dists, by.x = "id", by.y = "ID")
    write.csv(d, "data/clean_with_neighborsDS_and_proximity.csv")
}
d = read.csv("data/clean_with_neighborsDS_and_proximity.csv")

#compute current DS behavior
# dsBehavior = names(d)[grepl("f2.1", names(d))]
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
rid = grep("a5|^a7|^b([4-9]|10|11)|^(c|d|e|f)1|^f(3|4|5)|^g|^i|^j(4|5|7|8)", names(d2))
d2 = d2[, -rid]

d2 = d2[, -which(names(d2) == "j2")] # same so remove j2

# Maybe delete two rows with missing response variables:
# d2 = d2[apply(d2, 1, function(x) sum(is.na(x[grep("^f", names(x))]))) == 0, ]

# Impute. Will want to do something more principled eventually, for now just use KNN
library(DMwR)
dImp = knnImputation(d2, k = 10, scale = TRUE)

dvs = grep("^f2", names(dImp))
dv = dImp[, dvs]
dv = as.data.frame(apply(dv, 1:2, function(x) ifelse(x >= .5, 1, 0)))  #Imputation puts some inbetween
iv = dImp[, -dvs]

# Predict each DS behavior each seperately
library(ggplot2)
library(reshape2)
dvLong = melt(dv)
ggplot(dvLong, aes(x = variable, fill = as.factor(value))) +
  geom_histogram(stat = "bin") + theme_minimal()

library(randomForest)
set.seed(7890)
RFs = lapply(seq_along(dv), function(y) {
    df = cbind(y = as.factor(dv[[y]]), iv)
    train = sample(1:nrow(df), nrow(df) * .67) 
    rf = randomForest(y ~ ., df, subset = train, 
                      type = "classification", importance = TRUE,
                      ntree = 1e3)
    yhat = predict(rf, newdata = df[-train, ])
    confMat = table(as.numeric(as.character(yhat)), dv[[y]][-train])
    varimp = varImp(rf, n.var = 10)  # What are the key var's in the dataset for predicting DS Behav?
    list(mod = rf, cm = confMat, imp = varimp)
})
pdf("results/RFVarImpPlots.pdf", height = 8, width = 14)
par(mfcol = c(2, 3))
plots = lapply(seq_along(RFs), function(x) varImpPlot(RFs[[x]]$mod, main = names(dv)[x]))
print(plots)
dev.off()

lapply(RFs, function(x) {
  x = x$cm
  if(nrow(x) == 1)
    x = rbind("0" = c(0, 0), x)
  x
})



library(gbm)
set.seed(90210)
D = 3
lam = .001
train = sample(nrow(dImp), nrow(dImp) * .67)
df = cbind(y = dv$f2a1, iv)
gbmGutters = 
  gbm(y ~ ., data = df, distribution = "bernoulli", n.trees = 5e3,
             interaction.depth = D, shrinkage = lam)


gbms = 
  lapply(seq_along(dv[,5:7]), function(i) {
    df = cbind(y = dv[, i], iv)
    train = sample(nrow(dImp), nrow(dImp) * .67)
    mod = gbm(y ~ ., data = df, distribution="bernoulli", n.trees = 5e3,
              interaction.depth = D, shrinkage = lam)
    pred = predict(mod, newdata = df[-train, ], n.trees = 5e3, type = "response")
    cm = confusionMatrix(pred, df$y[-train])
    list(model = mod, predictions = pred, confMat = cm)
  })



sapply(gbms, function(x) x$predictions)
preds = 
  lapply(gbms, predict, newdata = d)

head(summary(gbmGutters))  # Note the plot probably won't display all the variable names
plot(gbmGutters, i = "city")
plot(boost1, i = "f3e1")  # f3b1 U-shaped too. Weird.
boost1Yhat = predict(boost1, newdata = dImp[-trainingRows, ], n.trees = 5e3)




# Prediction on sum of defensible space behaviors:
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
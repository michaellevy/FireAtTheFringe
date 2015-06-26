setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
d = read.csv('data/cleanData.csv')  # Read the data in
library(ggplot2)
library(reshape2)
library(dplyr)

# Names of defensible space behavior questions
dsBehavior = names(d)[grepl("f2.1", names(d))]
melt(d[dsBehavior]) %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_violin()
# Most people their gutters; other DS behaviors are rarer
d$sumDSBehavior = apply(d[dsBehavior], 1, 
                           function(x) sum(x, na.rm = TRUE))
ggplot(d, aes(x = sumDSBehavior)) + geom_histogram()

dsEffectiveness = names(d)[grepl("^f1", names(d))]
melt(d[dsEffectiveness]) %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_violin()
# There's quite a bit of variance in how effective different DS behaviors are.
d$medianDSEffectiveness = apply(d[dsEffectiveness], 1, 
                           function(x) median(x, na.rm = TRUE))

mosaicplot(
  table(
    data.frame(
      medianEffectiveness = floor(d$medianDSEffectiveness), 
      sumDSBehavior = d$sumDSBehavior)),
  main = "Most people perceive defensible space behavior to be quite effective;
  even among those who think it's extremely effective, many haven't implemented much."
)

# Breakdown actual behavior vs. perceived effectiveness by individual actions
t1 = melt(d[dsBehavior], value.name = "behavior")
t2 = melt(d[dsEffectiveness], value.name = "effectiveness")
t = data.frame(question = substr(t1$variable, 3, 3),
               behavior = t1$behavior,
               effectiveness = t2$effectiveness)
levels(t$question) = 
  c("clean roof & gutters", "stack wood > 30' from house", 
    "non-flammable building materials", "fire resistant plants",
    "space plants > 15' apart", "prune low branches", "reduce tree density")
t$own = c("rent", "own")[d$a5]

likert5 =  c("Not all", "Slightly", "Neutral", "Quite", "Extremely")

t[complete.cases(t), ] %>%
  ggplot(aes(x = effectiveness, y = behavior, color = own)) +
    geom_point(position = "jitter", alpha = 1) + 
    facet_wrap(~question, ncol = 2) + 
    geom_smooth(method = "lm") +
    scale_y_continuous(breaks = 0:1, labels = c("no", "yes")) +
    scale_x_continuous(breaks = 1:5, labels = likert5) +
    theme_bw() +
    theme(axis.text.x=element_text(angle = -45, hjust = 0))
# Some ds behaviors renters can't implement. We may want to remove them
# from some analyses. They only make up 5% of the dataset:
table(t$own)

# d[d$a5 == 2, ] %>% # Run this line instead of the next to plot only homeowners
d %>%
ggplot(aes(x = f5b, y = sumDSBehavior)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = 1:5, labels = likert5) +
  xlab("To what extent do you consider your own home\nto be at risk from wildland fire?") +
  ylab("Defensible space implementation") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))

# Greater perceived risk is not at all or slightly negatively correlated
# with defensible space behavior.
# Maybe because DS-adopters feel like they've ameliorated the risk.
# Check that by replacing their personal risk with community risk...

ggplot(d, aes(x = f5a, y = sumDSBehavior)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = 1:5, labels = likert5) +
  xlab("To what extent do you consider the community in which you live
       to be at risk from wildland fire?") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
# Same pattern! Maybe they feel their whole community is protected. Or maybe 
# people are foolish.

# USFS is like me
fsAlike = names(d)[grepl("^g1", names(d))]
d$medianfsAlike = apply(d[fsAlike], 1, 
                                function(x) median(x, na.rm = TRUE))
ggplot(d, aes(x = medianfsAlike, y = sumDSBehavior)) +
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm')
# No obvious relationship between relating to USFS and adopting DS behavior

townTable = table(d$b3atwnct)
mainTowns = names(townTable)[townTable > 5]
# Assign "other" to towns for which we have 5 or fewer responses
d$town = as.character(rep(d$b3atwnct))
d$town[!(d$town %in% mainTowns)] = "other"

ggplot(d, aes(x = town, y = sumDSBehavior)) +
  geom_violin()
# Things are different in Alpine!


# Note some e1-Qs  (ds beliefs) are reverse coded

# Create a new dataframe having removed a few variables that can't be used for prediction
# What variables have a lot of missingness?
sort(sapply(d, function(x) sum(is.na(x))))

# F2xx questions need to be removed (1's are the response, 2's are too close, I think)
d2 = d[, !grepl("^f2", names(d))]

# a5xx are involvement of home modification/construction.
# These could be informative, but missing lots of entries, so ofr now removing
d2 = d2[, !grepl("^a5.", names(d2))]

# Some misc weird variables
d2 = subset(d2, select = -(c(X, address, state, id, b1, i2b, i2c)))
str(d2, list.len = 999)  # Not sure what the next-to-last ~10 columns are. 
# Look like derived variables. Perhaps they should be deleted???

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

# Still have missing data on the majority of rows.
sum(complete.cases(d2)) / nrow(d2)

# So, let's impute. Will want to do something more principled eventually, for now jsut mean impute
library(DMwR)
dImp = knnImputation(d2, k = 3, scale = TRUE)

# Let's try some prediction
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
head(summary(boost1))
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

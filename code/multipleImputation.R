setwd(file.path('~', 'Dropbox', 'FireAtTheFringe'))  # Set working directory
d = read.csv("data/clean_with_neighborsDS_and_proximity.csv")
library(dplyr)
library(Amelia)

# Merge acres and sqft:
d$a8acres = ifelse(is.na(d$a8acres) & !is.na(d$a8sqfeet),
                   d$a8sqfeet / 43560,
                   d$a8acres)
# Get rid of sqft:
d = subset(d, select = -a8sqfeet)

# Remove neighbor behavior question:
d = d[, -grep('neighbors', names(d))]

dd = d[, grep('id|city|^c1|^e|^f1|f2[a-g]1|^f5[a-d]|^d1[a-g]|h1|near_03f', names(d))]

mi = amelia(dd, m = 10, p2s = 1, idvars = 'id', noms = 'city', parallel = 'multicore', ncpus = 3)
corrplot(cor(mi$imputations[[1]][, -(1:2)]), tl.cex = .5)

noCities = lapply(mi$imputations, function(x) x[, -2])
averaged = Reduce('+', noCities) / length(mi$imputations)
averaged$city = dd$city
averaged = mutate(averaged, numBehaviors = rowSums(averaged[, grep('f2[d-g]1', names(averaged))]))
cityRates = 
    group_by(averaged, city) %>%
    summarise(avgDS = mean(numBehaviors))
averaged$cityRate = cityRates$avgDS[match(averaged$city, cityRates$city)]

### Output for SEM:
write.csv(averaged, 'data/multipleImputed.csv', row.names = FALSE)

str(averaged)
### Non-SEM modeling:
cats = 
    lapply(mi$imputations, function(d) {
        # Reverse the reverse-coded questions: e1c & e1f
        # corrplot(cor(d[, grep('^e1', names(d))]))
        data.frame(
            policyBeliefs = d$e1a + d$e1g - d$e1c - d$e1f,
            effectiveness = apply(d[, grep('f1[d-g]', names(d))], 1, sum),
            experience = d$h1b + d$h1c,
            risk = apply(d[, grep('f5[a-c]', names(d))], 1, sum),
            near_03f = d$near_03f,
            dsBehaviors = apply(d[, grep('f2[d-g]1', names(d))], 1, sum)
        )
    })

# library(Zelig)
# z <- zls$new()
# z$zelig(dsBehaviors ~ ., data = cats)

# Average across the imputed datasets
# This may fuck up uncertainty.
imp = Reduce("+", cats) / length(cats)

imp$city = d$city
cityRates = 
    group_by(imp, city) %>%
    summarise(avgDS = mean(dsBehaviors))
imp$cityRate = cityRates$avgDS[match(imp$city, cityRates$city)]

for(i in c(1:5, 8))
    imp[[i]] = scale(imp[[i]])

corrplot(cor(imp[, -7]))

models = list(ExpSurvey = lm(dsBehaviors ~ policyBeliefs + effectiveness + experience + risk + cityRate, data = imp),
              ExpDirect = lm(dsBehaviors ~ policyBeliefs + effectiveness + near_03f + risk + cityRate, data = imp))
lapply(models, summary)
lapply(models, AIC)
m = models$ExpDirect
summary(m)
library(mvtnorm)
params = rmvnorm(1e3, sigma = vcov(m))
library(rethinking)
preds = link(m)
meanPreds = apply(preds, 2, mean)
plot(imp$dsBehaviors, meanPreds, col = adjustcolor('firebrick', .4), xlab = 'observed', ylab = 'predicted')

write.csv(imp, 'data/multipleImputed')

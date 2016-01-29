setwd(file.path('~', 'Dropbox', 'FireAtTheFringe'))  # Set working directory
d = read.csv("data/clean_with_neighborsDS_and_proximity.csv")
library(dplyr)
library(Amelia)
library(mvtnorm)

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
# write.csv(averaged, 'data/multipleImputed.csv', row.names = FALSE)

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
imp$city = as.character(imp$city)
imp$city = 
    ifelse(imp$city %in% c('Pine Valley', 'Boulevard'), 'Pine Valley-Boulevard',
           ifelse(imp$city %in% c('El Cajon', 'Lakeside'), 'El Cajon-Lakeside',
                  ifelse(imp$city %in% c('Santa Ysabel', 'Julian'), 'Santa Ysabel-Julian', imp$city)))
cityRates = 
    group_by(imp, city) %>%
    summarise(avgDS = mean(dsBehaviors))
imp$cityRate = cityRates$avgDS[match(imp$city, cityRates$city)]
imp$logDist = log(1 + imp$near_03f)
imp$cityRate4 = imp$cityRate^4
imp$cityRate2 = imp$cityRate^2
imp$cityRateExp = exp(imp$cityRate)

# corrplot(cor(imp[, -which(names(imp) == 'city')]))

par(mfrow = c(3, 3))
lapply(names(imp)[-which(names(imp) == 'city')], function(x) {
    dens(imp[[x]], main = x)
    })

# Standardize all predictors:
for(i in names(imp)[-which(names(imp) %in% c('city', 'dsBehaviors'))])
    imp[[i]] = scale(imp[[i]])

# Squash DS into [0, 1]
imp$dsBehaviors = (imp$dsBehaviors - min(imp$dsBehaviors)) / max(imp$dsBehaviors)

models = list(ExpSurvey = lm(dsBehaviors ~ policyBeliefs + effectiveness + experience + risk + cityRate, data = imp),
              ExpDirect = lm(dsBehaviors ~ policyBeliefs + effectiveness + near_03f + risk + cityRate, data = imp),
              ExpLog = lm(dsBehaviors ~ policyBeliefs + effectiveness + logDist + risk + cityRate, data = imp),
              ExpLog2 = lm(dsBehaviors ~ policyBeliefs + effectiveness + logDist + risk + cityRate2, data = imp),
              ExpLog4 = lm(dsBehaviors ~ policyBeliefs + effectiveness + logDist + risk + cityRate4, data = imp),
              ExpLogExp = lm(dsBehaviors ~ policyBeliefs + effectiveness + logDist + risk + cityRateExp, data = imp)
              )
sapply(models, AIC)
# library(stargazer)
# stargazer(models, type = 'text')
m = models[[sapply(models, AIC) %>% which.min]]
summary(m)
formula(m)

saveRDS(m, 'data/derived/model.RDS')
saveRDS(imp, 'data/derived/imputedData.RDS')

# What are the differences across towns?
table(imp$city) %>% sort
ggplot(imp, aes(x = city, y = dsBehaviors)) +
    geom_violin(fill = "gray") +
    theme_bw() +
    theme(axis.text.x=element_text(angle = -90, hjust = 0, size = 12))

# How does the MVNormal assumption look across our sample:
# graphics.off()
# preds = c('policyBeliefs', 'effectiveness', 'risk', 'logDist')
# pairs(imp[, preds], col = adjustcolor('blue', .1))


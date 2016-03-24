library(dplyr)
library(rethinking)
d = readRDS('data/derived/imputedData.RDS')
summary(d)

##### First "reproduce" old model with Bayesian style
d1 = select(d, dsBehaviors, policyBeliefs, effectiveness, risk, logDist, cityRate)

mOld = 
    map2stan(
        alist(
            dsBehaviors ~ dnorm( mu, sigma ) ,
            mu <- a + bP * policyBeliefs + bE * effectiveness + bR * risk + bD * logDist + bC * cityRate,
            a ~ dnorm(.5, 2) ,
            c(bP, bE, bR, bD, bC) ~ dnorm(0, 1),
            sigma ~ dcauchy(0, 2)
        ), 
        data = d1,
        start = list(a = .5, bP = 0, bE = 0, bR = 0, bD = 0, bC = 0, sigma = 1),
        chains = 2, cores = 2
    )

plot(mOld)
plot(precis(mOld))

# How do those estimates compare to the old model?
m = readRDS('data/derived/model.RDS')
summary(m)
summary(mOld)
# Basically identical. Except now we have an estimated value for the variance.

##### Add varying intercepts by town
# Note: Squashed dsBeh into [0, 1], but it's a 0-6 count
# Probably should model this as binomial with n = 6

d2 = select(d, dsBehaviors, policyBeliefs, effectiveness, risk, logDist, city)
d2$cityIndex = coerce_index(d2$city)

m2 = 
    map2stan(
        alist(
            dsBehaviors ~ dnorm( mu, sigma ) ,
            mu <- a_city[cityIndex] + bP * policyBeliefs + bE * effectiveness + bR * risk + bD * logDist,
            a_city[cityIndex] ~ dnorm(a, sigma_cities),
            a ~ dnorm(.5, 2),
            sigma_cities ~ dcauchy(0, 2),
            sigma ~ dcauchy(0, 2),
            c(bP, bE, bR, bD) ~ dnorm(0, 1)
        ), 
        data = d2
        , start = list(a = .5, a_city = rep(.5, length(unique(d2$cityIndex))), bP = 0, bE = 0, bR = 0, bD = 0, sigma = 1, sigma_cities = 1)
        , chains = 3, cores = 3
        , iter = 5e3, warmup = 2e3
    )
plot(m2)
precis(m2)
par(mfrow = c(1, 1))
plot(coeftab(mOld, m2))

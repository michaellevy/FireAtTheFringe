library(dplyr)
library(rethinking)
library(tidyr)
library(ggplot2)
invisible(lapply(list.files("code/functions/", full.names = TRUE), source))
d = readRDS('data/derived/imputedData.RDS')
strs = read.csv("data/derived/mergedPoints.csv") 
strs = mutate(strs, logDensityTr = log(popDensityTr), logDensityBl = log(popDensityBg))

d2 = inner_join(d, strs[!is.na(strs$surveyId), ], by = c("id" = "surveyId")) %>%
  rename(city = city.y) %>% select(-city.x)

# BG density is severely right skewed so prob use log. Tr not so much
par(mfrow = c(1, 2))
dens(log(d2$popDensityBg)); dens(d2$popDensityTr)

janitor::crosstab(d2, wuiClass10, insideCnfBuffer)  # Hmm, homes outside the 
# buffer are really out-of-sample for us, but it's a shame to give up 1/3 of our data.
# Try filtering and see what difference it makes

numPredictors = c("effectiveness", "risk", "logDensityBl", "popDensityTr")

# Standardize numeric predictors, but get rid of scale-attributes before they muck up stan:
for(i in numPredictors) {
  tmp = scale(d2[[i]])
  attributes(tmp) = NULL
  d2[[i]] = tmp
}

d2$cityIndex = coerce_index(d2$city)

dm1 = select(d2, cityIndex, effectiveness, risk, logDensityBl, wuiClass10, numBehaviors)
m1 = map2stan(
  alist(
    numBehaviors ~ dbinom( 4 , p ) ,
    logit(p) <- 
      a_city[cityIndex] + 
      bEffectiveness * effectiveness + 
      bRisk * risk +
      bDensity * logDensityBl +
      bWUI * wuiClass10,
    a_city[cityIndex] ~ dnorm(a, sigma_cities),
    sigma_cities ~ dcauchy(0, 2),
    a ~ dnorm(.5, 1),
    c(bEffectiveness, bRisk, bDensity, bWUI) ~ dnorm(0, 1)
  ), 
  data = dm1
  , chains = 3, cores = 3
  , iter = 1e4, warmup = 2.5e3
)
png("results/estimates.png")
plot(coeftab(m1))
abline(v = 0, col = "red")
dev.off()
saveRDS(m1, "data/derived/modelBGdensity.RDS")
write.csv(dm1, "data/derived/modelBGdensity-data.csv", row.names = FALSE)

# With census tract instead of blockgroup
dm2 = select(d2, cityIndex, effectiveness, risk, popDensityTr, wuiClass10, numBehaviors)
m2 = map2stan(
  alist(
    numBehaviors ~ dbinom( 4 , p ) ,
    logit(p) <- 
      a_city[cityIndex] + 
      bEffectiveness * effectiveness + 
      bRisk * risk +
      bDensity * popDensityTr +
      bWUI * wuiClass10,
    a_city[cityIndex] ~ dnorm(a, sigma_cities),
    sigma_cities ~ dcauchy(0, 2),
    a ~ dnorm(.5, 1),
    c(bEffectiveness, bRisk, bDensity, bWUI) ~ dnorm(0, 1)
  ), 
  data = dm2
  , chains = 3, cores = 3
  , iter = 1e4, warmup = 2.5e3
)
plot(coeftab(m2))
abline(v = 0, col = "red")
compare(m1, m2)  # Clear preference for block groups

dm3 = select(d2, cityIndex, effectiveness, risk, logDensityBl, numBehaviors)
m3 = map2stan(
  alist(
    numBehaviors ~ dbinom( 4 , p ) ,
    logit(p) <- 
      a_city[cityIndex] + 
      bEffectiveness * effectiveness + 
      bRisk * risk +
      bDensity * logDensityBl,
    a_city[cityIndex] ~ dnorm(a, sigma_cities),
    sigma_cities ~ dcauchy(0, 2),
    a ~ dnorm(.5, 1),
    c(bEffectiveness, bRisk, bDensity) ~ dnorm(0, 1)
  ), 
  data = dm3
  , chains = 3, cores = 3
  , iter = 1e4, warmup = 2.5e3
)
compare(m1, m3)
plot(coeftab(m1, m3))
abline(v = 0, col = "red")
# m1 wins. 

table(d2$wuiClass10)  # Group 0 and 1 together since there are only 19 0's
dm4 = select(d2, cityIndex, effectiveness, risk, logDensityBl, wuiClass10, numBehaviors) %>%
  mutate(wuiClass10 = ifelse(wuiClass10 < 2, 0, 1))
m4 = map2stan(
  alist(
    numBehaviors ~ dbinom( 4 , p ) ,
    logit(p) <- 
      a_city[cityIndex] + 
      bEffectiveness * effectiveness + 
      bRisk * risk +
      bDensity * logDensityBl +
      bWUI * wuiClass10,
    a_city[cityIndex] ~ dnorm(a, sigma_cities),
    sigma_cities ~ dcauchy(0, 2),
    a ~ dnorm(.5, 1),
    c(bEffectiveness, bRisk, bDensity, bWUI) ~ dnorm(0, 1)
  ), 
  data = dm4
  , chains = 3, cores = 3
  , iter = 1e4, warmup = 2.5e3
)
compare(m1, m4)
# 50/50

# Old ->

if(!all(file.exists("data/derived/modelWithHousingDensity.RDS", "data/derived/noDistance-noPolicy.RDS"))) {
  m5 =     
    map2stan(
      alist(
        numBehaviors ~ dbinom( 4 , p ) ,
        logit(p) <- a_city[cityIndex] + bEffectiveness * effectiveness + bRisk * risk,
        a_city[cityIndex] ~ dnorm(a, sigma_cities),
        sigma_cities ~ dcauchy(0, 2),
        a ~ dnorm(.5, 1),
        c(bEffectiveness, bRisk) ~ dnorm(0, 1)
      ), 
      data = d2
      , chains = 3, cores = 3
      , iter = 1e4, warmup = 2.5e3
    )
  saveRDS(m5, "data/derived/noDistance-noPolicy.RDS")
  
  m6 = 
    map2stan(
      alist(
        numBehaviors ~ dbinom( 4 , p ) ,
        logit(p) <- a_city[cityIndex] + bEffectiveness * effectiveness + bRisk * risk + bHDens * logDensity,
        a_city[cityIndex] ~ dnorm(a, sigma_cities),
        sigma_cities ~ dcauchy(0, 2),
        a ~ dnorm(.5, 1),
        c(bEffectiveness, bRisk, bHDens) ~ dnorm(0, 1)
      ), 
      data = d2
      , chains = 3, cores = 3
      , iter = 1e4, warmup = 2.5e3
    )
  saveRDS(m6, "data/derived/modelWithHousingDensity.RDS")  
} else {
  m5 = readRDS("data/derived/noDistance-noPolicy.RDS")
  m6 = readRDS("data/derived/modelWithHousingDensity.RDS")
}

compare(m5, m6)   # 100% weight on m6 with housing density
plot(coeftab(m5, m6))

latexCoeftab(m6)

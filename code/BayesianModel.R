library(dplyr)
library(rethinking)
library(tidyr)
library(ggplot2)
invisible(lapply(list.files("code/functions/", full.names = TRUE), source))
d = readRDS('data/derived/imputedData.RDS')
strs = readRDS("data/derived/structuresWithDensity.RDS")
d2 = 
  inner_join(d, strs[!is.na(strs$id), ], by = "id") %>% 
  mutate(logDensity = log(dens)) %>%
  select(numBehaviors, effectiveness, risk, city, logDensity)
nrow(d); nrow(d2) # 28 rows lost in this merge. Maybe PO Boxes? Ignoring for now at least.
d2$cityIndex = coerce_index(d2$city)
saveRDS(d2, "data/derived/dataVImodel.RDS")

# Standardize all predictors, but get rid of scale-attributes before they muck up stan:
for(i in names(d2)[!names(d2) %in% grep("city", names(d2), value = TRUE)]) {
  tmp = scale(d2[[i]])
  attributes(tmp) = NULL
  d2[[i]] = tmp
}
head(d2)

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

compare(m5, m6)   # 100% weight on m6 with housing density
plot(coeftab(m5, m6))

latexCoeftab(m6)

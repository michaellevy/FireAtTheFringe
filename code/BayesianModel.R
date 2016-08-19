library(dplyr)
library(rethinking)
library(tidyr)
library(ggplot2)
d = readRDS('data/derived/imputedData.RDS')
str(d)

d1 = select(d, dsBehaviors, policyBeliefs, effectiveness, risk, logDist, cityRate)

d2 = select(d, dsBehaviors, policyBeliefs, effectiveness, risk, logDist, city)
d2$cityIndex = coerce_index(d2$city)
d2 = lapply(d2, function(x) {
  attributes(x) = NULL
  x
})
# To model as binomial process with N = 4 for each case
d2$numBehaviors = d2$dsBehaviors * 4   # undoing what was done in multipleImputation.R
saveRDS(d2, "data/derived/dataVImodel.RDS")



##### First "reproduce" old model with Bayesian style
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
dev.off()
plot(coeftab(mOld, m2), cex = .75)
# Still no major changes from old model in terms of point estimates, but
# now have appropriate measures of uncertainty among and within towns and
# have accounted for that level of clustering in the data.


m3 = 
    map2stan(
        alist(
            numBehaviors ~ dbinom( 4 , p ) ,
            logit(p) <- a_city[cityIndex] + bPolicy * policyBeliefs + 
                bEffectiveness * effectiveness + bRisk * risk + bDistance * logDist,
            a_city[cityIndex] ~ dnorm(a, sigma_cities),
            sigma_cities ~ dcauchy(0, 2),
            a ~ dnorm(.5, 1),
            c(bPolicy, bEffectiveness, bRisk, bDistance) ~ dnorm(0, 1)
        ), 
        data = d2
        , chains = 3 #, cores = 3
        , iter = 1e4, warmup = 2.5e3
    )
saveRDS(m3, "data/derived/varyInterceptModel.RDS")
plot(m3)
dev.off()
plot(precis(m3, depth = 1))

m3 = readRDS("data/derived/varyInterceptModel.RDS")
paramSamps = extract.samples(m3, n = 5e3)
betas = as.data.frame(paramSamps[4:7])
corrplot::corrplot(cor(betas), method = 'ellipse', diag = FALSE, addCoef.col = 'black', type = 'upper')
gather(betas, parameter, coefficient) %>%
    ggplot(aes(x = parameter, y = coefficient)) + 
    geom_violin(fill = 'gray') +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw()

# Or as a table:
tab = precis(m3, depth = 2, prob = .95)@output
rownames(tab)[1:7] = paste0('\\alpha_{', sort(unique(d2$city)))
betas = grepl("^b", rownames(tab))
rownames(tab)[betas] = substr(rownames(tab)[betas], 1, 2)
rownames(tab) = stringr::str_replace(rownames(tab), "^b", "\\\\beta_{")
rownames(tab) = stringr::str_replace(rownames(tab), "sigma_cities", "\\\\sigma")
noSubs = !grepl("\\{", rownames(tab))
rownames(tab)[rownames(tab) == "a"] = "\\alpha"
rownames(tab)[!noSubs] = paste0(rownames(tab)[!noSubs], "}")
rownames(tab) = paste0("$", rownames(tab), "$")
colnames(tab)[2:4] = c("SD", "Lower 95% CI", "Upper 95% CI")
mdTab = round(tab[c(10:13, 9, 8, 1:7), 1:4], 2)
# separate = mdTab[1, ]
# rownames(separate) = "Varying Intercepts"
# separate[, 1:4] = rep("", 4)
# mdTab = rbind(
#   tab[1:6, ],
#   separate,
#   tab[7:13, ]
# )
# Copy and paste this into markdown writeup:
(coefTab = knitr::kable(mdTab, digits = 2, align = rep('c', 4)))


simOrig = sim(m3, data = d2)
dd2 = d2
dd2$effectiveness = dd2$effectiveness + 2
simEff = sim(m3, data = dd2)
ddd2 = d2
ddd2$risk = ddd2$risk + 2
simRisk = sim(m3, data = ddd2)

sims = 
    rbind(
        cbind(t(simOrig), city = d2$city, simulation = rep('original_data')),
        cbind(t(simEff), city = d2$city, simulation = rep('effectiveness_promoted')),
        cbind(t(simRisk), city = d2$city, simulation = rep('greater_perceived_risk'))
    ) %>% as.data.frame %>%
    gather(data, number_behaviors, -city, -simulation)
sims$number_behaviors = as.integer(sims$number_behaviors)
sims$simulation = factor(sims$simulation, levels = rev(levels(sims$simulation)))

interventionResponsesPlot = 
    ggplot(sims, aes(x = number_behaviors, fill = simulation)) + 
    geom_density() +
    facet_grid(simulation ~ city) +
    theme_bw() + 
    scale_fill_brewer(type = 'qual', palette = 'Dark2', guide = 'none') 

ggsave('results/interventionEffects.png', interventionResponsesPlot, width = 10, height = 7)

##### Model Comparison
m4 =     
  map2stan(
    alist(
      numBehaviors ~ dbinom( 4 , p ) ,
      logit(p) <- a_city[cityIndex] + bPolicy * policyBeliefs + 
        bEffectiveness * effectiveness + bRisk * risk,
      a_city[cityIndex] ~ dnorm(a, sigma_cities),
      sigma_cities ~ dcauchy(0, 2),
      a ~ dnorm(.5, 1),
      c(bPolicy, bEffectiveness, bRisk) ~ dnorm(0, 1)
    ), 
    data = d2
    , chains = 3 #, cores = 3
    , iter = 1e4, warmup = 2.5e3
  )
saveRDS(m4, "data/derived/noDistanceModel.RDS")

m5 =     
  map2stan(
    alist(
      numBehaviors ~ dbinom( 4 , p ) ,
      logit(p) <- a_city[cityIndex] + bEffectiveness * effectiveness + bRisk * risk,
      a_city[cityIndex] ~ dnorm(a, sigma_cities),
      sigma_cities ~ dcauchy(0, 2),
      a ~ dnorm(.5, 1),
      c(bPolicy, bEffectiveness, bRisk) ~ dnorm(0, 1)
    ), 
    data = d2
    , chains = 3 #, cores = 3
    , iter = 1e4, warmup = 2.5e3
  )
saveRDS(m5, "data/derived/noDistance-noPolicy.RDS")

m3 = readRDS("data/derived/varyInterceptModel.RDS")
m4 = readRDS("data/derived/noDistanceModel.RDS")
m5 = readRDS("data/derived/noDistance-noPolicy.RDS")
(comp = compare(m3, m4, m5))
knitr::kable(round(comp@output, 2))
plot(comp)



#### To do:
# Simultaniously impute missing data and estimate model to get info flowing both ways.
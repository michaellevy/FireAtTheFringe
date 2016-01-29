m = readRDS('data/derived/model.RDS')
imp = readRDS('data/derived/imputedData.RDS')

# Draw parameters for N agents
drawSamples = function(N, mod = m, dat = imp, predictors = names(coef(mod))[-grep('Intercept|cityRate', names(coef(mod)))]) {
    parameterSamples = rmvnorm(N, mean = coef(mod), sigma = vcov(mod))
    
    # Construct MVNormal distribution for each city.
    # Will need less than N individuals for each city, but go ahead and draw N for each and sample from them later
    predictorSamples = 
        lapply(as.character(unique(dat$city)), function(city) {
            dat = dat[dat$city == city, predictors]
            cbind(rmvnorm(n = N, mean = colMeans(dat), sigma = cov(dat)), city = city) %>%
                as.data.frame
        }) %>%
        do.call(rbind, .)
    
    write.csv(parameterSamples, 'data/derived/parameterSamples.csv', row.names = FALSE)
    write.csv(predictorSamples, 'data/derived/predictorSamples.csv', row.names = FALSE)
    return('Wrote samples to data/derived/')
}

drawSamples(1e2)
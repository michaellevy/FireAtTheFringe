setwd(file.path('~', 'GitHub', 'FireAtTheFringe'))  # Set working directory
survey = read.csv('data/cleanData.csv')  # Read the data in

# specify the model
# latent variable =~ indicator1 + indicator2 + indicator3
model <- 'anthro  =~ c1e + c1f + c1g + c1h + c1i
          biocentric =~ c1a + c1b + c1c + c1d
          freedom =~ c1o + c1p + c1q
          responsibility =~ c1j + c1k + c1l
          commattach =~ b11a + b11b + b11c + b11d + b11e + b11f + 
                        b11g + b11h + b11i + b11j + b11k
          effectiveness =~ f1a + f1b + f1c + f1d + f1e + f1f + f1g
          risk =~ f5a + f5b + f5c
          trust =~ g2a + g2b + g2c + g2d
          dsfuture =~ f2a2 + f2b2 + f2c2 + f2d2 + f2e2 + f2f2 + f2g2
'

# fit the model and display output
fit <- cfa(model, data = d)
summary(fit, fit.measures = TRUE)

# SEM model
semmodel <- '
  # measurement model
    anthro  =~ c1e + c1f + c1g + c1h + c1i
    biocentric =~ c1a + c1b + c1c + c1d
    freedom =~ c1o + c1p + c1q
    responsibility =~ c1j + c1k + c1l
    commattach =~ b11a + b11b + b11c + b11d + b11e + b11f + 
                  b11g + b11h + b11i + b11j + b11k
    effectiveness =~ f1a + f1b + f1c + f1d + f1e + f1f + f1g
    risk =~ f5a + f5b + f5c
    trust =~ g2a + g2b + g2c + g2d
    dsfuture =~ f2a2 + f2b2 + f2c2 + f2d2 + f2e2 + f2f2 + f2g2
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

# compute current DS behavior
dsBehavior = names(d)[grepl("f2.1", names(d))]
d$sumDSBehavior = apply(d[dsBehavior], 1, 
                        function(x) sum(x, na.rm = TRUE))

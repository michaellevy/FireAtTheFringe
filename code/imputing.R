library(dplyr)
library(Amelia)
d = read.csv("data/cleanData.csv")

dd = d[, grep('id|city|^c1|^e|^f1|f2[a-g]1|^f5[a-d]|^d1[a-g]|h1|near_03f', names(d))]

set.seed(5038)
system.time({  # 10"
  mi = amelia(dd, m = 10, p2s = 1, idvars = 'id', noms = 'city', parallel = 'multicore', ncpus = 3)  
})
cats = 
    lapply(mi$imputations, function(d) {
        data.frame(
            policyBeliefs = d$e1a + d$e1g - d$e1c - d$e1f,
            effectiveness = apply(d[, grep('f1[d-g]', names(d))], 1, sum),
            experience = d$h1b + d$h1c,
            risk = apply(d[, grep('f5[a-c]', names(d))], 1, sum),
            numBehaviors = apply(d[, grep('f2[d-g]1', names(d))], 1, sum)
        )
    })
imp = Reduce("+", cats) / length(cats)

imp$city = as.character(dd$city)
imp$city = 
    ifelse(imp$city %in% c('Pine Valley', 'Boulevard'), 'Pine Valley-Boulevard',
           ifelse(imp$city %in% c('El Cajon', 'Lakeside'), 'El Cajon-Lakeside',
                  ifelse(imp$city %in% c('Santa Ysabel', 'Julian'), 'Santa Ysabel-Julian', imp$city)))

imp$id = dd$id
saveRDS(imp, 'data/derived/imputedData.RDS')

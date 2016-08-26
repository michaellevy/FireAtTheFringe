library(ggmap)
library(dplyr)
library(ks)
d = readRDS("data/derived/mergedPoints.RDS")
summary(d)
d$insideCnfBuffer = as.logical(d$insideCnfBuffer)
d$surveyed = !is.na(d$surveyId)

# source("~/Dropbox/R/util.R")
# lats = rangeExpand(d$myLat, 20)
# longs = rangeExpand(d$myLong, 20)
# cnf = get_map(c(longs[1], lats[1], longs[2], lats[2]),
#               source = "stamen", maptype = "toner")
# baseMap = ggmap(cnf)
# saveRDS(baseMap, "data/derived/baseMap.RDS")
baseMap = readRDS("data/derived/baseMap.RDS")
# mp = 
#   baseMap +
#   geom_point(data = d, 
#              mapping = aes(x = myLong, y = myLat, 
#                            color = surveyed, size = surveyed),
#              shape = 16) +
#   guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
#   scale_size_manual(values = c("FALSE" = .25, "TRUE" = 2))
# ggsave("results/mergedPointsMapTest.pdf", mp, width = 8, height = 6)

# Filter out the dense western edge:
keep = d$myLat > d$myLong * -3.33 - 356.5
# system.time({   # 10h
#   H = Hscv(d[keep, 4:5], verbose = TRUE)
# })
# myKDE = kde(d[keep, 4:5], H = H)
saveRDS(myKDE, "data/derived/homesKDE.RDS")  # Note H is stored in here: all.equal(H, myKDE[["H"]])
homesKDE = kde(x = d[keep, 4:5], 
               H = myKDE[["H"]], 
               eval.points = d[keep, 4:5])  

homeEsts = data.frame(homesKDE$eval.points, 
                      dens = homesKDE$estimate, 
                      id = d$surveyId[keep],
                      inBuffer = d$insideCnfBuffer[keep])

png("results/housingDensities.png", width = 500, height = 300)
par(mfrow = c(1, 2))
plot(density(homesKDE$estimate), main = "Density")
plot(density(log(homesKDE$estimate)), main = "Log density")
dev.off()

baseMap + 
  geom_point(data = homeEsts,
             mapping = aes(x = myLong, y = myLat, color = log(dens)),
             size = .3)
ggsave("results/mapHousingDensities.png", width = 6, height = 4)

ggSurvey = 
  ggplot(homeEsts, aes(x = dens, fill = !is.na(id))) + 
  geom_density(alpha = .5) +
  scale_x_log10(name = "Housing density") + annotation_logticks(sides = "b") +
  scale_fill_brewer(name = "Survey respondent") +
  geom_vline(xintercept = exp(2)) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))

ggBuffer = 
  ggplot(homeEsts, aes(x = dens, fill = inBuffer)) + 
  geom_density(alpha = .5) +
  scale_x_log10(name = "Housing density") + annotation_logticks(sides = "b") +
  scale_fill_brewer(name = "In 1.5 mile buffer") +
  geom_vline(xintercept = exp(2)) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))

cowplot::plot_grid(ggSurvey, ggBuffer)
ggsave("results/housingDensityCompare.png", width = 10, height = 4)

baseMap + 
  geom_point(aes(color = log(dens) > 2, x = myLong, y = myLat), homeEsts, size = .3) +
  guides(color = guide_legend(override.aes = list(size = 2)))
ggsave("results/mapHousingDensityCutoff.png", width = 6, height = 4)

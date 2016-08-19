d = readRDS("data/derived/mergedPoints.RDS")
summary(d)
d$insideCnfBuffer = as.logical(d$insideCnfBuffer)
d$surveyed = !is.na(d$surveyId)

library(ggmap)
source("~/Dropbox/R/util.R")
lats = rangeExpand(d$myLat, 20)
longs = rangeExpand(d$myLong, 20)
cnf = get_map(c(longs[1], lats[1], longs[2], lats[2]),
              source = "stamen", maptype = "toner")

mp = 
  ggmap(cnf) +
  geom_point(data = d, 
             mapping = aes(x = myLong, y = myLat, 
                 color = surveyed,
                 size = surveyed),
             shape = 16) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  scale_size_manual(values = c("FALSE" = .25, "TRUE" = 2))
ggsave("mergedPointsMapTest.pdf", mp, width = 8, height = 6)

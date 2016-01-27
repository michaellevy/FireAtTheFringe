setwd(file.path('~', 'GitHub', 'FireAtTheFringe')) # Set working directory
d = read.csv('data/cleanDataSPSS.csv')  # Read the data in
source("code/addNeighborsDSBehavior.R")
dists = read.csv("data/withDist.csv")
dists = subset(dists, select = c("ID", "near_cnf", "near_03f"))
d = merge(d, dists, by.x = "id", by.y = "ID")

# Subset data for sem
sem  <- subset(d, select = c(id, e1a, e1c, e1f, e1g, f5a:f5c, f1a:f1g, f2a1, f2b1, f2c1, f2d1, f2e1, f2f1, f2g1, f2a2, f2b2, f2c2, f2d2, f2e2, f2f2, f2g2, h1a:h1c, near_03f))


# Write data for sem analysis to csv file
write.csv(sem, 'data/semdata.csv', row.names = FALSE)

# Write data for sem analysis in mplus to csv file (no column names)
write.table(sem, 'mplus/mplusdata.csv', row.names = FALSE, col.names = FALSE, na = "99", sep=",")

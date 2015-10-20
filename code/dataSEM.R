setwd(file.path('~', 'GitHub', 'FireAtTheFringe')) # Set working directory
d = read.csv('data/cleanDataSPSS.csv')  # Read the data in
source("code/addNeighborsDSBehavior.R")
dists = read.csv("data/withDist.csv")
dists = subset(dists, select = c("ID", "near_cnf", "near_03f"))
d = merge(d, dists, by.x = "id", by.y = "ID")

sem  <- subset(d, select = c(id, c1a:c1r, f1a:f1g, f2a1:f2g2, f5a:f5c, neighbors_f2a1:near_03f))

# Write data for sem analysis in mplus to csv file
write.table(sem, 'mplus/mplusdata.csv', row.names = FALSE, col.names = FALSE, na = "99", sep=",")

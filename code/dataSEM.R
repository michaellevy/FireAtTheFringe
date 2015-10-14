setwd(file.path('~', 'GitHub', 'FireAtTheFringe')) # Set working directory
survey = read.csv('data/cleanDataSPSS.csv')  # Read the data in
sem  <- subset(data, select = c(id, c1a:c1r, f1a:f1g, f2a1:f2g2, f5a:f5c))

# Write data for sem analysis in mplus to csv file
write.table(sem, 'mplus/mplusdata.csv', row.names = FALSE, col.names = FALSE, na = "99", sep=",")

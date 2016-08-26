survey <- read.csv('data/refineData.csv', header=TRUE)  # Read the refined data file

#delete variables: region, jrecq4, identity, affect, depend, social, community, risk, trust
cleandata <- subset(survey, select = -c(region, recaq4, identity, affect, depend, social, community, risk, trust))


# Change questions coded as yes = 1, no = 2 to yes = 1, no = 0:
recode <- c('a1','a2','b3','b9','b10','f2a1','f2b1','f2c1','f2d1','f2e1','f2f1',
            'f2g1','f3a1','f3b1','f3c1','f3d1','f3e1','f3f1','h1a','h1b','h1c',
            'h1d','h1e','h1f','h1g','h1h','h1i','h1j','h1k','h1l','h1m','h1n',
            'h1o','h1p')
cleandata[, which(names(cleandata) %in% recode)] <-
  ifelse(cleandata[ ,which(names(cleandata) %in% recode)] == '2', 0, 1)	
recode1 <- c('i1', 'i2', 'j7', 'j8')	
cleandata[, which(names(cleandata) %in% recode1)] <-
  ifelse(cleandata[, which(names(cleandata) %in% recode1)] == '1', 0, 1)	

# Change the id number of the duplicated entry
cleandata[duplicated(cleandata$id), 'id'] <- cleandata[duplicated(cleandata$id), 'id'] + 1

# Exclude renters and a5 = 5: "other" type homes:
cleandata = cleandata[cleandata$a3 != 5, ]
cleandata = cleandata[cleandata$a5 != 1, ]

# Drop multiple responses from same address:
cleandata = subset(cleandata, !(cleandata$id %in% c(955, 1925, 2041)))

#change NA's meaning missing to blanks in data.frame for spss
cleandata2 <- sapply(cleandata, as.character)
cleandata2[is.na(cleandata2)] <- " "

# Write cleaned data to csv file to be read for SPSS, STATA:
write.csv(cleandata2, 'data/cleanDataSPSS.csv', row.names = FALSE)


# Assign NA to missing values that were coded 99 or 999:
cleandata[cleandata == 99 | cleandata == 999] <- NA  

# On likert scale Qs, change not applicable to missings:
notapplic <- c('a5nd','a5nm','a5md','a5mm','g1a','g1b','g1c','g1d','g1e','g2a',
               'g2b','g2c','g2d')
cleandata[, which(names(cleandata) %in% notapplic)] <-
  apply(
    cleandata[, which(names(cleandata) %in% notapplic)],
    MARGIN = c(1, 2), function(x) 
      ifelse(x == '6', NA, x))

# Merge acres and sqft:
d$a8acres = ifelse(is.na(d$a8acres) & !is.na(d$a8sqfeet),
                   d$a8sqfeet / 43560,
                   d$a8acres)
# Get rid of sqft:
d = subset(d, select = -a8sqfeet)

# Write cleaned data to csv file to be read for R:
write.csv(cleandata, 'data/cleanData.csv', row.names = FALSE)



#delete PO boxes?, delete non-owners?, delete duplicated addresses?

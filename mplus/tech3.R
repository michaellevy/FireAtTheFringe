# Set working directory
setwd(file.path('~', 'GitHub', 'FireAtTheFringe', 'mplus'))


#install.packages("MplusAutomation")
library(MplusAutomation)
sessionInfo()

runModels()

#allOutput <- readModels()
semOutput <- readModels("2-SEM-Future.out")

tech3 <- semOutput= c(savedata_info$tech3file)





# allOutput <- readModels("mplus", recursive=TRUE)

#fit <- mplusModeler(future, 'data/semfut.csv', modelout = 'mplus/semfut.inp')



library(texreg)
screenreg(fit, summaries = c("Observations", "CFI", "SRMR"), single.row=TRUE)

pathmodel2 <- update(pathmodel, MODEL = ~ . + "
    mpg ON disp;
                     wt ON hp;")

fit2 <- mplusModeler(pathmodel2, "mtcars2.dat", modelout = "model2.inp", run = 1L)


screenreg(list(
  extract(fit, summaries = c("Observations", "CFI", "SRMR")),
  extract(fit2, summaries = c("Observations", "CFI", "SRMR"))),
  single.row=TRUE)


library(plyr)
tech3 <- do.call("rbind.fill",sapply(allOutput,"[", "tech3"))

mySummaries <- extractModelSummaries("mplus", recursive=TRUE)

##############
# Green Book Subset Data By Fed Chairman Term
# Christopher Gandrud
# 26 January 2013
##############



setwd("/git_repositories/GreenBook/Paper/")

load("ModelObjects.RData")

# library(Zelig)
library(stringr)

#### Run regressions dropping each presidential term ####
# Create funciton and run regressions
ChairTerms <- c("Bernanke", "Burns", "Greenspan", "Martin", "Miller", "Volcker")
)

SubSetChairTerms <- function(x){
  assign("SubData", subset(cpi.data2, Chair != x), envir = .GlobalEnv)
  Temp <- Zelig::zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP +
                               PotentialGDP + DiscountRate2qChange + UNRATE + 
                               time_to_election + pres_party + GlobalModel, 
                             model = "normal", data = SubData, cite = FALSE)
  STemp <- summary(Temp)
  STemp
}

lapply(ChairTerms, SubSetChairTerms)
rm(SubData)
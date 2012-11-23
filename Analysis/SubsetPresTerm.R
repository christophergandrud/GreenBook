##############
# Green Book Subset Data By Presidential Term
# Christopher Gandrud
# 23 November 2012
##############

# library(Zelig)

# Create funciton and run regressions
PresTerms <- c("Nixon1", "Nixon2", "Ford1", "Carter1", "Reagan1", "Reagan2", 
                "GHWBush1", "Clinton1", "Clinton2", "GWBush1", "GWBush2"
                )

SubSetPresTerms <- function(x){
  SubData <- subset(cpi.data2, presTerm != x)
  SData <- paste("S", x, sep = "")
  assign(SData, zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP +
                        PotentialGDP + DiscountRate2qChange + UNRATE + time_to_election +
                        pres_party + GlobalModel, model = "ls", data = SubData, cite = FALSE), 
         env=.GlobalEnv)
}

lapply(PresTerms, SubSetPresTerms)
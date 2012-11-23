##############
# Green Book Subset Data By Presidential Term
# Christopher Gandrud
# 23 November 2012
##############

# library(Zelig)

# Create funciton and run regressions
PresTerms <- c("Nixon1", "Nixon2", "Ford1", "Carter1", "Regan1", "Regan2", "GHWBush1", "Clinton1", "Clinton2", "GWBush1", "GWBush2")

SubSetPresTerms <- function(x){
  SubData <- subset(cpi.data2, presTerm != x)
  SData <- paste("S", x, sep = "")
  assign(SData, summary(zelig(error.prop.deflator.q2 ~ pres_party + 
                                time_to_election + recession + senate_dem_rep +
                                house_dem_rep + DebtGDP + ExpenditureGDP + 
                                PotentialGDP + DiscountRate2qChange + GlobalModel +
                                UNRATE, model = "ls", data = SubData, 
                              cite = FALSE)), 
         env=.GlobalEnv)
}

lapply(PresTerms, SubSetPresTerms)
##############
# Green Book Subset Data By Presidential Term
# Christopher Gandrud
# 17 January 2013
##############

# library(Zelig)
library(stringr)

#### Run regressions dropping each presidential term ####
# Remove spaces in president term names
cpi.data2$presTerm<- str_replace_all(cpi.data2$presTerm, " ", "")
cpi.data2$elect2 <- factor(cpi.data2$presTerm)

# Create funciton and run regressions
PresTerms <- c("Nixon1", "Nixon2", "Ford1", "Carter1", "Reagan1", "Reagan2", 
                "GHWBush1", "Clinton1", "Clinton2", "GWBush1", "GWBush2"
                )

SubSetPresTerms <- function(x){
  assign("SubData", subset(cpi.data2, presTerm != x), envir = .GlobalEnv)
  SData <- paste0("S", x)
  assign(SData, Zelig::zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP +
                        PotentialGDP + DiscountRate2qChange + UNRATE + time_to_election +
                        pres_party + GlobalModel, model = "normal", data = SubData, cite = FALSE), 
         envir = .GlobalEnv)
}

lapply(PresTerms, SubSetPresTerms)
rm(SubData)
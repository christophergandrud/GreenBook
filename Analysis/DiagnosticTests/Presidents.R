################
# GreenBook: President Diagnostics
# Christopher Gandrud
# 22 November 2012
###############

## Load libraries
# library(devtools)
library(Zelig)
library(stringr)
# library(plyr)

# To run as a stand alone file. First, run the following files from earlier in the paper:
## source_url("http://bit.ly/NXdCpk") 

# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("Quarter", "ElectionPeriod", "pres_party", "error.prop.deflator.q2", 
          "time_to_election", "recession", "senate_dem_rep", 
          "house_dem_rep", "DebtGDP", "ExpenditureGDP", "UNRATE",
          "PotentialGDP", "GlobalModel", "FedFunds", "FedFunds2qChange", "DiscountRate",
          "DiscountRate2qChange", "Chair"
)  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

cpi.complete <- subset(cpi.complete, !(time_to_election %in% c(14, 15)))

cpi.data2 <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

cpi.data2$elect2 <- (cpi.data2$time_to_election)^2

#### Run regressions dropping each president ####
# Remove spaces in president's names
cpi.data2$president <- str_replace_all(cpi.data2$president, " ", "")
cpi.data2$elect2 <- factor(cpi.data2$president)

# Create funciton and run regressions
PresNames <- c("Nixon", "Ford", "Carter", "Reagan", "GHWBush", "Clinton", "GWBush")

SubSetPres <- function(x){
            SubData <- subset(cpi.data2, president != x)
            SData <- paste("S", x, sep = "")
            assign(SData, summary(zelig(error.prop.deflator.q2 ~ pres_party + 
                                        time_to_election + recession + senate_dem_rep +
                                        house_dem_rep + DebtGDP + ExpenditureGDP + 
                                        PotentialGDP + DiscountRate2qChange + GlobalModel +
                                        UNRATE, model = "ls", data = SubData, 
                                        cite = FALSE)), 
                   env=.GlobalEnv)
}

lapply(PresNames, SubSetPres)

#### Run regressions dropping each presidential term ####
# Remove spaces in president term names
cpi.data2$presTerm<- str_replace_all(cpi.data2$presTerm, " ", "")
cpi.data2$elect2 <- factor(cpi.data2$presTerm)

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

#### President Dummy Variables ####
MPresDummies <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel + UNRATE + president, model = "ls", data = cpi.data2, cite = FALSE)

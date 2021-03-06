###############
# GreenBook Misc. Alternative Model Specifications
# Christopher Gandrud
# 22 November 2012
###############

## Load libraries
library(Zelig)

# To run as a stand alone file. First, run the following files from earlier in the paper:
## devtools::source_url("http://bit.ly/NXdCpk") 

cpi.data2 <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

cpi.data2$elect2 <- (cpi.data2$time_to_election)^2

#### Model including absolute error ####
cpi.data2$error.deflator.q2 <-  cpi.data2$GB_CPI_QTR2 - cpi.data2$deflator

## Proportion error Response, i.e. the basic model
A1.1 <-zelig(error.prop.deflator.q2 ~ pres_party + 
                                    time_to_election + recession + senate_dem_rep +
                                    house_dem_rep + DebtGDP + ExpenditureGDP + 
                                    PotentialGDP + DiscountRate2qChange + GlobalModel +
                                    UNRATE,
                                    model = "ls", data = cpi.data2, cite = FALSE)

summary(A1.1)

## Absolute error Response
A1.2 <-zelig(error.deflator.q2 ~ pres_party + 
               time_to_election + recession + senate_dem_rep +
               house_dem_rep + DebtGDP + ExpenditureGDP + 
               PotentialGDP + DiscountRate2qChange + GlobalModel +
               UNRATE,
             model = "ls", data = cpi.data2, cite = FALSE)

summary(A1.2) # No change in the main results.

#### Election * Incumbent ReElection Bid ####
cpi.data2$EligibleReElect[cpi.data2$term == 1] <- 1
cpi.data2$EligibleReElect[cpi.data2$term == 2] <- 0

A2 <- zelig(error.prop.deflator.q2 ~ elect2*EligibleReElect, model = "ls", 
                                     data = cpi.data2, cite = FALSE)

summary(A2)

A3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod*EligibleReElect, model = "ls", 
            data = cpi.data2, cite = FALSE)

summary(A3)

A4 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod*EligibleReElect, model = "ls", 
            data = cpi.data2, cite = FALSE)

summary(A4)

A5 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod*EligibleReElect + pres_party + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel + UNRATE + error.deflator.q2, model = "ls", data = cpi.data2, cite = FALSE)

summary(A5)

A6 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + EligibleReElect*pres_party + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel + UNRATE + error.deflator.q2, model = "ls", data = cpi.data2, cite = FALSE)

summary(A6)

A7 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod*EligibleReElect*pres_party + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel + UNRATE + error.deflator.q2, model = "ls", data = cpi.data2, cite = FALSE)

summary(A7)
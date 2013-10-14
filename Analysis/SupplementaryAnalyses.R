###############
# Supplemental Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 13 October 2013
###############

## Load libraries
# library(Zelig)
# library(digest)
library(DataCombine)

# To run as a stand alone file. First, run the following files from earlier in the paper:
# devtools::source_url("http://bit.ly/NXdCpk") 

cpi.data2S <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

# Create inflation lag variable
cpi.data2S <- slide(cpi.data2S, Var = "deflator", NewVar = "deflatorLag3", slideBy = -3)

###### Models #####
#### Standardised Infation Error ####
# Pres. ID*Linear Time to Election
S1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Present inflation
S2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflator, model = "normal", data = cpi.data2S, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S3 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflatorLag3, model = "normal", data = cpi.data2S, cite = FALSE)

# Oil Shocks
S4 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price, model = "normal", data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S5 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + num_conflicts, model = "normal", data = cpi.data2S, cite = FALSE)

# Productivity change
S6 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S7 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + num_conflicts + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

#### Absolute Infation Error ####
cpi.data2S$abs.deflator.q2 <- cpi.data2S$GB_CPI_QTR2/cpi.data2S$deflator

# Vanilla Model
S8 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Pres. ID*Linear Time to Election
S9 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Present inflation
S10 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflator, model = "normal", data = cpi.data2S, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S11 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflatorLag3, model = "normal", data = cpi.data2S, cite = FALSE)

# Oil Shocks
S12 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price, model = "normal", data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S13 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + num_conflicts, model = "normal", data = cpi.data2S, cite = FALSE)

# Productivity change
S15 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S15 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + num_conflicts + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

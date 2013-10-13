###############
# Supplemental Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 13 October 2013
###############

## Load libraries
library(MatchIt)
library(Zelig)
library(digest)
library(DataCombine)

# To run as a stand alone file. First, run the following files from earlier in the paper:
devtools::source_url("http://bit.ly/NXdCpk") 

# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("Quarter", "ElectionPeriod4", "pres_party", "error.prop.deflator.q2", 
          "time_to_election", "recession", "senate_dem_rep", "cpi_change",
          "house_dem_rep", "DebtGDP", "DeficitGDP", "ExpenditureGDP", "UNRATE",
          "PotentialGDP", "GlobalModel", "FedFunds", "FedFunds2qChange", "DiscountRate",
          "DiscountRate2qChange", "Chair", "productivity_change", "WTI_crude_price", "num_conflicts"
)  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

# Remove quarters when president party ID would be unknown for 2 quarter forecasting (time_to_election = 15, 14)

cpi.complete <- subset(cpi.complete, !(time_to_election %in% c(14, 15)))

cpi.data2 <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

# Create inflation lag variable
cpi.data2 <- slide(cpi.data2, Var = "deflator", NewVar = "deflatorLag3", slideBy = -3)


###### Models #####

# Pres. ID*Linear Time to Election
S1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*time_to_election + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2, cite = FALSE)

# Present inflation
S2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + deflator, model = "normal", data = cpi.data2, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S3 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + deflatorLag3, model = "normal", data = cpi.data2, cite = FALSE)

# Oil Shocks
S4 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + WTI_crude_price, model = "normal", data = cpi.data2, cite = FALSE)

# Number of interstate conflicts
S5 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + num_conflicts, model = "normal", data = cpi.data2, cite = FALSE)

# Productivity change
S6 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + productivity_change, model = "normal", data = cpi.data2, cite = FALSE)

# Garbage can shocks
S7 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + senate_dem_rep + house_dem_rep + WTI_crude_price + num_conflicts + productivity_change, model = "normal", data = cpi.data2, cite = FALSE)

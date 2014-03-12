###############
# Supplemental Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 12 March 2014
###############

## Load libraries
# library(Zelig)
library(DataCombine)

# To run as a stand alone file. First, run the following files from earlier in the paper:
# devtools::source_url("http://bit.ly/NXdCpk") 

# Create mid-term + presidential elections variable
cpi.data$time_to_election_midterm <- cpi.data$time_to_election

for (i in 1:nrow(cpi.data)){
  if (cpi.data[i, "time_to_election_midterm"] > 7){
    cpi.data[i, "time_to_election_midterm"]<- cpi.data[i, "time_to_election_midterm"] - 8
  }else{
    cpi.data[i, "time_to_election_midterm"]<- cpi.data[i, "time_to_election_midterm"]
  }
}

# Subset for mid-term and presidential elections
cpi.data2SMidterm <- subset(cpi.data, !(time_to_election_midterm %in% c(6, 7)))

# Subset for presidential elections only
cpi.data2S <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

# Create inflation lag variable
cpi.data2S <- slide(cpi.data2S, Var = "deflator", NewVar = "deflatorLag3", slideBy = -3)

###### Models #####

# ------------------------------------------------------------------------------------ #
#### Midterms ####
SM1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election_midterm + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2SMidterm, cite = FALSE)

SM2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*time_to_election_midterm + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2SMidterm, cite = FALSE)

# ------------------------------------------------------------------------------------ #
#### Standardised Infation Error ####
# Pres. ID*Linear Time to Election
S1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Present inflation
S2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflator, model = "normal", data = cpi.data2S, cite = FALSE)

S2.2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*deflator + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S3 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + deflatorLag3, model = "normal", data = cpi.data2S, cite = FALSE)

S3.2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*deflatorLag3 + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Oil Shocks
S4 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price, model = "normal", data = cpi.data2S, cite = FALSE)

S4.2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*WTI_crude_price + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Productivity change
S5 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

S5.2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*productivity_change + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S6 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

S6.2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*num_conflicts + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S7 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + num_conflicts + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

# ------------------------------------------------------------------------------------ #
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

S12.2 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*WTI_crude_price + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Productivity change
S13 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + productivity_change, model = "normal", data = cpi.data2S, cite = FALSE)

S13.2 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*productivity_change + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S14 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + num_conflicts, model = "normal", data = cpi.data2S, cite = FALSE)

S14.2 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*num_conflicts + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S15 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election + GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + productivity_change + num_conflicts, model = "normal", data = cpi.data2S, cite = FALSE)

# ------------------------------------------------------------------------------------ #
#### Partisan composition of the FOMC
# Proportion of FOMC members per quarter appointed during democratic presidency, when forcasts were made 
S16 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + GlobalModel + senate_dem_rep + house_dem_rep + DemAppointPerc_Lag3, model = "normal", data = cpi.data2S, cite = FALSE)

S17 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*DemAppointPerc_Lag3 + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

# Proportion of FOMC members per quarter appointed during democratic presidency, when forcasts were made 
S18 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + GlobalModel + senate_dem_rep + house_dem_rep + DemAppointPerc_Lag3, model = "normal", data = cpi.data2S, cite = FALSE)

S19 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + pres_party*DemAppointPerc_Lag3 + GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", data = cpi.data2S, cite = FALSE)

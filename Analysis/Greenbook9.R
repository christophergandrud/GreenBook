###############
# Supplemental Analyses for GreenBook Forecast Error Paper
# Models suggested by PSRM Reviewers
# Christopher Gandrud 
# 7 May 2014
###############

## Load libraries
library(Zelig)
library(DataCombine)

# To run as a stand alone file. First, run the following files from earlier in the paper:
## source('Analysis/Greenbook1.R')  

# Create mid-term + presidential elections variable
cpi.data$time_to_election_midterm <- cpi.data$time_to_election

for (i in 1:nrow(cpi.data)){
  if (cpi.data[i, "time_to_election_midterm"] > 7){
    cpi.data[i, "time_to_election_midterm"]<- 
        cpi.data[i, "time_to_election_midterm"] - 8
  }else{
    cpi.data[i, "time_to_election_midterm"]<- 
        cpi.data[i, "time_to_election_midterm"]
  }
}

# Subset for mid-term and presidential elections
cpi.data2SMidterm <- subset(cpi.data, !(time_to_election_midterm %in% c(6, 7)))

# Subset for presidential elections only
cpi.data2S <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

# Create inflation lag variable
cpi.data2S <- slide(cpi.data2S, Var = "deflator", NewVar = "deflatorLag3", 
                    slideBy = -3)

###### Models #####

# ---------------------------------------------------------------------------- #
#### Midterms ####
SM1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + 
            PotentialGDP + DiscountRate2qChange + UNRATE + pres_party + time_to_election_midterm + GlobalModel + senate_dem_rep + 
                house_dem_rep, model = "normal", data = cpi.data2SMidterm, 
                cite = FALSE)

SM2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + 
            PotentialGDP + DiscountRate2qChange + UNRATE + 
            pres_party*time_to_election_midterm + GlobalModel + senate_dem_rep + 
            house_dem_rep, model = "normal", data = cpi.data2SMidterm, 
            cite = FALSE)

# ---------------------------------------------------------------------------- #
#### Standardised Infation Error ####
# Pres. ID*Linear Time to Election
S1 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*time_to_election, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Present inflation
S2 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + time_to_election + deflator, 
            model = "normal", data = cpi.data2S, cite = FALSE)

S3 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*deflator, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S4 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + deflatorLag3, model = "normal", 
            data = cpi.data2S, cite = FALSE)

S5 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*deflatorLag3, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Oil Shocks
S6 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + WTI_crude_price, model = "normal", 
            data = cpi.data2S, cite = FALSE)

S7 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*WTI_crude_price, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Productivity change
S8 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + productivity_change, model = "normal", 
            data = cpi.data2S, cite = FALSE)

S9 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP +
            DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*productivity_change, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S10 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP 
            + DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + num_conflicts, model = "normal", 
            data = cpi.data2S, cite = FALSE)

S11 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP 
            + DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*num_conflicts, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S12 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP 
            + DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + 
            num_conflicts + productivity_change, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# ---------------------------------------------------------------------------- #
#### Absolute Infation Error ####
cpi.data2S$abs.deflator.q2 <- cpi.data2S$GB_CPI_QTR2/cpi.data2S$deflator

# Vanilla Model
S13 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Pres. ID*Linear Time to Election
S14 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party*time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# Present inflation
S15 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + deflator, 
            model = "normal", data = cpi.data2S, cite = FALSE)

# Inflation in the quarter before the forecast quarter
S16 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + deflatorLag3, 
            model = "normal", data = cpi.data2S, cite = FALSE)

# Oil Shocks
S17 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price, 
            model = "normal", data = cpi.data2S, cite = FALSE)

# Productivity change
S18 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + productivity_change, 
            model = "normal", data = cpi.data2S, cite = FALSE)

# Number of interstate conflicts
S19 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + num_conflicts, 
            model = "normal", data = cpi.data2S, cite = FALSE)

# Garbage can shocks
S20 <- zelig(abs.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP + 
            DiscountRate2qChange + UNRATE + pres_party + time_to_election + 
            GlobalModel + senate_dem_rep + house_dem_rep + WTI_crude_price + productivity_change + num_conflicts, model = "normal", 
            data = cpi.data2S, cite = FALSE)

# ---------------------------------------------------------------------------- #
#### Partisan composition of the Board of Governors
# Proportion of Board of Governors members per quarter appointed during 
# democratic presidency, when forcasts were made 
S21 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP 
            + DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party + DemAppointPerc_Lag3, model = "normal", 
            data = cpi.data2S, cite = FALSE)

S22 <- zelig(error.prop.deflator.q2 ~ recession + ExpenditureGDP + PotentialGDP 
            + DiscountRate2qChange + UNRATE + GlobalModel + senate_dem_rep + 
            house_dem_rep + pres_party*DemAppointPerc_Lag3, model = "normal", 
            data = cpi.data2S, cite = FALSE)

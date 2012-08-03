###############
# Main Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 3 August 2012
###############

library(devtools)
library(MatchIt)
library(Zelig)
library(stats)

# To run as a stand alone file. First, run the following files from earlier in the paper:
## source_url("http://bit.ly/NXdCpk") 
## source_url("http://bit.ly/Nehu34")

# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("Quarter", "ElectionPeriod", "pres_party", "error.prop.deflator.q2", "time_to_election",
          "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP",
          "PotentialGDP", "GlobalModel", "FedFunds", "FedFunds2qChange", "DiscountRate",
          "DiscountRate2qChange", "Chair"
          )  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

#### Matching Model ####

# Elections
cpi.matched.election <- matchit(ElectionPeriod ~ recession + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel + DiscountRate2qChange, data = cpi.complete, method = "genetic", pop.size = 161)

# Party 
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel + DiscountRate2qChange, data = cpi.complete, method = "genetic", pop.size = 161)

# Diagnostics for Covariate Balance
# summary(cpi.matched.election)
# plot(cpi.matched.election)

# summary(cpi.matched.party)
# plot(cpi.matched.party)

# Turn matched data into data.frame for analysis
cpi.Mdf.election <- match.data(cpi.matched.election)

cpi.Mdf.party <- match.data(cpi.matched.party)

################### Parametric Models ################

###### Non-matched (N) ######
# Least Squares

NL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.data, cite = FALSE)

NL8 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.data, cite = FALSE)

NL9 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + time_to_election + Chair + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL10 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.data, cite = FALSE)

NL13 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.data, cite = FALSE)

###### Matched based with the ElectionPeriod as the treatment variable (E) ######

# Least Squares

EL1 <- zelig(error.prop.deflator.q2 ~ DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL4 <- zelig(error.prop.deflator.q2 ~ pres_party + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL7 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL9 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + time_to_election + senate_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.election, cite = FALSE)

EL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.election, cite = FALSE)

###### Matched based with the pres_party as the treatment variable (P) ######

# Least Squares

PL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL8 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL9 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.party, cite = FALSE)

##### Normal Bayes, Not Matched (NB) #####
NB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "normal.bayes", data = cpi.data, cite = FALSE)

#### Normal Bayes, Matched (MP) ####
PB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "normal.bayes", data = cpi.Mdf.party, cite = FALSE)

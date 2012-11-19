###############
# Main Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 19 November 2012
###############

## Load libraries
# library(devtools)
 library(MatchIt)
 library(Zelig)
 library(stats)
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

# Remove quarters when president party ID would be unknown for 2 quarter forecasting (time_to_election = 15, 14)

cpi.complete <- subset(cpi.complete, !(time_to_election %in% c(14, 15)))

cpi.data2 <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

#### Matching Model ####

# Elections, No interactions
# cpi.matched.election <- matchit(ElectionPeriod ~ pres_party + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel + DiscountRate2qChange + UNRATE, data = cpi.complete, method = "genetic", pop.size = 161)

# Party, All Interactions
# cpi.matched.party.all <- matchit(pres_party ~ recession + time_to_election + ElectionPeriod + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + UNRATE + GlobalModel + DiscountRate2qChange + pres_party*ElectionPeriod + pres_party*senate_dem_rep + pres_party*house_dem_rep + senate_dem_rep + house_dem_rep, data = cpi.complete, method = "genetic", pop.size = 161)

# Party, Only pres*ElectionPeriod Interaction
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + ElectionPeriod + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel + DiscountRate2qChange + UNRATE + pres_party*ElectionPeriod, data = cpi.complete, method = "genetic", pop.size = 161)

#### Diagnostics for Covariate Balance ####
# summary(cpi.matched.election)
# plot(cpi.matched.election, type = "QQ")
# plot(cpi.matched.election, type = "jitter")

# summary(cpi.matched.party.all, interactions = TRUE)
# plot(cpi.matched.party.all, type = "QQ")
# plot(cpi.matched.party.all, type = "jitter")

# summary(cpi.matched.party, interactions = TRUE)
# plot(cpi.matched.party, type = "QQ", interactive = FALSE)
# plot(cpi.matched.party, type = "jitter", interactive = FALSE)

# Turn matched data into data.frame for analysis
# cpi.Mdf.election <- match.data(cpi.matched.election)

cpi.Mdf.party <- match.data(cpi.matched.party)

#### Create Time To Election Squared Variable ####
cpi.data2$elect2 <- (cpi.data2$time_to_election)^2

# cpi.Mdf.election$elect2 <- (cpi.Mdf.election$time_to_election)^2

cpi.Mdf.party$elect2 <- (cpi.Mdf.party$time_to_election)^2

################### Parametric Models ################

###### Non-matched (N) ######
# Least Squares

NL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL8 <- zelig(error.prop.deflator.q2 ~ pres_party*elect2 + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = cpi.data2, cite = FALSE)

NL9 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + time_to_election + Chair + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL10 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.data2, cite = FALSE)

NL13 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.data2, cite = FALSE)

###### Matched based with the ElectionPeriod as the treatment variable (E) ######

# Least Squares

# EL1 <- zelig(error.prop.deflator.q2 ~ DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL3 <- zelig(error.prop.deflator.q2 ~ time_to_election, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL4 <- zelig(error.prop.deflator.q2 ~ pres_party + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL5 <- zelig(error.prop.deflator.q2 ~ pres_party, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL8 <- zelig(error.prop.deflator.q2 ~ pres_party*elect2 + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL9 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + time_to_election + senate_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.election, cite = FALSE)
# 
# EL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.election, cite = FALSE)

###### Matched based with the pres_party as the treatment variable (P) ######

# Least Squares

PL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL8 <- zelig(error.prop.deflator.q2 ~ pres_party*elect2 + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL9 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE, model = "ls", data = cpi.Mdf.party, cite = FALSE)

PL12 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.party, cite = FALSE)

##### Normal Bayes, Not Matched (NB) #####
NB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "normal.bayes", data = cpi.data, cite = FALSE)

#### Normal Bayes, Matched (MP) ####
PB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "normal.bayes", data = cpi.Mdf.party, cite = FALSE)

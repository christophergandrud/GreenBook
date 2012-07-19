###############
# Main Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 18 July 2012
###############


library(MatchIt)
library(Zelig)
library(stats)


# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("ElectionPeriod", "pres_party", "error.prop.deflator.q2", "time_to_election", "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP", "PotentialGDP", "GlobalModel")  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

#### Matching Model ####

# Elections
cpi.matched.election <- matchit(ElectionPeriod ~ recession + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel, data = cpi.complete, method = "genetic")

# Party 
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel, data = cpi.complete, method = "genetic")

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

NL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + GlobalModel, model = "ls", data = cpi.data)

NL8 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL9 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

NL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.data)

###### Matched based with the ElectionPeriod as the treatment variable (E) ######

# Least Squares

EL1 <- zelig(error.prop.deflator.q2 ~ DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL4 <- zelig(error.prop.deflator.q2 ~ pres_party + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + GlobalModel, model = "ls", data = cpi.Mdf.election)

EL8 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + time_to_election + senate_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL9 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + time_to_election + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

EL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.election)

###### Matched based with the pres_party as the treatment variable (P) ######

# Least Squares

PL1 <- zelig(error.prop.deflator.q2 ~ recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL2 <- zelig(error.prop.deflator.q2 ~ time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL3 <- zelig(error.prop.deflator.q2 ~ ElectionPeriod + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL4 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL5 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL6 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL7 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + GlobalModel, model = "ls", data = cpi.Mdf.party)

PL8 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + DebtGDP + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL9 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL10 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep + recession + DebtGDP + time_to_election + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)

PL11 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep*house_dem_rep, model = "ls", data = cpi.Mdf.party)

##### Normal Bayes, Not Matched (NB) #####

# Normal Bayes

NB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + GlobalModel, model = "normal.bayes", data = cpi.data)

#### Normal Bayes, Matched (MP) ####

# Normal Bayes

PB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + GlobalModel, model = "normal.bayes", data = cpi.Mdf.party)

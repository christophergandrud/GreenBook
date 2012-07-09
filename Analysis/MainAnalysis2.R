###############
# Main Analyses for GreenBook Forecast Error Paper
# Christopher Gandrud 
# 9 July 2012
###############


library(MatchIt)
library(Zelig)
library(stats)


# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("ElectionPeriod", "pres_party", "error.prop.deflator.q2", "time_to_election", "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP", "PotentialGDP")  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

#### Matching Model ####

# Party 
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

# Elections
cpi.matched.election <- matchit(ElectionPeriod ~ recession + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

# Diagnostics for Covariate Balance
# summary(cpi.matched.party)
# plot(cpi.matched.party)

# summary(cpi.matched.election)
# plot(cpi.matched.election)

# Turn matched data into data.frame for analysis
cpi.Mdf.election <- match.data(cpi.matched.election)

cpi.Mdf.party <- match.data(cpi.matched.party)

################### Models ################


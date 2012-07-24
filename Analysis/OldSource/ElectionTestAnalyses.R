cpi.Mdf.election <- match.data(cpi.matched.election)

#### Non-Matched ####

NPL1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + ElectionPeriod + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

summary(NPL1)

# Interaction of pres_party and ElectionPeriod #

# 2 quarter election dummy 
NPI1 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

summary(NPI1)

# Continuous countdown to election 
NPI2 <- zelig(error.prop.deflator.q2 ~ pres_party*time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)

summary(NPI2)

# Continuous countdown to election 
NBI2 <- zelig(error.prop.deflator.q2 ~ pres_party*time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.data)

summary(NBI2)

#### Matched ####
# recession is dropped because there is no variation in the matched data, i.e. there were no recessions during election periods

MPL1 <- zelig(error.prop.deflator.q2 ~ pres_party + ElectionPeriod + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

summary(MPL1)

MB1 <- zelig(error.prop.deflator.q2 ~ pres_party +  ElectionPeriod + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.election)

summary(MB1)

# Interaction of pres_party and ElectionPeriod #

# 2 quarter election dummy 
MPI1 <- zelig(error.prop.deflator.q2 ~ pres_party*ElectionPeriod + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)
N
summary(MPI1)

# Continuous countdown to election 
MPI2 <- zelig(error.prop.deflator.q2 ~ pres_party*time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.election)

summary(MPI2)

# Continuous countdown to election 
MBI2 <- zelig(error.prop.deflator.q2 ~ pres_party*time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.election)

summary(MBI2)
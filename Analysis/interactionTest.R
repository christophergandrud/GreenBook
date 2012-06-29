################
# President*Congress Interaction Graphs
# Christopher Gandrud
# Updated 15 
################

#### Models ####

######### Senate Interaction with President ##########

MPB2 <- zelig(error.prop.deflator.q2 ~ pres_party*senate_dem_rep + recession + time_to_election + house_dem_rep + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.party)

# summary(MPB2)


#### Simulations ####
senate_dem_rep.r <- seq(0.8, 2.125, by = .01)

# Set Fitted Values 
M2.partySetDem <- setx(MPB2, pres_party = 1, senate_dem_rep = senate_dem_rep.r)

M2.partySetRep <- setx(MPB2, pres_party = 0,senate_dem_rep = senate_dem_rep.r )

# Simulate quantities of interest
M2.partySim <- sim(MPB2, x = M2.partySetRep, x1 = M2.partySetDem)



######### House Interaction with President ##########


MPB3 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep + recession + time_to_election + senate_dem_rep + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.party)

# MPB4 <- zelig(error.prop.deflator.q2 ~ pres_party*house_dem_rep*senate_dem_rep + recession + time_to_election + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.party)

# summary(MPB3)


#### Simulations ####
house_dem_rep.r <- seq(0.8, 2.125, by = .01)

# Set Fitted Values 
M3.partySetDem <- setx(MPB3, pres_party = 1, house_dem_rep = house_dem_rep.r)

M3.partySetRep <- setx(MPB3, pres_party = 0,house_dem_rep = house_dem_rep.r )

# Simulate quantities of interest
M3.partySim <- sim(MPB3, x = M3.partySetRep, x1 = M3.partySetDem)

############################################

#### Zelig Plots ####
par(mfrow = c(1, 2))
house.inter <- plot.ci(M3.partySim, main = "House", xlab = "Dem to Rep Ratio", ylab = "Expected Inflation Forecast Error", ylim = c(-0.8, 0.6))
senate.inter <- plot.ci(M2.partySim, main = "Senate", xlab = "Dem to Rep Ratio", ylab = "", ylim = c(-0.8, 0.6))

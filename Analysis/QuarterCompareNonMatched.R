###############
# Graph of simulated errors across all quarter estimates for model NL7 (A7 in the manuscript table). 
# Uses non-matched data.
# Christopher Gandrud 
# 22 November 2012
###############

## Load libraries
# library(devtools)
library(Zelig)
library(plyr)

# To run as a stand alone file. First, run the following files from the paper:
## source_url("http://bit.ly/NXdCpk") 

#### Run two matching models ####
# One model is for estimates made 0 through 2 quarters before a given quarter. There is full data for these estimates.
# Another model is for estimates made 3 to 5 quarters before a given quarter. There is missing data for these estimates early in the observation period.

## Subset for complete (nonmissing) values ##
## Quarter 0 through 2
# matchit requires data sets to have no missing values
vars <- c("Quarter", "ElectionPeriod", "pres_party", "error.prop.deflator.q0", 
          "error.prop.deflator.q1", "error.prop.deflator.q2", "time_to_election", 
          "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP",
          "PotentialGDP", "UNRATE", "GlobalModel", "FedFunds", "FedFunds2qChange",
          "DiscountRate1qChange", "DiscountRate2qChange", "Chair"
)  
CPIEstimates02 <- cpi.data[complete.cases(cpi.data[vars]),]
CPIEstimates02 <- CPIEstimates02[vars]

## Quarter 3 through 5
vars <- c("Quarter", "ElectionPeriod", "pres_party", "error.prop.deflator.q3", 
          "error.prop.deflator.q4", "error.prop.deflator.q5", "time_to_election", 
          "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP",
          "PotentialGDP", "GlobalModel", "UNRATE", "FedFunds", "FedFunds2qChange", 
          "DiscountRate3qChange", "DiscountRate4qChange", "DiscountRate5qChange", "Chair"
)  
CPIEstimates35 <- cpi.data[complete.cases(cpi.data[vars]),]
CPIEstimates35 <- CPIEstimates35[vars]

#### Run Parametric OLS Models ####
NL.02.0 <- zelig(error.prop.deflator.q0 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP  + UNRATE + GlobalModel, model = "ls", data = CPIEstimates02, cite = FALSE)

NL.02.1 <- zelig(error.prop.deflator.q1 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate1qChange + UNRATE + GlobalModel, model = "ls", data = subset(CPIEstimates02, time_to_election != 15), cite = FALSE)

NL.02.2 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + UNRATE + GlobalModel, model = "ls", data = subset(CPIEstimates02, !(time_to_election %in% c(15, 14))), cite = FALSE)

NL.35.3 <- zelig(error.prop.deflator.q3 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate3qChange + UNRATE + GlobalModel, model = "ls", data = CPIEstimates35, cite = FALSE)

NL.35.4 <- zelig(error.prop.deflator.q4 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate4qChange + UNRATE + GlobalModel, model = "ls", data = subset(CPIEstimates35 , !(time_to_election %in% c(15, 14, 13, 12))), cite = FALSE)

NL.35.5 <- zelig(error.prop.deflator.q5 ~ pres_party + time_to_election + recession + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate5qChange + UNRATE + GlobalModel, model = "ls", data = subset(CPIEstimates35 , !(time_to_election %in% c(15, 14, 13, 12, 11))), cite = FALSE)

#### Simulate Expected Values & Melt ####
# Ranges of fitted values 
pres_party.r <- c(0, 1)

## Quarter 0 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty0 <- setx(NL.02.0, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim0 <- sim(NL.02.0, x = ModelParty0)

# Extract expected values from simulations
ModelParty.ev0 <- ModelParty.sim0$qi
ModelParty.ev0 <-data.frame(ModelParty.ev0$ev)
names(ModelParty.ev0) <- c("Rep", "Dem")
ModelParty.ev0 <- melt(ModelParty.ev0, measure = 1:2)
ModelParty.ev0$variable <- factor(ModelParty.ev0$variable) 
ModelParty.ev0$QrtEstimate <- 0

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer0 <- ddply(ModelParty.ev0, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer0 <- ddply(ModelParty.evPer0, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer0 <- subset(ModelParty.evPer0, Lower == FALSE & Upper == FALSE)

## Quarter 1 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty1 <- setx(NL.02.1, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim1 <- sim(NL.02.1, x = ModelParty1)

# Extract expected values from simulations
ModelParty.ev1 <- ModelParty.sim1$qi
ModelParty.ev1 <-data.frame(ModelParty.ev1$ev)
names(ModelParty.ev1) <- c("Rep", "Dem")
ModelParty.ev1 <- melt(ModelParty.ev1, measure = 1:2)
ModelParty.ev1$variable <- factor(ModelParty.ev1$variable) 
ModelParty.ev1$QrtEstimate <- 1

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer1 <- ddply(ModelParty.ev1, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer1 <- ddply(ModelParty.evPer1, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer1 <- subset(ModelParty.evPer1, Lower == FALSE & Upper == FALSE)

## Quarter 2 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty2 <- setx(NL.02.2, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim2 <- sim(NL.02.2, x = ModelParty2)

# Extract expected values from simulations
ModelParty.ev2 <- ModelParty.sim2$qi
ModelParty.ev2 <-data.frame(ModelParty.ev2$ev)
names(ModelParty.ev2) <- c("Rep", "Dem")
ModelParty.ev2 <- melt(ModelParty.ev2, measure = 1:2)
ModelParty.ev2$variable <- factor(ModelParty.ev2$variable) 
ModelParty.ev2$QrtEstimate <- 2

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer2 <- ddply(ModelParty.ev2, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer2 <- ddply(ModelParty.evPer2, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer2 <- subset(ModelParty.evPer2, Lower == FALSE & Upper == FALSE)

## Save estimates to be used in the in-text equations
write.csv(ModelParty.evPer2, "cache/SimQrt2.csv")

## Quarter 3 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty3 <- setx(NL.35.3, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim3 <- sim(NL.35.3, x = ModelParty3)

# Extract expected values from simulations
ModelParty.ev3 <- ModelParty.sim3$qi
ModelParty.ev3 <-data.frame(ModelParty.ev3$ev)
names(ModelParty.ev3) <- c("Rep", "Dem")
ModelParty.ev3 <- melt(ModelParty.ev3, measure = 1:2)
ModelParty.ev3$variable <- factor(ModelParty.ev3$variable) 
ModelParty.ev3$QrtEstimate <- 3

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer3 <- ddply(ModelParty.ev3, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer3 <- ddply(ModelParty.evPer3, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer3 <- subset(ModelParty.evPer3, Lower == FALSE & Upper == FALSE)

## Quarter 4 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty4 <- setx(NL.35.4, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim4 <- sim(NL.35.4, x = ModelParty4)

# Extract expected values from simulations
ModelParty.ev4 <- ModelParty.sim4$qi
ModelParty.ev4 <-data.frame(ModelParty.ev4$ev)
names(ModelParty.ev4) <- c("Rep", "Dem")
ModelParty.ev4 <- melt(ModelParty.ev4, measure = 1:2)
ModelParty.ev4$variable <- factor(ModelParty.ev4$variable) 
ModelParty.ev4$QrtEstimate <- 4

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer4 <- ddply(ModelParty.ev4, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer4 <- ddply(ModelParty.evPer4, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer4 <- subset(ModelParty.evPer4, Lower == FALSE & Upper == FALSE)

## Quarter 5 ##
# Set fitted values, all variables other than pres_party set to their means
ModelParty5 <- setx(NL.35.5, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim5 <- sim(NL.35.5, x = ModelParty5)

# Extract expected values from simulations
ModelParty.ev5 <- ModelParty.sim5$qi
ModelParty.ev5 <-data.frame(ModelParty.ev5$ev)
names(ModelParty.ev5) <- c("Rep", "Dem")
ModelParty.ev5 <- melt(ModelParty.ev5, measure = 1:2)
ModelParty.ev5$variable <- factor(ModelParty.ev5$variable) 
ModelParty.ev5$QrtEstimate <- 5

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer5 <- ddply(ModelParty.ev5, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer5 <- ddply(ModelParty.evPer5, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer5 <- subset(ModelParty.evPer5, Lower == FALSE & Upper == FALSE)

ModelPartyAll <- rbind(ModelParty.evPer0, ModelParty.evPer1, 
                       ModelParty.evPer2, ModelParty.evPer3, ModelParty.evPer4, ModelParty.evPer5)

# Create objects recording the number of observations used in each model.
O0 <- nrow(CPIEstimates02)
O1 <- nrow(subset(CPIEstimates02, time_to_election != 15))
O2 <- nrow(subset(CPIEstimates02, !(time_to_election %in% c(15, 14))))
O3 <- nrow(CPIEstimates35)
O4 <- nrow(subset(CPIEstimates35 , !(time_to_election %in% c(15, 14, 13, 12))))
O5 <- nrow(subset(CPIEstimates35 , !(time_to_election %in% c(15, 14, 13, 12, 11))))

#### Plot expected values ####
# Partisan colours, initially run in ErrorPresPartyGraph.R
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

# Create plot
ModelPartyPlotAll <- ggplot(data = ModelPartyAll, aes(QrtEstimate, value)) +
  geom_hline(yintercept = 0, size = 1,
             alpha = I(0.5)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = variable), colour = "grey70") +
  geom_point(aes(colour = variable), alpha = I(0.05), size = 3) +
  scale_color_manual(values = partisan.colors, 
                     name = "") +
  scale_x_reverse() +
  scale_y_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.35), 
                     labels = c(-0.5, -0.25, 0, 0.25, "N =")) +
  xlab("\n Age of Forecast in Quarters") +
  ylab("Expected Standardized Forecast Error \n") +
  annotate(geom = "text", x = 5, y = 0.37,
           label = O5, size = 4) +
  annotate(geom = "text", x = 4, y = 0.37,
           label = O4, size = 4) +
  annotate(geom = "text", x = 3, y = 0.37, 
           label = O3, size = 4) +
  annotate(geom = "text", x = 2, y = 0.37, 
           label = O2, size = 4) +
  annotate(geom = "text", x = 1, y = 0.37, 
           label = O1, size = 4) +
  annotate(geom = "text", x = 0, y = 0.37, 
           label = O0, size = 4) +
  guides(colour = guide_legend(override.aes = list(alpha = 1), reverse = TRUE)) +
  theme_bw(base_size = 12)    

print(ModelPartyPlotAll)
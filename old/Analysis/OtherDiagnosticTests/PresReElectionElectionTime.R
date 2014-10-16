###############
# GreenBook Misc. Election Timing, Party, & Incumbency
# Christopher Gandrud
# 19 November 2012
###############

## Load libraries
# library(devtools)
library(Zelig)
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

cpi.complete <- subset(cpi.complete, !(time_to_election %in% c(14, 15)))

cpi.data2 <- subset(cpi.data, !(time_to_election %in% c(14, 15)))

cpi.data2$elect2 <- (cpi.data2$time_to_election)^2

#### Election * Incumbent ReElection Bid ####
cpi.data2$EligibleReElect[cpi.data2$term == 1] <- 1
cpi.data2$EligibleReElect[cpi.data2$term == 2] <- 0

E1 <- zelig(error.prop.deflator.q2 ~ elect2*EligibleReElect*pres_party, model = "ls", data = cpi.data2, cite = FALSE)

#### Organize Data for Graphing ####
# Create range of values to simulate expected values across
pres_party.r <- c(0, 1)
tte.r <- 1 # Errors made for the quarter before the election

# Set fitted values 
E1SetRe <- setx(E1, pres_party = pres_party.r, time_to_election = tte.r, EligibleReElect = 1)
E1SetNotRe <- setx(E1, pres_party = pres_party.r, time_to_election = tte.r, EligibleReElect =0)

# Simulate expected values.
E1SimRe <- sim(E1, x = E1SetRe)
E1SimNotRe <- sim(E1, x = E1SetNotRe)

# Extract expected values from simulations (Re)
E1SimRe.ev <- E1SimRe$qi
E1SimRe.ev <-data.frame(E1SimRe.ev$ev)
names(E1SimRe.ev) <- c("Rep, Pres.", "Dem. Pres.")
E1SimRe.ev <- melt(E1SimRe.ev, measure = 1:2)

# Extract expected values from simulations (NotRe)
E1SimNotRe.ev <- E1SimNotRe$qi
E1SimNotRe.ev <-data.frame(E1SimNotRe.ev$ev)
names(E1SimNotRe.ev) <- c("Rep, Pres.", "Dem. Pres.")
E1SimNotRe.ev <- melt(E1SimNotRe.ev, measure = 1:2)

# Final clean up
E1SimRe.ev$variable <- factor(E1SimRe.ev$variable)
E1SimRe.ev$ReElectable <- "Re"

E1SimNotRe.ev$variable <- factor(E1SimNotRe.ev$variable)
E1SimNotRe.ev$ReElectable <- "NotRe"

# Append both sets of simulation results
E1Bound <- rbind(E1SimNotRe.ev, E1SimRe.ev)

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
E1BoundPer <- ddply(E1Bound, .(variable, ReElectable), transform, Lower = value < quantile(value, c(0.025)))

E1BoundPer <- ddply(E1BoundPer, .(variable, ReElectable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
E1BoundPer <- subset(E1BoundPer, Lower == FALSE & Upper == FALSE)

#### Create plot ####
# Partisan colours
partisan.ReElectable.colours = c("NotRe" = "#E6AB02", "Re" = "#66A61E")

ReElectionPlot <- ggplot(data = E1BoundPer, aes(variable, value)) +
                          geom_hline(aes(intercept= 0), linetype = "dotted") +
                          stat_summary(fun.y = mean, geom = "line", 
                                        aes(group = ReElectable), colour = "grey70") +
                          geom_point(aes(colour = ReElectable), alpha = I(0.1)) +
                          xlab("") + ylab("Expected Standardized Forecast Error\n") +
                          scale_color_manual(values = partisan.ReElectable.colours, 
                                             name = "Re-electable") +
                          guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                          theme_bw(base_size = 12)

print(ReElectionPlot)





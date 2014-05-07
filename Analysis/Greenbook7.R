################
# President*Congress Interaction Graph
# Christopher Gandrud
# 7 May 2014
################

library(Zelig)

# To run as a stand alone file. First, run the following files from earlier in the paper:
## source('Analysis/Greenbook1.R') 
## source('Analysis/Greenbook4.R') 

# Set fitted values 
NL11SetRDem <- setx(NL11, pres_party = 0, house_dem_rep = 1.2, senate_dem_rep = 1.2)
NL11SetDDem <- setx(NL11, pres_party = 1, house_dem_rep = 1.2, senate_dem_rep = 1.2)
NL11SetRRep <- setx(NL11, pres_party = 0, house_dem_rep = 0.8, senate_dem_rep = 0.8)
NL11SetDRep <- setx(NL11, pres_party = 1, house_dem_rep = 0.8, senate_dem_rep = 0.8)

# Simulate expected values.
NL11SimDem <- sim(NL11, x = NL11SetRDem, x1 = NL11SetDDem)
NL11SimRep <- sim(NL11, x = NL11SetRRep, x1 = NL11SetDRep)

# Extract expected values from simulations (Dem)
NL11SimDem.evR = data.frame(simulation.matrix(NL11SimDem, 
                                               "Expected Values: E(Y|X)"))
NL11SimDem.evD = data.frame(simulation.matrix(NL11SimDem, 
                                               "Expected Values: E(Y|X1)"))

NL11SimDem.ev <- cbind(NL11SimDem.evR, NL11SimDem.evD)
names(NL11SimDem.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL11SimDem.ev <- melt(NL11SimDem.ev, measure = 1:2)

# Extract expected values from simulations (Rep)
NL11SimRep.evR = data.frame(simulation.matrix(NL11SimRep, 
                                               "Expected Values: E(Y|X)"))
NL11SimRep.evD = data.frame(simulation.matrix(NL11SimRep, 
                                               "Expected Values: E(Y|X1)"))
NL11SimRep.ev <- cbind(NL11SimRep.evR, NL11SimRep.evD)
names(NL11SimRep.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL11SimRep.ev <- melt(NL11SimRep.ev, measure = 1:2)

# Final clean up
NL11SimDem.ev$variable <- factor(NL11SimDem.ev$variable)
NL11SimDem.ev$Congress <- "Dem"

NL11SimRep.ev$variable <- factor(NL11SimRep.ev$variable)
NL11SimRep.ev$Congress <- "Rep"

# Append both sets of simulation results
NL11Bound <- rbind(NL11SimRep.ev, NL11SimDem.ev)

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
NL11BoundPer <- ddply(NL11Bound, .(variable, Congress), 
                      transform, 
                      Lower = value < quantile(value, c(0.025)))

NL11BoundPer <- ddply(NL11BoundPer, .(variable, Congress), 
                      transform, 
                      Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
NL11BoundPer <- subset(NL11BoundPer, Lower == FALSE & Upper == FALSE)

#### Create plot ####
# Partisan colours
partisan.congress.colours = c("Rep" = "#C42B00", "Dem" = "#2259B3")

PartyInteractionPlot <- ggplot(data = NL11BoundPer, aes(variable, value)) +
                          geom_hline(yintercept = 0, size = 1,
                                      alpha = I(0.5)) +
                          stat_summary(fun.y = mean, geom = "line", 
                                       aes(group = Congress), colour = "grey70") +
                          geom_point(aes(color = Congress), 
                                    alpha = I(0.01), size = 3) +
                          xlab("") + ylab("") +
                          scale_color_manual(values = partisan.congress.colours, 
                            name = "Control\nof\nCongress") +
                          scale_y_continuous(limits = c(-0.5, 0.5), 
                                             breaks = c(-0.25, 0, 0.25),
                                             labels = c(-0.25, 0, 0.25)) +
                          guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                          theme_bw(base_size = 11)
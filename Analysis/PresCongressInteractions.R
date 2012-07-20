################
# President*Congress Interaction Graphs
# Christopher Gandrud
# Updated 19 July 2012 
################

# Create range of values to simulate expected values across
pres_party.r <- c(0, 1)
house_dem_rep.r <- seq(0.87, 2.04, by = .01)

# Set fitted values 
PL10SetDem <- setx(PL10, pres_party = pres_party.r, house_dem_rep = 1.2, senate_dem_rep = 1.2)
PL10SetRep <- setx(PL10, pres_party = pres_party.r, house_dem_rep = 0.8, senate_dem_rep = 0.8)

# Simulate expected values.
PL10SimDem <- sim(PL10, x = PL10SetDem)
PL10SimRep <- sim(PL10, x = PL10SetRep)

# Extract expected values from simulations (Dem)
PL10SimDem.ev <- PL10SimDem$qi
PL10SimDem.ev <-data.frame(PL10SimDem.ev$ev)
names(PL10SimDem.ev) <- c("Republican President", "Democratic President")
PL10SimDem.ev <- melt(PL10SimDem.ev, measure = 1:2)

# Extract expected values from simulations (Rep)
PL10SimRep.ev <- PL10SimRep$qi
PL10SimRep.ev <-data.frame(PL10SimRep.ev$ev)
names(PL10SimRep.ev) <- c("Republican President", "Democratic President")
PL10SimRep.ev <- melt(PL10SimRep.ev, measure = 1:2)

# Final clean up
PL10SimDem.ev$variable <- factor(PL10SimDem.ev$variable)
PL10SimDem.ev$Congress <- "Dem. Congress"

PL10SimRep.ev$variable <- factor(PL10SimRep.ev$variable)
PL10SimRep.ev$Congress <- "Rep. Congress"

# Append both sets of simulation results
PL10Bound <- rbind(PL10SimRep.ev, PL10SimDem.ev)

#### Create plots ####

# Partisan colours
partisan.congress.colours = c("Rep. Congress" = "#C42B00", "Dem. Congress" = "#2259B3")

PartyInteractionPlot <- ggplot(data = PL10Bound, aes(variable, value)) +
                                  geom_hline(aes(intercept= 0), linetype = "dotted") +
                                  stat_summary(fun.y = mean, geom = "line", aes(group = Congress), colour = "grey70") +
                                  geom_point(shape = 21, aes(color = Congress), alpha = I(0.05), size = 7) +
                                  scale_x_discrete(limits = c("Republican President", "Democratic President")) +
                                  xlab("") + ylab("Expected Standardized Forecast Error\n") +
                                  scale_color_manual(values = partisan.congress.colours, name = "") +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                                  theme_bw(base_size = 11)
                                  
print(PartyInteractionPlot)



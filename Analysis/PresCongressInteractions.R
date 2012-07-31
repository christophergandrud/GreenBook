################
# President*Congress Interaction Graphs
# Christopher Gandrud
# Updated 31 July 2012 
################

# Create range of values to simulate expected values across
pres_party.r <- c(0, 1)
house_dem_rep.r <- seq(0.87, 2.04, by = .01)

# Set fitted values 
PL11SetDem <- setx(PL11, pres_party = pres_party.r, house_dem_rep = 1.2, senate_dem_rep = 1.2)
PL11SetRep <- setx(PL11, pres_party = pres_party.r, house_dem_rep = 0.8, senate_dem_rep = 0.8)

# Simulate expected values.
PL11SimDem <- sim(PL11, x = PL11SetDem)
PL11SimRep <- sim(PL11, x = PL11SetRep)

# Extract expected values from simulations (Dem)
PL11SimDem.ev <- PL11SimDem$qi
PL11SimDem.ev <-data.frame(PL11SimDem.ev$ev)
names(PL11SimDem.ev) <- c("Republican President", "Democratic President")
PL11SimDem.ev <- melt(PL11SimDem.ev, measure = 1:2)

# Extract expected values from simulations (Rep)
PL11SimRep.ev <- PL11SimRep$qi
PL11SimRep.ev <-data.frame(PL11SimRep.ev$ev)
names(PL11SimRep.ev) <- c("Republican President", "Democratic President")
PL11SimRep.ev <- melt(PL11SimRep.ev, measure = 1:2)

# Final clean up
PL11SimDem.ev$variable <- factor(PL11SimDem.ev$variable)
PL11SimDem.ev$Congress <- "Dem. Congress"

PL11SimRep.ev$variable <- factor(PL11SimRep.ev$variable)
PL11SimRep.ev$Congress <- "Rep. Congress"

# Append both sets of simulation results
PL11Bound <- rbind(PL11SimRep.ev, PL11SimDem.ev)

#### Create plots ####

# Partisan colours
partisan.congress.colours = c("Rep. Congress" = "#C42B00", "Dem. Congress" = "#2259B3")

PartyInteractionPlot <- ggplot(data = PL11Bound, aes(variable, value)) +
                                  geom_hline(aes(intercept= 0), linetype = "dotted") +
                                  stat_summary(fun.y = mean, geom = "line", aes(group = Congress), colour = "grey70") +
                                  geom_point(shape = 21, aes(color = Congress), alpha = I(0.05), size = 7) +
                                  scale_x_discrete(limits = c("Republican President", "Democratic President")) +
                                  xlab("") + ylab("Expected Standardized Forecast Error\n") +
                                  scale_color_manual(values = partisan.congress.colours, name = "") +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                                  theme_bw(base_size = 11)
                                  
print(PartyInteractionPlot)



################
# President*Congress Interaction Graph
# Christopher Gandrud
# Updated 31 July 2012 
################

# Create range of values to simulate expected values across
pres_party.r <- c(0, 1)

# Set fitted values 
NL11SetDem <- setx(NL11, pres_party = pres_party.r, house_dem_rep = 1.2, senate_dem_rep = 1.2)
NL11SetRep <- setx(NL11, pres_party = pres_party.r, house_dem_rep = 0.8, senate_dem_rep = 0.8)

# Simulate expected values.
NL11SimDem <- sim(NL11, x = NL11SetDem)
NL11SimRep <- sim(NL11, x = NL11SetRep)

# Extract expected values from simulations (Dem)
NL11SimDem.ev <- NL11SimDem$qi
NL11SimDem.ev <-data.frame(NL11SimDem.ev$ev)
names(NL11SimDem.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL11SimDem.ev <- melt(NL11SimDem.ev, measure = 1:2)

# Extract expected values from simulations (Rep)
NL11SimRep.ev <- NL11SimRep$qi
NL11SimRep.ev <-data.frame(NL11SimRep.ev$ev)
names(NL11SimRep.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL11SimRep.ev <- melt(NL11SimRep.ev, measure = 1:2)

# Final clean up
NL11SimDem.ev$variable <- factor(NL11SimDem.ev$variable)
NL11SimDem.ev$Congress <- "Dem."

NL11SimRep.ev$variable <- factor(NL11SimRep.ev$variable)
NL11SimRep.ev$Congress <- "Rep."

# Append both sets of simulation results
NL11Bound <- rbind(NL11SimRep.ev, NL11SimDem.ev)

#### Create plots ####

# Partisan colours
partisan.congress.colours = c("Rep." = "#C42B00", "Dem." = "#2259B3")

PartyInteractionPlot <- ggplot(data = NL11Bound, aes(variable, value)) +
                                  geom_hline(aes(intercept= 0), linetype = "dotted") +
                                  stat_summary(fun.y = mean, geom = "line", aes(group = Congress), colour = "grey70") +
                                  geom_point(shape = 21, aes(color = Congress), alpha = I(0.05), size = 7) +
                                  scale_y_continuous(limits = c(-1, 0.75)) +
                                  xlab("") + ylab("") +
                                  scale_color_manual(values = partisan.congress.colours, name = "Control\nof\nCongress") +
                                  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                                  theme_bw(base_size = 11)
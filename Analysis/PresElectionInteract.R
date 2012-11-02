################
# President*Election Interaction Graph
# Christopher Gandrud
# Updated 2 November 2012 
################

# Create range of values to simulate expected values across
election_time <- (0:10)^2

# Set fitted values 
NL8SetElectionDem <- setx(NL8, pres_party = 1, elect2 = election_time)
NL8SetElectionRep <- setx(NL8, pres_party = 0, elect2 = election_time)

# Simulate expected values.
NL8SimElectionDem <- sim(NL8, x = NL8SetElectionDem)
NL8SimElectionRep <- sim(NL8, x = NL8SetElectionRep)

# Extract expected values from simulations (Democratic President)
NL8SimElectionDem.ev <- NL8SimElectionDem$qi
NL8SimElectionDem.ev <-data.frame(NL8SimElectionDem.ev$ev)
names(NL8SimElectionDem.ev) <- c("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
NL8SimElectionDem.ev <- melt(NL8SimElectionDem.ev, measure = 1:11)
NL8SimElectionDem.ev$variable <- as.numeric(gsub("x", "", NL8SimElectionDem.ev$variable))
NL8SimElectionDem.ev$Party <- "Dem"

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
NL8SimElectionDem.ev <- ddply(NL8SimElectionDem.ev, .(variable), transform, Lower = value < quantile(value, c(0.025)))

NL8SimElectionDem.ev <- ddply(NL8SimElectionDem.ev, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
NL8SimElectionDem.ev <- subset(NL8SimElectionDem.ev, Lower == FALSE & Upper == FALSE)

# Extract expected values from simulations (ElectionRep)
NL8SimElectionRep.ev <- NL8SimElectionRep$qi
NL8SimElectionRep.ev <-data.frame(NL8SimElectionRep.ev$ev)
names(NL8SimElectionRep.ev) <- c("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
NL8SimElectionRep.ev <- melt(NL8SimElectionRep.ev, measure = 1:11)
NL8SimElectionRep.ev$variable <- as.numeric(gsub("x", "", NL8SimElectionRep.ev$variable))
NL8SimElectionRep.ev$Party <- "Rep"

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
NL8SimElectionRep.ev <- ddply(NL8SimElectionRep.ev, .(variable), transform, Lower = value < quantile(value, c(0.025)))

NL8SimElectionRep.ev <- ddply(NL8SimElectionRep.ev, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
NL8SimElectionRep.ev <- subset(NL8SimElectionRep.ev, Lower == FALSE & Upper == FALSE)

# Append both sets of simulation results
NL8Bound <- rbind(NL8SimElectionDem.ev, NL8SimElectionRep.ev)

#### Create plots ####
ElectionInteractionPlot <- ggplot(data = NL8Bound, aes(variable, value), group) +
                                geom_hline(aes(intercept= 0), linetype = "dotted") +
                                stat_summary(fun.y = mean, geom = "line", 
                                             aes(group = Party), colour = "grey70") +
                                geom_point(aes(color = Party), alpha = I(0.01), size = 3) +
                                scale_x_reverse(breaks = c(10, 8, 6, 4, 2, 0)) +
                                xlab("\nQuarters to Election") + 
                                ylab("Expected Standardized Forecast Error\n") +
                                scale_color_manual(values = partisan.colors) +
                                guides(colour = guide_legend(title = NULL, override.aes = list(alpha = 1))) +
                                theme_bw(base_size = 11)
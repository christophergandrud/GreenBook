################
# President*Election Interaction Graph
# Christopher Gandrud
# Updated 3 August 2012 
################

# Create range of values to simulate expected values across
pres_party.r <- c(0, 1)

# Set fitted values 
NL8SetElection <- setx(NL8, pres_party = pres_party.r, ElectionPeriod = 1)
NL8SetNonElection <- setx(NL8, pres_party = pres_party.r, ElectionPeriod = 0)

# Simulate expected values.
NL8SimElection <- sim(NL8, x = NL8SetElection)
NL8SimNonElection <- sim(NL8, x = NL8SetNonElection)

# Extract expected values from simulations (Election)
NL8SimElection.ev <- NL8SimElection$qi
NL8SimElection.ev <-data.frame(NL8SimElection.ev$ev)
names(NL8SimElection.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL8SimElection.ev <- melt(NL8SimElection.ev, measure = 1:2)

# Extract expected values from simulations (NonElection)
NL8SimNonElection.ev <- NL8SimNonElection$qi
NL8SimNonElection.ev <-data.frame(NL8SimNonElection.ev$ev)
names(NL8SimNonElection.ev) <- c("Rep. Pres.", "Dem. Pres.")
NL8SimNonElection.ev <- melt(NL8SimNonElection.ev, measure = 1:2)

# Final clean up
NL8SimElection.ev$variable <- factor(NL8SimElection.ev$variable)
NL8SimElection.ev$Congress <- "Yes"

NL8SimNonElection.ev$variable <- factor(NL8SimNonElection.ev$variable)
NL8SimNonElection.ev$Congress <- "No"

# Append both sets of simulation results
NL8Bound <- rbind(NL8SimNonElection.ev, NL8SimElection.ev)

#### Create plots ####

# Partisan colours
election.colours = c("No" = "#B35B40", "Yes" = "#696969")

ElectionInteractionPlot <- ggplot(data = NL8Bound, aes(variable, value)) +
                                geom_hline(aes(intercept= 0), linetype = "dotted") +
                                stat_summary(fun.y = mean, geom = "line", aes(group = Congress), colour = "grey70") +
                                geom_point(shape = 21, aes(color = Congress), alpha = I(0.07), size = 7) +
                                scale_y_continuous(limits = c(-1, 0.75)) +
                                xlab("") + ylab("Expected Standardized Forecast Error\n") +
                                scale_color_manual(values = election.colours, name = "Election Period") +
                                guides(colour = guide_legend(override.aes = list(alpha = 1))) +
                                theme_bw(base_size = 11)
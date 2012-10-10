################
# Comparative Expected Values of Inflation Error with Democratic and Republican Presidents 
# using results from matched data where pres_party is the treatment variable and ls is the parametric model (PL7 from MainAnalysis2.R)
# Christopher Gandrud
# Updated 10 October 2012
################

# requires Zelig, reshape2, ggplot2

# Ranges of fitted values 
pres_party.r <- c(0, 1)

# Set fitted values, all variables other than pres_party set to their means
ModelParty <- setx(PL7, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim <- sim(PL7, x = ModelParty)

# Extract expected values from simulations
ModelParty.ev <- ModelParty.sim$qi
ModelParty.ev <-data.frame(ModelParty.ev$ev)
names(ModelParty.ev) <- c("Rep", "Dem")
ModelParty.ev <- melt(ModelParty.ev, measure = 1:2)
ModelParty.ev$variable <- factor(ModelParty.ev$variable) 

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
ModelParty.evPer <- ddply(ModelParty.ev, .(variable), transform, Lower = value < quantile(value, c(0.025)))

ModelParty.evPer <- ddply(ModelParty.evPer, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
ModelParty.evPer <- subset(ModelParty.evPer, Lower == FALSE & Upper == FALSE)

# Plot
PartyBreak <- c("Rep", "Dem")
NamePartyBreak <- c("Republican President", "Democratic President")

ModelPartyPlot <- ggplot(data = ModelParty.evPer, aes(variable, value)) +
                          geom_hline(aes(intercept= 0), linetype = "dotted") +
                          stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "grey70") +
                          geom_point(shape = 21, aes(color = variable), alpha = I(0.05), size = 7) +
                          scale_color_manual(values = partisan.colors, guide = FALSE) + # partisan.colors defined in the main .Rnw file
                          scale_x_discrete(breaks = PartyBreak, labels = NamePartyBreak) +
                          xlab("") + ylab("Expected Standardized Forecast Error\n") +
                          theme_bw(base_size = 11)

print(ModelPartyPlot)

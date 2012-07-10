################
# Comparative Expected Values of Inflation Error with Democratic and Republican Presidents 
# using results from matched data where pres_party is the treatment variable and normal.bayes is the parametric model (PB1 from MainAnalysis2.R)
# Christopher Gandrud
# Updated 10 July 2012
################

# requires Zelig, reshape2, ggplot2

# Ranges of fitted values 
pres_party.r <- c(0, 1)

# Set fitted values, all variables other than pres_party set to their means
ModelParty <- setx(PB1, pres_party = pres_party.r)

# Simulate quantities of interest
ModelParty.sim <- sim(PB1, x = ModelParty)

# Extract expected values from simulations
ModelParty.ev <- ModelParty.sim$qi
ModelParty.ev <-data.frame(ModelParty.ev$ev)
ModelParty.ev <- melt(ModelParty.ev, measure = 1:2)

# Remove 'X' from variable
ModelParty.ev$variable <- as.numeric(gsub("X", "", ModelParty.ev$variable))

# Clean up variable 
ModelParty.ev$variable[ModelParty.ev$variable == 16] <- 0
ModelParty.ev$variable[ModelParty.ev$variable == 17] <- 1
ModelParty.ev$variable <- factor(ModelParty.ev$variable, labels = c("Rep", "Dem")) 

# Plot

ModelPartyPlot <- ggplot(data = ModelParty.ev, aes(variable, value)) +
                          geom_point(shape = 21, aes(color = variable), alpha = I(0.05), size = 7) +
                          scale_color_manual(values = partisan.colors, guide = FALSE) + # partisan.colors defined in the main .Rnw file
                          geom_hline(aes(intercept= 0), linetype = "dotted") +
                          xlab("") + ylab("Expected Standardized Forecast Error\n") +
                          theme_bw(base_size = 15)

print(ModelPartyPlot)

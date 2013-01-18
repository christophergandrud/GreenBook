################
# President*Election Interaction Graph
# Christopher Gandrud
# Updated 18 January 2013
################

# To run as a stand alone file. First, run the following files from earlier in the paper:
## devtools::source_url("http://bit.ly/NXdCpk") 
## devtools::source_url("http://bit.ly/OFdA4u")

#### Simulate Expected Values
SimExpectElect <- function(m){
  require(Zelig)
  require(reshape2)
  require(plyr)
  TempSet <- setx(m, pres_party = p, elect2 = e)
  TempSim <- sim(m, x = TempSet)
  Temp.ev <- simulation.matrix(TempSim, "Expected Values: E(Y|X)") 
  Temp.ev <- reshape2::melt(Temp.ev, measure = 1:11)
  
  # Remove values outside of the 2.5% and 97.5% quantiles
  # Find 2.5% and 97.5% quantiles for HRCC
  Temp.ev <- ddply(Temp.ev, .(Var2), transform, Lower = value < quantile(value, c(0.025)))
  
  Temp.ev <- ddply(Temp.ev, .(Var2), transform, Upper = value > quantile(value, c(0.975)))
  
  # Remove variables outside of the middle 95%
  Temp.ev <- subset(Temp.ev, Lower == FALSE & Upper == FALSE)
  
  if (p == 0){
    Temp.ev$Party <- "Rep"
    name <- paste0("RepElect.ev")
  } else if (p == 1) {
    Temp.ev$Party <- "Dem"
    name <- paste0("DemElect.ev")
  }
  assign(name, Temp.ev, envir = .GlobalEnv)
}

#### Run simulations for a range of election times and pres parties ####
e <- (0:10)^2

for (p in 0:1){
  SimExpectElect(m = NL8)
}

# Append both sets of simulation results
NL8Bound <- rbind(DemElect.ev, RepElect.ev)

#### Create plot ####
ElectionInteractionPlot <- ggplot(data = NL8Bound, aes(Var2, value), group = Party) +
  geom_hline(yintercept = 0, size = 1,
             alpha = I(0.5)) +
  stat_summary(fun.y = mean, geom = "line", 
               aes(group = Party), colour = "grey70") +
  geom_point(aes(color = Party), alpha = I(0.01), size = 3) +
  scale_x_reverse(breaks = c(10, 8, 6, 4, 2, 0)) +
  scale_y_continuous(breaks = c(-0.2, 0, 0.2), 
                     labels = c(-0.2, 0, 0.2)) +
  xlab("\nQuarters to Election") + 
  ylab("Expected Standardized Forecast Error\n") +
  scale_color_manual(values = partisan.colors) +
  guides(colour = guide_legend(title = NULL, override.aes = list(alpha = 1))) +
  theme_bw(base_size = 11)
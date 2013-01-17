################
# President*Election Interaction Graph
# Christopher Gandrud
# Updated 17 January 2013
################

# To run as a stand alone file. First, run the following files from earlier in the paper:
## devtools::source_url("http://bit.ly/NXdCpk") 
## devtools::source_url("http://bit.ly/OFdA4u")

# Load library 
library(plyr)

#### Simulate Expected Values
SimExpect <- function(m){
    if (p == 0){
      name <- paste0("Rep", "Elect", i)
    } else {
      name <- paste0("Dem", "Elect", i)
    }
    TempSet <- setx(m, pres_party = p, elect2 = i)
    TempSim <- sim(m, x = TempSet)
    Temp.qi <- TempSim$qi
    assign(name, data.frame(Temp.qi$ev1), envir = .GlobalEnv) 
}

for (p in 0:1){
  for (i in (0:10)^2){
      SimExpect(m = NL8)
  }
}

#### Expected values for Democratic presidencies ####
DemElect.ev <- cbind(DemElect0, DemElect1, DemElect4, DemElect9, DemElect16, DemElect25, DemElect36, DemElect49, DemElect64, DemElect81, DemElect100)
rm(DemElect0, DemElect1, DemElect4, DemElect9, DemElect16, DemElect25, DemElect36, DemElect49, DemElect64, DemElect81, DemElect100)
names(DemElect.ev) <- c("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
DemElect.ev <- melt(DemElect.ev, measure = 1:11)
DemElect.ev$variable <- as.numeric(gsub("x", "", DemElect.ev$variable))
DemElect.ev$Party <- "Dem"

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
DemElect.ev <- ddply(DemElect.ev, .(variable), transform, Lower = value < quantile(value, c(0.025)))

DemElect.ev <- ddply(DemElect.ev, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
DemElect.ev <- subset(DemElect.ev, Lower == FALSE & Upper == FALSE)

#### Expected values for Republican presidencies ####
RepElect.ev <- cbind(RepElect0, RepElect1, RepElect4, RepElect9, RepElect16, RepElect25, RepElect36, RepElect49, RepElect64, RepElect81, RepElect100)
rm(RepElect0, RepElect1, RepElect4, RepElect9, RepElect16, RepElect25, RepElect36, RepElect49, RepElect64, RepElect81, RepElect100)
names(RepElect.ev) <- c("x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
RepElect.ev <- melt(RepElect.ev, measure = 1:11)
RepElect.ev$variable <- as.numeric(gsub("x", "", RepElect.ev$variable))
RepElect.ev$Party <- "Rep"

# Remove values outside of the 2.5% and 97.5% quantiles
# Find 2.5% and 97.5% quantiles for HRCC
RepElect.ev <- ddply(RepElect.ev, .(variable), transform, Lower = value < quantile(value, c(0.025)))

RepElect.ev <- ddply(RepElect.ev, .(variable), transform, Upper = value > quantile(value, c(0.975)))

# Remove variables outside of the middle 95%
RepElect.ev <- subset(RepElect.ev, Lower == FALSE & Upper == FALSE)

# Append both sets of simulation results
NL8Bound <- rbind(DemElect.ev, RepElect.ev)

#### Create plot ####
ElectionInteractionPlot <- ggplot(data = NL8Bound, aes(variable, value), group) +
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
####################
# Greenbook MatchIt 1st Try
# Christopher Gandrud
# 11 June 2012
####################

library(foreign)
library(MatchIt)
library(Zelig)
library(reshape2)
library(ggplot2)

#### Load data ####
  cpi.data <- read.dta("/Users/christophergandrud/Dropbox/GreenBook/GB_FRED_cpi.dta") # Load data locally from Christopher's computer
  #address <- "http://dl.dropbox.com/u/12581470/code/Replicability_code/GreenBook/GB_FRED_cpi.csv"
  #cpi.data <- read.csv(address)

#### Clean up data and create forecast error variable ####
  cpi.data$error.prop.deflator.q2 <-  (cpi.data$GB_CPI_QTR2 - cpi.data$deflator)/cpi.data$deflator

# Subset for complete (nonmissing) variables
  vars <- c("ElectionPeriod", "pres_party", "error.prop.deflator.q2", "time_to_election", "recession", "senate_dem_rep", "house_dem_rep", "ExpenditureGDP", "PotentialGDP")  
  cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
  cpi.complete <- cpi.complete[vars]  

#### Matching Model ####
  cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

cpi.matched.election <- matchit(ElectionPeriod ~ recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

summary(cpi.matched.party)
plot(cpi.matched.party)

summary(cpi.matched.election)
plot(cpi.matched.election)

# Turn matched data into data.frame for analysis
cpi.Mdf.party <- match.data(cpi.matched.party)
cpi.Mdf.election <- match.data(cpi.matched.election)

#### Run Analyses & Diagnostics in Zelig ####
MP1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.party)

ME1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.election)

# Diagnostics
  M1.geweke <- geweke.diag(M1$coefficients)
  M1.geweke

  M1.heidel <- heidel.diag(M1$coefficients)
  M1.heidel

#### Simulations ####
# Set values for simulations
  party.r <- c(0, 1)

# Set Fitted Values 
  M1.partySet <- setx(M1, pres_party = party.r)

# Simulate quantities of interest
  M1.partySim <- sim(M1, x = M1.partySet)

# Zelig Plots
plot(M1.partySim)

#### ggplot2 Plots of Expected Values ####
# Extract expected values from simulations
M1.party.e <- (M1.partySim$qi)
M1.party.e <- data.frame(M1.party.e$ev)
M1.party.e <- melt(M1.party.e, measure = 1:2)

# Remove "X" from variable
M1.party.e$variable <- as.numeric(gsub("X", "", M1.party.e$variable))

# Plot
M1.party.plot <- ggplot(M1.party.e, aes(variable, value)) +
  geom_point(shape = 21, color = "gray30", alpha = I(0.05)) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_x_reverse(breaks = c(10, 13), labels = c("Republican", "Democrat")) +
  xlab("\nPresident's Party") + ylab("Expected Inflation Forecast Error\n") +
  opts(axis.title.x = theme_text(size = 12, vjust = 0)) +
  theme_bw()

M1.party.plot

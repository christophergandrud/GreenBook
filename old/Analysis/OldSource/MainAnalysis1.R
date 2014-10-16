####################
# Greenbook MatchIt Analyses
# Christopher Gandrud
# 14 June 2012
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
vars <- c("ElectionPeriod", "pres_party", "error.prop.deflator.q2", "time_to_election", "recession", "senate_dem_rep", "house_dem_rep", "DebtGDP", "ExpenditureGDP", "PotentialGDP")  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

#### Matching Model ####
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

cpi.matched.election <- matchit(ElectionPeriod ~ recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, data = cpi.complete, method = "genetic")

# Diagnostics for Covariate Balance
# summary(cpi.matched.party)
# plot(cpi.matched.party)

# summary(cpi.matched.election)
# plot(cpi.matched.election)

# Turn matched data into data.frame for analysis
cpi.Mdf.party <- match.data(cpi.matched.party)
cpi.Mdf.election <- match.data(cpi.matched.election)


#### Presidential Party Models ####

#### Non-matched (NP) ####

# Least Squares

NPL1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.data)


# Normal Bayes

NPB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.data)

#### Matched (MP) ####

# Least Squares

MPL1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party)


# Normal Bayes

MPB1 <- zelig(error.prop.deflator.q2 ~ pres_party + recession + time_to_election + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP, model = "normal.bayes", data = cpi.Mdf.party)

#### Figures for normal.bayes Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NPB1.cat <- summary(NPB1)
NPB1.cat.sum <- as.data.frame(NPB1.cat$summary)
NPB1.cat.sum$var <- rownames(NPB1.cat.sum)

NPB1.mean.molten <- melt(NPB1.cat.sum, id = c("var"), measure.vars = c("Mean"), value.name = "mean")
NPB1.mean.molten <- NPB1.mean.molten[, -2]

NPB1.lower.molten <- melt(NPB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
NPB1.lower.molten <- NPB1.lower.molten[, -2]

NPB1.upper.molten <- melt(NPB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
NPB1.upper.molten <- NPB1.upper.molten[, -2]

NPB1.molten <- merge(NPB1.mean.molten, NPB1.lower.molten)
NPB1.molten <- merge(NPB1.molten, NPB1.upper.molten)
NPB1.molten$model <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
MPB1.cat <- summary(MPB1)
MPB1.cat.sum <- as.data.frame(MPB1.cat$summary)
MPB1.cat.sum$var <- rownames(MPB1.cat.sum)

MPB1.mean.molten <- melt(MPB1.cat.sum, id = c("var"), measure.vars = c("Mean"), value.name = "mean")
MPB1.mean.molten <- MPB1.mean.molten[, -2]

MPB1.lower.molten <- melt(MPB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
MPB1.lower.molten <- MPB1.lower.molten[, -2]

MPB1.upper.molten <- melt(MPB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
MPB1.upper.molten <- MPB1.upper.molten[, -2]

MPB1.molten <- merge(MPB1.mean.molten, MPB1.lower.molten)
MPB1.molten <- merge(MPB1.molten, MPB1.upper.molten)
MPB1.molten$model <- "Matched"

estimates <- rbind(NPB1.molten, MPB1.molten)

# Remove Intercept
estimates <- subset(estimates, var != c("(Intercept)"))
estimates <- subset(estimates, var != c("sigma2"))


##### Create comparison plot

cols <- c("#1074B3", "#BDBDBD")
name.break <- c("pres_party", "house_dem_rep", "ExpenditureGDP", "recession", "DebtGDP", "time_to_election", "PotentialGDP", "senate_dem_rep")
labels.break <- c("Dem. President", "Prop. House Dem.", "Gov. Expenditure (% GDP)", "Recession", "Gov. Debt (% GDP)", "Quarters Until Election", "Potential GDP (%GDP)", "Prop. Senate Dem.")

est.plot <- ggplot(data = estimates, aes(x = reorder(var, mean), mean, ymin = lower, ymax = upper, colour = model)) +
                      geom_pointrange(size = 3, alpha = 0.5) +
                      scale_x_discrete(breaks = name.break, labels = labels.break) +
                      scale_color_manual(values = cols, name = "") +                            
                      geom_hline(aes(intercept= 0), linetype = "dotted") +
                      ylab("\nCoefficient Estimate") + xlab("") +
                      coord_flip() +
                      theme_bw(base_size = 18)

est.plot



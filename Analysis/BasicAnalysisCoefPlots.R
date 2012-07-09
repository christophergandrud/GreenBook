####################
# Greenbook MatchIt Analyses Coef Plots Compare
# Christopher Gandrud
# 7 July 2012
####################

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

################################ Plots ############################################

#### Figures for ls Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

### LS 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NPL1.cat <- confint(NPL1)
NPL1.cat.sum <- as.data.frame(NPL1.cat)
NPL1.cat.sum$var <- rownames(NPL1.cat.sum)

NPL1.lower.molten <- melt(NPL1.cat.sum, id = c("var"), measure.vars = c("2.5 %"), value.name = "lower")
NPL1.lower.molten <- NPL1.lower.molten[, -2]

NPL1.upper.molten <- melt(NPL1.cat.sum, id = c("var"), measure.vars = c("97.5 %"), value.name = "upper")
NPL1.upper.molten <- NPL1.upper.molten[, -2]

NPL1.molten <- merge(NPL1.lower.molten, NPL1.upper.molten)
NPL1.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
MPL1.cat <- confint(MPL1)
MPL1.cat.sum <- as.data.frame(MPL1.cat)
MPL1.cat.sum$var <- rownames(MPL1.cat.sum)

MPL1.lower.molten <- melt(MPL1.cat.sum, id = c("var"), measure.vars = c("2.5 %"), value.name = "lower")
MPL1.lower.molten <- MPL1.lower.molten[, -2]

MPL1.upper.molten <- melt(MPL1.cat.sum, id = c("var"), measure.vars = c("97.5 %"), value.name = "upper")
MPL1.upper.molten <- MPL1.upper.molten[, -2]

MPL1.molten <- merge(MPL1.lower.molten, MPL1.upper.molten)
MPL1.molten$match <- "Matched"

estimates.ls <- rbind(NPL1.molten, MPL1.molten)
estimates.ls$method <- "OLS"



#### Figures for normal.bayes Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

### Normal Bayes Models 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NPB1.cat <- summary(NPB1)
NPB1.cat.sum <- as.data.frame(NPB1.cat$summary)
NPB1.cat.sum$var <- rownames(NPB1.cat.sum)

NPB1.lower.molten <- melt(NPB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
NPB1.lower.molten <- NPB1.lower.molten[, -2]

NPB1.upper.molten <- melt(NPB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
NPB1.upper.molten <- NPB1.upper.molten[, -2]

NPB1.molten <- merge(NPB1.lower.molten, NPB1.upper.molten)
NPB1.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
MPB1.cat <- summary(MPB1)
MPB1.cat.sum <- as.data.frame(MPB1.cat$summary)
MPB1.cat.sum$var <- rownames(MPB1.cat.sum)

MPB1.lower.molten <- melt(MPB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
MPB1.lower.molten <- MPB1.lower.molten[, -2]

MPB1.upper.molten <- melt(MPB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
MPB1.upper.molten <- MPB1.upper.molten[, -2]

MPB1.molten <- merge(MPB1.lower.molten, MPB1.upper.molten)
MPB1.molten$match <- "Matched"

estimates.b <- rbind(NPB1.molten, MPB1.molten)
estimates.b$method <- "Normal Bayes"

estimates <- rbind(estimates.ls, estimates.b)

# Remove Intercept
estimates <- subset(estimates, var != c("(Intercept)"))
estimates <- subset(estimates, var != c("sigma2"))


##### Create comparison plot

cols <- c("#67A380", "#696969")
name.break <- c("pres_party", "house_dem_rep", "ExpenditureGDP", "recession", "DebtGDP", "time_to_election", "PotentialGDP", "senate_dem_rep")
labels.break <- c("Dem. President", "Prop. House Dem.", "Gov. Expenditure (% GDP)", "Recession", "Gov. Debt (% GDP)", "Quarters Until Election", "Potential GDP (%GDP)", "Prop. Senate Dem.")

est.plot <- ggplot(data = estimates, aes(x = reorder(var, lower), ymin = lower, ymax = upper, colour = match)) +
                      facet_grid(~ method) + 
                      geom_linerange(size = 3, alpha = 0.6) +
                      scale_x_discrete(breaks = name.break, labels = labels.break) +
                      scale_color_manual(values = cols, name = "") +                            
                      geom_hline(aes(intercept= 0), linetype = "dotted") +
                      ylab("\nCoefficient Estimate") + xlab("") +
                      coord_flip() +
                      theme_bw(base_size = 11)

print(est.plot)



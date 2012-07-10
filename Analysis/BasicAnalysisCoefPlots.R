####################
# Greenbook MatchIt Analyses Coef Plots Compare
# Christopher Gandrud
# 10 July 2012
####################

#### Figures for ls Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

### LS 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NL6.cat <- confint(NL6)
NL6.cat.sum <- as.data.frame(NL6.cat)
NL6.cat.sum$var <- rownames(NL6.cat.sum)

NL6.lower.molten <- melt(NL6.cat.sum, id = c("var"), measure.vars = c("2.5 %"), value.name = "lower")
NL6.lower.molten <- NL6.lower.molten[, -2]

NL6.upper.molten <- melt(NL6.cat.sum, id = c("var"), measure.vars = c("97.5 %"), value.name = "upper")
NL6.upper.molten <- NL6.upper.molten[, -2]

NL6.molten <- merge(NL6.lower.molten, NL6.upper.molten)
NL6.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
PL6.cat <- confint(PL6)
PL6.cat.sum <- as.data.frame(PL6.cat)
PL6.cat.sum$var <- rownames(PL6.cat.sum)

PL6.lower.molten <- melt(PL6.cat.sum, id = c("var"), measure.vars = c("2.5 %"), value.name = "lower")
PL6.lower.molten <- PL6.lower.molten[, -2]

PL6.upper.molten <- melt(PL6.cat.sum, id = c("var"), measure.vars = c("97.5 %"), value.name = "upper")
PL6.upper.molten <- PL6.upper.molten[, -2]

PL6.molten <- merge(PL6.lower.molten, PL6.upper.molten)
PL6.molten$match <- "Matched"

estimates.ls <- rbind(NL6.molten, PL6.molten)
estimates.ls$method <- "OLS"



#### Figures for normal.bayes Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

### Normal Bayes Models 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NB1.cat <- summary(NB1)
NB1.cat.sum <- as.data.frame(NB1.cat$summary)
NB1.cat.sum$var <- rownames(NB1.cat.sum)

NB1.lower.molten <- melt(NB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
NB1.lower.molten <- NB1.lower.molten[, -2]

NB1.upper.molten <- melt(NB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
NB1.upper.molten <- NB1.upper.molten[, -2]

NB1.molten <- merge(NB1.lower.molten, NB1.upper.molten)
NB1.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
PB1.cat <- summary(PB1)
PB1.cat.sum <- as.data.frame(PB1.cat$summary)
PB1.cat.sum$var <- rownames(PB1.cat.sum)

PB1.lower.molten <- melt(PB1.cat.sum, id = c("var"), measure.vars = c("2.5%"), value.name = "lower")
PB1.lower.molten <- PB1.lower.molten[, -2]

PB1.upper.molten <- melt(PB1.cat.sum, id = c("var"), measure.vars = c("97.5%"), value.name = "upper")
PB1.upper.molten <- PB1.upper.molten[, -2]

PB1.molten <- merge(PB1.lower.molten, PB1.upper.molten)
PB1.molten$match <- "Matched"

estimates.b <- rbind(NB1.molten, PB1.molten)
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



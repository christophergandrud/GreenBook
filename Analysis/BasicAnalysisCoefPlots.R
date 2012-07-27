####################
# Greenbook MatchIt Analyses Coef Plots Compare
# Christopher Gandrud
# 27 July 2012
####################

#### Figures for ls Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary
library(reshape)

### LS 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NL7.cat <- confint(NL7)
NL7.cat.sum <- as.data.frame(NL7.cat)
NL7.cat.sum$var <- rownames(NL7.cat.sum)

NL7.lower.molten <- melt(NL7.cat.sum, id = c("var"), measure.vars = c("2.5 %"))
NL7.lower.molten <- rename(NL7.lower.molten, c(value = "lower"))
NL7.lower.molten <- NL7.lower.molten[, -2]

NL7.upper.molten <- melt(NL7.cat.sum, id = c("var"), measure.vars = c("97.5 %"))
NL7.upper.molten <- rename(NL7.upper.molten, c(value = "upper"))
NL7.upper.molten <- NL7.upper.molten[, -2]

NL7.molten <- merge(NL7.lower.molten, NL7.upper.molten)
NL7.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
PL7.cat <- confint(PL7)
PL7.cat.sum <- as.data.frame(PL7.cat)
PL7.cat.sum$var <- rownames(PL7.cat.sum)

PL7.lower.molten <- melt(PL7.cat.sum, id = c("var"), measure.vars = c("2.5 %"))
PL7.lower.molten <- rename(PL7.lower.molten, c(value = "lower"))
PL7.lower.molten <- PL7.lower.molten[, -2]

PL7.upper.molten <- melt(PL7.cat.sum, id = c("var"), measure.vars = c("97.5 %"))
PL7.upper.molten <- rename(PL7.upper.molten, c(value = "upper"))
PL7.upper.molten <- PL7.upper.molten[, -2]

PL7.molten <- merge(PL7.lower.molten, PL7.upper.molten)
PL7.molten$match <- "Matched"

estimates.ls <- rbind(NL7.molten, PL7.molten)
estimates.ls$method <- "OLS"



#### Figures for normal.bayes Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary

### Normal Bayes Models 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NB1.cat <- summary(NB1)
NB1.cat.sum <- as.data.frame(NB1.cat$summary)
NB1.cat.sum$var <- rownames(NB1.cat.sum)

NB1.lower.molten <- melt(NB1.cat.sum, id = c("var"), measure.vars = c("2.5%"))
NB1.lower.molten <- rename(NB1.lower.molten, c(value = "lower"))
NB1.lower.molten <- NB1.lower.molten[, -2]

NB1.upper.molten <- melt(NB1.cat.sum, id = c("var"), measure.vars = c("97.5%"))
NB1.upper.molten <- rename(NB1.upper.molten, c(value = "upper"))
NB1.upper.molten <- NB1.upper.molten[, -2]

NB1.molten <- merge(NB1.lower.molten, NB1.upper.molten)
NB1.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
PB1.cat <- summary(PB1)
PB1.cat.sum <- as.data.frame(PB1.cat$summary)
PB1.cat.sum$var <- rownames(PB1.cat.sum)

PB1.lower.molten <- melt(PB1.cat.sum, id = c("var"), measure.vars = c("2.5%"))
PB1.lower.molten <- rename(PB1.lower.molten, c(value = "lower"))
PB1.lower.molten <- PB1.lower.molten[, -2]

PB1.upper.molten <- melt(PB1.cat.sum, id = c("var"), measure.vars = c("97.5%"))
PB1.upper.molten <- rename(PB1.upper.molten, c(value = "upper"))
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
breaks <- c("pres_party", "house_dem_rep", "ExpenditureGDP", "recession", "DebtGDP", "time_to_election", "PotentialGDP", "DiscountRate2qChange", "GlobalModelAfter 1996", "senate_dem_rep")
break.labels <- c("Dem. President", "Prop. House Dem.", "Gov. Expenditure (% GDP)", "Recession", "Gov. Debt (% GDP)", "Quarters Until Election", "Potential GDP (%GDP)", "Discount Rate Change", "Global Model", "Prop. Senate Dem.")

est.plot <- ggplot(data = estimates, aes(x = reorder(var, lower), ymin = lower, ymax = upper, colour = match)) +
                      facet_grid(~ method) + 
                      geom_linerange(size = 3, alpha = 0.6) +
                      scale_x_discrete(breaks = breaks, labels = break.labels) +
                      #scale_y_continuous(breaks = c(-1, 0.0, 0.5), labels = c("-1", "0", "0.5")) +
                      scale_color_manual(values = cols, name = "") +                            
                      geom_hline(aes(intercept= 0), linetype = "dotted") +
                      ylab("\nCoefficient Estimate") + xlab("") +
                      coord_flip() +
                      theme_bw(base_size = 11)

print(est.plot)



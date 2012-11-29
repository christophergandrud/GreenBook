####################
# Greenbook MatchIt Analyses Coef Plots Compare
# Christopher Gandrud
# 29 November 2012
####################

#### Figures for ls Results Catapilar Plot ####
# .cat for catapilar graph
# .sum for summary
library(reshape)

### LS 
# Extract and melt quantiles for marginal posterior distributions (not matched data set)
NL5.cat <- confint(NL5)
NL5.cat.sum <- as.data.frame(NL5.cat)
NL5.cat.sum$var <- rownames(NL5.cat.sum)

NL5.lower.molten <- melt(NL5.cat.sum, id = c("var"), measure.vars = c("2.5 %"))
NL5.lower.molten <- rename(NL5.lower.molten, c(value = "lower"))
NL5.lower.molten <- NL5.lower.molten[, -2]

NL5.upper.molten <- melt(NL5.cat.sum, id = c("var"), measure.vars = c("97.5 %"))
NL5.upper.molten <- rename(NL5.upper.molten, c(value = "upper"))
NL5.upper.molten <- NL5.upper.molten[, -2]

NL5.molten <- merge(NL5.lower.molten, NL5.upper.molten)
NL5.molten$match <- "Not Matched"

# Extract and melt quantiles for marginal posterior distributions (matched data set)
PL5.cat <- confint(PL5)
PL5.cat.sum <- as.data.frame(PL5.cat)
PL5.cat.sum$var <- rownames(PL5.cat.sum)

PL5.lower.molten <- melt(PL5.cat.sum, id = c("var"), measure.vars = c("2.5 %"))
PL5.lower.molten <- rename(PL5.lower.molten, c(value = "lower"))
PL5.lower.molten <- PL5.lower.molten[, -2]

PL5.upper.molten <- melt(PL5.cat.sum, id = c("var"), measure.vars = c("97.5 %"))
PL5.upper.molten <- rename(PL5.upper.molten, c(value = "upper"))
PL5.upper.molten <- PL5.upper.molten[, -2]

PL5.molten <- merge(PL5.lower.molten, PL5.upper.molten)
PL5.molten$match <- "Matched"

estimates.ls <- rbind(NL5.molten, PL5.molten)
estimates.ls$method <- "Normal Linear"



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

cols <- c("#B35B40", "#696969")
breaks <- c("pres_party", "ExpenditureGDP", "recession", "DebtGDP", "time_to_election", "PotentialGDP", "DiscountRate2qChange", "UNRATE", "GlobalModelAfter 1996")
break.labels <- c("Dem. President", "Gov. Expenditure (% GDP)", "Recession", "Gov. Debt (% GDP)", "Quarters Until Election", "Output Gap", "Discount Rate Change", "Unemployment Rate", "Global Model")

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



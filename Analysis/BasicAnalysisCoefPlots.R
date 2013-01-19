####################
# Greenbook MatchIt Analyses Coef Plots Compare
# Christopher Gandrud
# 19 January 2013
####################

#### Figures for Results Catapilar Plot ####
#### Create Function to find and melt coefficients ####
CoefPlotPrep <- function(x){
  require(ZeligBayesian)
  
  y <- substr(deparse(substitute(x)), start = 1, stop = 1) 
  z <- substr(deparse(substitute(x)), start = 2, stop = 2)
  n <- substr(deparse(substitute(x)), start = 3, stop = 3)
  
  # Find confidence bounds
  if (z == "L"){
    Conf <- confint(x)
    Sum <- as.data.frame(Conf)
  } else if (z == "B"){
    Conf <- summary(x)
    Sum <- as.data.frame(Conf$summary)
  }
  
  Sum$var <- rownames(Sum)
  
  # Extract lower bounds
  if (z == "L"){
    Lower.molten <- reshape2::melt(Sum, id = c("var"), measure.vars = c("2.5 %"))
  } else if (z == "B"){
    Lower.molten <- reshape2::melt(Sum, id = c("var"), measure.vars = c("2.5%"))
  }
  Lower.molten <- reshape::rename(Lower.molten, c(value = "lower"))
  Lower.molten <- Lower.molten[, -2]
  
  # Extract upper bounds
  if (z == "L"){
    Upper.molten <- reshape2::melt(Sum, id = c("var"), measure.vars = c("97.5 %"))
  } else if (z == "B"){
    Upper.molten <- reshape2::melt(Sum, id = c("var"), measure.vars = c("97.5%"))
  }
  Upper.molten <- reshape::rename(Upper.molten, c(value = "upper"))
  Upper.molten <- Upper.molten[, -2]
  
  # Merge upper and lower bounds
  Molten <- merge(Lower.molten, Upper.molten)
  
  # Add descriptors 
  if (y == "N"){
    Molten$match <- "Not Matched"
  } else if (y == "P"){
    Molten$match <- "Matched"
  }
  if (z == "L"){
    Molten$method <- "Normal Linear"
  } else if (z == "B"){
    Molten$method <- "Normal Bayes"
  } 
  Name <- paste0(y, z, n, "Estimates")
  assign(Name, Molten, envir = .GlobalEnv)
}

#### Run function ####
CoefPlotPrep(NL5)
CoefPlotPrep(PL5)
CoefPlotPrep(NB1)
CoefPlotPrep(PB1)

# Bind into one data frame
estimates <- rbind(NL5Estimates, PL5Estimates, NB1Estimates, PB1Estimates)

# Remove Intercept
estimates <- subset(estimates, var != c("(Intercept)"))
estimates <- subset(estimates, var != c("sigma2"))

##### Create comparison plot ####
cols <- c("#B35B40", "#696969")

breaks <- c("pres_party", "ExpenditureGDP", "recession", "DeficitGDP", "time_to_election", "PotentialGDP", "DiscountRate2qChange", "UNRATE", "GlobalModelBefore 1996")

break.labels <- c("Dem. President", "Gov. Expenditure (% GDP)", "Recession", "Gov. Deficit (% GDP)", "Quarters Until Election", "Output Gap", "Discount Rate Change", "Unemployment Rate", "Global Model")

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



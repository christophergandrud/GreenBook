###############
# Difference of Mean Forecast Error
# Christopher Gandrud 
# 6 January 2013
###############

## This code creates a confidence interval of for the difference of mean forecast error in Democratic and Republican presidencies.

library(plotrix)

# To run as a stand alone file. First, run the following file from the paper:
## source('Analysis/Greenbook1.R') 

# Subset errors by presidential party ID
RepSubset <- subset(cpi.data$error.prop.deflator.q2,
                    cpi.data$pres_party == 0)
DemSubset <- subset(cpi.data$error.prop.deflator.q2,
                    cpi.data$pres_party == 1)

# Test for normality
# qqnorm(RepSubset); qqplot(RepSubset, lty = 2)
# qqnorm(DemSubset); qqplot(DemSubset, lty = 2)

# T-test that the subset means are differrent from 0
TRep <- t.test(RepSubset, conf.level = 0.99)
TDem <- t.test(DemSubset, conf.level = 0.99)


# Find means for each subsample
RepMean <- mean(RepSubset, na.rm = TRUE)
DemMean <- mean(DemSubset, na.rm = TRUE)

# Find the mean difference
MeanDiff <- DemMean - RepMean

# Calculate the standard errors
SERep <- std.error(RepSubset)
SEDem <- std.error(DemSubset)

# Calculate the standard error of the mean difference
SEDiff <- SEDem + SERep

# Create confidence interval to test the null hypothesis that the differenace of means = 0
LowerDiff <- MeanDiff - 2.58 * SEDiff
UpperDiff <- MeanDiff + 2.58 * SEDiff


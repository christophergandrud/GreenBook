###############
# Difference of Mean Forecast Error
# Christopher Gandrud 
# 16 October 2012
###############

## This code creates a confidence interval of for the difference of mean forecast error in Democratic and Republican presidencies.

library(plotrix)

# To run as a stand alone file. First, run the following files from the paper:
## source_url("http://bit.ly/NXdCpk") 
## source_url("http://bit.ly/Nehu34")

# Subset errors by presidential party ID
RepSubset <- subset(cpi.data$error.prop.deflator.q2,
                    cpi.data$pres_party == 0)
DemSubset <- subset(cpi.data$error.prop.deflator.q2,
                    cpi.data$pres_party == 1)

# Visually examine distributions
RepHist <- hist(RepSubset)
DemHist <- hist(DemSubset)

# Find means for each subsample
RepMean <- mean(RepSubset)
DemMean <- mean(DemSubset)

# Find the mean difference
MeanDiff <- DemMean - RepMean

# Calculate the standard errors
SERep <- std.error(RepSubset)
SEDem <- std.error(DemSubset)

# Calculate the standard error of the mean difference
SEDiff <- SEDem + SERep

# Create confidence interval to test the null hypothesis that the differenace of means = 0
Lower <- MeanDiff - 1.96 * SEDiff
Upper <- MeanDiff + 1.96 * SEDiff


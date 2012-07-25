##############
# Fed Funds Rate Investigation on Partisan Bias
# Christopher Gandrud
# Updated 24 July 2012
##############

# Load packages
library(reshape)

# Load data
PresentFunds <- read.csv("/Users/christophergandrud/Desktop/FRED_Raw_June_2012/FRED_FedFunds.csv")

# Standardise Quarter Variabl
PresentFunds$Quarter <- gsub("-01-01", ".1", x = PresentFunds$Quarter)
PresentFunds$Quarter <- gsub("-04-01", ".2", x = PresentFunds$Quarter)
PresentFunds$Quarter <- gsub("-07-01", ".3", x = PresentFunds$Quarter)
PresentFunds$Quarter <- gsub("-10-01", ".4", x = PresentFunds$Quarter)

# Create Change variable
# Quarter0 - Quarter-2
PastFunds <- data.frame(PresentFunds[-231:-232, 2])
  PastFunds <- rename(PastFunds, c(PresentFunds..231..232..2. = "Past"))
nothing <- data.frame(c(0, 0))
  nothing <- rename(nothing, c(c.0..0. = "Past")) 
PastFunds <- rbind(nothing, PastFunds)

Funds <- cbind(PresentFunds, PastFunds)

Funds$FedFunds2qChange <- Funds$FedFunds - Funds$Past

Funds <- Funds[, c("Quarter", "FedFunds", "FedFunds2qChange")]

# Remove first two quarters for missing data
Funds <- Funds[-1:-2, ]

# merge with cpi.data
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi.csv")

cpi.data <- merge(cpi.data, Funds)

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi.csv", sep = ",")


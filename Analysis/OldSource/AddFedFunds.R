##############
# Fed Funds Rate Investigation on Partisan Bias
# Christopher Gandrud
# Updated 24 July 2012
##############

# Load packages
library(reshape)

# Load data
PastFunds <- read.csv("/Users/christophergandrud/Desktop/FRED_Raw_June_2012/FRED_FedFunds.csv")

# Standardise Quarter Variable

PastFunds$Quarter <- gsub("-01-01", ".1", x = PastFunds$Quarter)
PastFunds$Quarter <- gsub("-04-01", ".2", x = PastFunds$Quarter)
PastFunds$Quarter <- gsub("-07-01", ".3", x = PastFunds$Quarter)
PastFunds$Quarter <- gsub("-10-01", ".4", x = PastFunds$Quarter)

# Create Change variable
# Quarter0 - Quarter-2
FutureFunds <- data.frame(PastFunds[-1:-2, 2])
  FutureFunds <- rename(FutureFunds, c(PastFunds..1..2..2. = "Future"))
nothing <- data.frame(c(0, 0))
  nothing <- rename(nothing, c(c.0..0. = "Future")) 
FutureFunds <- rbind(FutureFunds, nothing)

Funds <- cbind(PastFunds, FutureFunds)

Funds$FedFunds2qChange <- Funds$Future - Funds$FedFunds

Funds <- Funds[, c("Quarter", "FedFunds", "FedFunds2qChange")]

# merge with cpi.data
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi.csv")

cpi.data <- merge(cpi.data, Funds)

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi.csv", sep = ",")


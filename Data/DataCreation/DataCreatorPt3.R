##############
# Fed Funds Rate & Discount Rate Investigation on Partisan Bias
# Christopher Gandrud
# Updated 26 October 2012
##############

# Load packages
library(reshape)

###### Fed Funds Rate #####
# Data is from the FRED system at the Federal Reserve Bank of St. Louis: http://research.stlouisfed.org/fred2/
# Load data
PresentFunds <- read.csv("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/FRED_FedFunds.csv")

# Standardise Quarter Variable
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

# Create relative change variable
Funds$FedFunds2qChange <- (Funds$FedFunds - Funds$Past)/Funds$FedFunds

Funds <- Funds[, c("Quarter", "FedFunds", "FedFunds2qChange")]
Funds$FedFunds2qChange <- round(Funds$FedFunds2qChange, digits = 2)

# Remove first two quarters for missing data
Funds <- Funds[-1:-2, ]

# merge with cpi.data
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi2006.csv")

cpi.data <- merge(cpi.data, Funds)

##### Discount/Primary Rate #####
# Data is from the FRED system at the Federal Reserve Bank of St. Louis: http://research.stlouisfed.org/fred2/
# The Discount Rate averaged over a quarter is used before 2003. We had intended to use the Primary Credit Rate from the beginning of 2003 (see: http://www.federalreserve.gov/boarddocs/press/bcreg/2002/200210312/default.htm
# However this is a relatively big discontenuity between the discount rate in Q4 2002 and the primary rate in Q1 2003
# So we use the IMF's reported US discountr rate (also found on FRED) from Q4 1982 and the Discount rate before that.
# Load data
IMFDiscount <- read.csv("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/FRED_IMFDiscountRate.csv")
OldDiscount <- read.csv("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/FRED_DiscountRate.csv")

# Data for comparison to primary rate
# primary <- read.csv("/Users/christophergandrud/Desktop/FRED_Raw_June_2012/FRED_PrimaryCredit.csv")

# Clean IMF reported Discount Rate
# Standardise Quarter Variable
IMFDiscount$Quarter <- gsub("-01-01", ".1", x = IMFDiscount$Quarter)
IMFDiscount$Quarter <- gsub("-04-01", ".2", x = IMFDiscount$Quarter)
IMFDiscount$Quarter <- gsub("-07-01", ".3", x = IMFDiscount$Quarter)
IMFDiscount$Quarter <- gsub("-10-01", ".4", x = IMFDiscount$Quarter)

IMFDiscount <- rename(IMFDiscount, c(IMFDiscountRate = "DiscountRate"))
IMFDiscount <- IMFDiscount[IMFDiscount$Quarter >=  1982.4, ]

# Clean Old Discount Rate
OldDiscount$Quarter <- gsub("-01-01", ".1", x = OldDiscount$Quarter)
OldDiscount$Quarter <- gsub("-04-01", ".2", x = OldDiscount$Quarter)
OldDiscount$Quarter <- gsub("-07-01", ".3", x = OldDiscount$Quarter)
OldDiscount$Quarter <- gsub("-10-01", ".4", x = OldDiscount$Quarter)

OldDiscount <- OldDiscount[OldDiscount$Quarter <  1982.4, ]

# Append IMF Discount
discountRate <- rbind(OldDiscount, IMFDiscount)

# Create Change variable
# Quarter0 - Quarter-2
PastRate <- data.frame(discountRate[-248:-249, 2])
PastRate <- rename(PastRate, c(discountRate..248..249..2. = "Past"))
nothing <- data.frame(c(0, 0))
nothing <- rename(nothing, c(c.0..0. = "Past")) 
PastRate <- rbind(nothing, PastRate)

Rate <- cbind(discountRate, PastRate)

#Create relative change variable
Rate$DiscountRate2qChange <- (Rate$DiscountRate - Rate$Past)/Rate$DiscountRate

Rate$DiscountRate2qChange <- round(Rate$DiscountRate2qChange, digits = 2)
Rate$DiscountRate <- round(Rate$DiscountRate, digits = 2)


Rate <- Rate[, c("Quarter", "DiscountRate", "DiscountRate2qChange")]

# Remove first two quarters for missing data
Rate <- Rate[-1:-2, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate)

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2006.csv", sep = ",")


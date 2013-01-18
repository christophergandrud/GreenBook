##############
# Fed Funds Rate & Discount Rate Investigation on Partisan Bias
# Christopher Gandrud
# Updated 17 Januar 2013
##############

# Load packages
library(reshape)
library(stringr)

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
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi_2006.csv")

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
# Quarter0 - Quarter-1
PastRate1 <- data.frame(discountRate[-249, 2])
PastRate1 <- rename(PastRate1, c(discountRate..249..2. = "Past1"))
nothing <- data.frame(c(0))
nothing <- rename(nothing, c(c.0. = "Past1")) 
PastRate1 <- rbind(nothing, PastRate1)

Rate1 <- cbind(discountRate, PastRate1)

#Create relative change variable
Rate1$DiscountRate1qChange <- (Rate1$DiscountRate - Rate1$Past1)/Rate1$DiscountRate

Rate1$DiscountRate1qChange <- round(Rate1$DiscountRate1qChange, digits = 2)
Rate1$DiscountRate <- round(Rate1$DiscountRate, digits = 2)

Rate1 <- Rate1[, c("Quarter", "DiscountRate", "DiscountRate1qChange")]

# Remove first quarter for missing data
Rate1 <- Rate1[-1, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate1)

# Create Change variable
# Quarter0 - Quarter-2
PastRate2 <- data.frame(discountRate[-248:-249, 2])
PastRate2 <- rename(PastRate2, c(discountRate..248..249..2. = "Past2"))
nothing <- data.frame(c(0, 0))
nothing <- rename(nothing, c(c.0..0. = "Past2")) 
PastRate2 <- rbind(nothing, PastRate2)

Rate2 <- cbind(discountRate, PastRate2)

#Create relative change variable
Rate2$DiscountRate2qChange <- (Rate2$DiscountRate - Rate2$Past2)/Rate2$DiscountRate

Rate2$DiscountRate2qChange <- round(Rate2$DiscountRate2qChange, digits = 2)
Rate2$DiscountRate <- round(Rate2$DiscountRate, digits = 2)


Rate2 <- Rate2[, c("Quarter", "DiscountRate2qChange")]

# Remove first two quarters for missing data
Rate2 <- Rate2[-1:-2, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate2)

# Create Change variable
# Quarter0 - Quarter-3
PastRate3 <- data.frame(discountRate[-247:-249, 2])
PastRate3 <- rename(PastRate3, c(discountRate..247..249..2. = "Past3"))
nothing <- data.frame(c(0, 0, 0))
nothing <- rename(nothing, c(c.0..0..0. = "Past3")) 
PastRate3 <- rbind(nothing, PastRate3)

Rate3 <- cbind(discountRate, PastRate3)

#Create relative change variable
Rate3$DiscountRate3qChange <- (Rate3$DiscountRate - Rate3$Past3)/Rate3$DiscountRate

Rate3$DiscountRate3qChange <- round(Rate3$DiscountRate3qChange, digits = 2)
Rate3$DiscountRate <- round(Rate3$DiscountRate, digits = 2)

Rate3 <- Rate3[, c("Quarter", "DiscountRate3qChange")]

# Remove first three quarters for missing data
Rate3 <- Rate3[-1:-3, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate3)

# Create Change variable
# Quarter0 - Quarter-4
PastRate4 <- data.frame(discountRate[-246:-249, 2])
PastRate4 <- rename(PastRate4, c(discountRate..246..249..2. = "Past4"))
nothing <- data.frame(c(0, 0, 0, 0))
nothing <- rename(nothing, c(c.0..0..0..0. = "Past4")) 
PastRate4 <- rbind(nothing, PastRate4)

Rate4 <- cbind(discountRate, PastRate4)

#Create relative change variable
Rate4$DiscountRate4qChange <- (Rate4$DiscountRate - Rate4$Past4)/Rate4$DiscountRate

Rate4$DiscountRate4qChange <- round(Rate4$DiscountRate4qChange, digits = 2)
Rate4$DiscountRate <- round(Rate4$DiscountRate, digits = 2)

Rate4 <- Rate4[, c("Quarter", "DiscountRate4qChange")]

# Remove first four quarters for missing data
Rate4 <- Rate4[-1:-4, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate4)

# Create Change variable
# Quarter0 - Quarter-5
PastRate5 <- data.frame(discountRate[-245:-249, 2])
PastRate5 <- rename(PastRate5, c(discountRate..245..249..2. = "Past5"))
nothing <- data.frame(c(0, 0, 0, 0, 0))
nothing <- rename(nothing, c(c.0..0..0..0..0. = "Past5")) 
PastRate5 <- rbind(nothing, PastRate5)

Rate5 <- cbind(discountRate, PastRate5)

#Create relative change variable
Rate5$DiscountRate5qChange <- (Rate5$DiscountRate - Rate5$Past5)/Rate5$DiscountRate

Rate5$DiscountRate5qChange <- round(Rate5$DiscountRate5qChange, digits = 2)
Rate5$DiscountRate <- round(Rate5$DiscountRate, digits = 2)

Rate5 <- Rate5[, c("Quarter", "DiscountRate5qChange")]

# Remove first five quarters for missing data
Rate5 <- Rate5[-1:-5, ]

# merge with cpi.data
cpi.data <- merge(cpi.data, Rate5)

#### Add Deficit Data ####
# From FRED: http://research.stlouisfed.org/fred2/graph/?id=FYFSGDA188S
# Accessed 18 January 2013
# Federal Surplus or Deficit [-] as Percent of Gross Domestic Product (FYFSGDA188S)
# Annual

# Load data
Deficit <- read.csv("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012/FRED_DeficitGDP.csv")

# Standardise Year Variable
Deficit$Year <- gsub("01/01/", "", x = Deficit$Year)

# Create cpi.data year variable
cpi.data$Year <- substr(cpi.data$Quarter, start = 1, stop = 4)

# Merge by year
cpi.data <- merge(cpi.data, Deficit)

# Remove year variable
cpi.data <- gdata::remove.vars(cpi.data, names = "Year")

### Write Data ####
write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2006.csv", sep = ",")


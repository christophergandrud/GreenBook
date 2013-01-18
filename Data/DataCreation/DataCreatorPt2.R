############
# Greenbook Data Merge Mid-June & Updated for the 2006 Data in October 2012
# Christopher Gandrud
# 26 October 2012
############

library(foreign)
library(reshape)

setwd("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/FREDRawJuneOct2012")

# Data downloaded from FRED, some clean up by hand
# Series included: GDP, GFDEBTN, FGEXPND, NGDPPOT, UNRATE

gdp <- read.csv("FRED_gdp.csv")
GovDebt<- read.csv("FRED_GovDebt.csv")
GovExpenditure <- read.csv("FRED_govExpenditure.csv")
Potential <- read.csv("FRED_Potential.csv")
Unemployment <- read.csv("FRED_Unemploy.csv") # Civilian Unemployment Rate
GBUnemployment <- read.csv("GB_Unemployment.csv")

# Standardise Quarter

gdp$Quarter <- as.Date(gdp$Quarter, "%m/%d/%Y")
gdp$Quarter <- as.character(gdp$Quarter)

Potential$Quarter <- as.Date(Potential$Quarter, "%m/%d/%Y")
Potential$Quarter <- as.character(Potential$Quarter)

gdp$Quarter <- gsub("-01-01", ".1", x = gdp$Quarter)
gdp$Quarter <- gsub("-01-04", ".2", x = gdp$Quarter)
gdp$Quarter <- gsub("-01-07", ".3", x = gdp$Quarter)
gdp$Quarter <- gsub("-01-10", ".4", x = gdp$Quarter)

GovDebt$Quarter <- gsub("-01-01", ".1", x = GovDebt$Quarter)
GovDebt$Quarter <- gsub("-04-01", ".2", x = GovDebt$Quarter)
GovDebt$Quarter <- gsub("-07-01", ".3", x = GovDebt$Quarter)
GovDebt$Quarter <- gsub("-10-01", ".4", x = GovDebt$Quarter)

GovExpenditure$Quarter <- gsub("-01-01", ".1", x = GovExpenditure$Quarter)
GovExpenditure$Quarter <- gsub("-04-01", ".2", x = GovExpenditure$Quarter)
GovExpenditure$Quarter <- gsub("-07-01", ".3", x = GovExpenditure$Quarter)
GovExpenditure$Quarter <- gsub("-10-01", ".4", x = GovExpenditure$Quarter)

Potential$Quarter <- gsub("-01-01", ".1", x = Potential$Quarter)
Potential$Quarter <- gsub("-01-04", ".2", x = Potential$Quarter)
Potential$Quarter <- gsub("-01-07", ".3", x = Potential$Quarter)
Potential$Quarter <- gsub("-01-10", ".4", x = Potential$Quarter)

Unemployment$Quarter <- gsub("-01-01", ".1", x = Unemployment$Quarter)
Unemployment$Quarter <- gsub("-04-01", ".2", x = Unemployment$Quarter)
Unemployment$Quarter <- gsub("-07-01", ".3", x = Unemployment$Quarter)
Unemployment$Quarter <- gsub("-10-01", ".4", x = Unemployment$Quarter)

#### Clean Up GreenBook Unemployment Data ####
# Data from http://www.phil.frb.org/research-and-data/real-time-center/greenbook-data/philadelphia-data-set.cfm
# Accessed 26 October 2012
GBUnemployment <- rename(GBUnemployment, c(
                          DATE = "Quarter", 
                          GB_Unemp2 = "GB_Unemp0",
                          GB_Unemp3 = "GB_Unemp1",
                          GB_Unemp4 = "GB_Unemp2",
                          GB_Unemp5 = "GB_Unemp3",
                          GB_Unemp6 = "GB_Unemp4",
                          GB_Unemp7 = "GB_Unemp5"))

GBUnemployment <- GBUnemployment[, 1:7]

# Merge datasets

data <- merge(gdp, GovDebt)
data <- merge(data, GovExpenditure)
data <- merge(data, Potential)
data <- merge(data, Unemployment)
data <- merge(data, GBUnemployment)


# Convert to billions
data$FedDebt <- data$FedDebt/1000

# Create Ratio Variables

data$DebtGDP <- (data$FedDebt/data$GDP)*100
data$ExpenditureGDP <- (data$CurrentExpenditure/data$GDP)*100
data$PotentialGDP <- (data$Potential/data$GDP)*100

# Keep relevant variables 

vars <- c("Quarter", "DebtGDP", "ExpenditureGDP", "PotentialGDP", "UNRATE",
          "GB_Unemp0", "GB_Unemp1", "GB_Unemp2", "GB_Unemp3", "GB_Unemp4",
          "GB_Unemp5")
data <- data[, vars]

# Save output

write.dta(data, file = "JuneOct2012Data.dta")

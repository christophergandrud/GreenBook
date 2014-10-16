##############
# Add PSRM R and R Round 1 Suggested Variables
# Christopher Gandrud
# Updated 12 March 2014
##############

library(lubridate)
library(plyr)
library(DataCombine)

# Load main data
cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv")
cpi.data$year <- gsub("\\.[1-4]", "", cpi.data$Quarter)

# Load Productivity data
## Data downloaded from the BLS, series ID: PRS85006092
product <- read.csv("~/Dropbox/GreenBook/Base_Data/BLS_Productivity2013.csv", stringsAsFactors = FALSE)

product$Period <- gsub("Q0", "", product$Period)

product$Quarter <- as.numeric(paste(product$Year, product$Period, sep = "."))

product <- product[, c("Quarter", "Value")]
names(product) <- c("Quarter", "productivity_change")

# Load Oil Price data
## Data downloaded from FRED. Symbol OILPRICE (West Texas Intermediate Crude)
## The FRED interface was used to convert the data into quarter averages and % change from quarter one year before.
oil <- read.csv("~/Dropbox/GreenBook/Base_Data/WTICrudePriceChange.csv", stringsAsFactors = FALSE)
oil <- subset(oil, OILPRICE_CH1 != "#N/A")
oil$OILPRICE_CH1 <- as.numeric(oil$OILPRICE_CH1)

oil$Quarter <- ymd(oil$Quarter)
oil$Quarter <- quarter(oil$Quarter, with_year = TRUE)
names(oil) <- c("Quarter", "WTI_crude_price")

# Number of armed conflicts
## Data downloaded from PCR UU (http://www.pcr.uu.se/research/ucdp/datasets/onset_of_intrastate_armed_conflict/)
conflict <- read.csv("~/Dropbox/GreenBook/Base_Data/DyadicInterstateConflict.csv", stringsAsFactors = FALSE)
# Keep conflicts that the United States is involved 
con1 <- grepl.sub(data = conflict, Var = 'SideA', patterns = 'United States')
con2 <- grepl.sub(data = conflict, Var = 'SideA2nd', patterns = 'United States')
con3 <- grepl.sub(data = conflict, Var = 'SideB', patterns = 'United States')
con4 <- grepl.sub(data = conflict, Var = 'SideB2nd', patterns = 'United States')

ConComb <- rbind(con1, con2, con3, con4)

ConComb <- ConComb[order(ConComb$YEAR, ConComb$ConflictID), ]

ConComb <- ConComb[, c('ConflictID', 'YEAR')]
ConComb$dum <- 1

ConComb <- ddply(ConComb, "YEAR", transform, num_conflicts = sum(dum))
ConComb <- ConComb[!duplicated(ConComb[, 'YEAR']), ]
ConComb <- ConComb[, c("YEAR", "num_conflicts")]
names(ConComb) <- c('year', 'num_conflicts')

# Combine data sets
cpi.data <- merge(cpi.data, product, all.x = TRUE)
cpi.data <- merge(cpi.data, oil, all.x = TRUE)
cpi.data <- merge(cpi.data, ConComb, all.x = TRUE)

cpi.data$num_conflicts[is.na(cpi.data$num_conflicts)] <- 0 # 0 conflicts if NA

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv", sep = ",", row.names = FALSE)

##############
# Add PSRM R and R Suggested Variables
# Christopher Gandrud
# Updated 13 October 2013
##############

library(lubridate)
library(plyr)

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
conflict <- read.csv("~/Dropbox/GreenBook/Base_Data/OnsetOfInterStateConflict.csv", stringsAsFactors = FALSE)
# Keep year and incidencev412, dummy for each country year that there was interstate conflict
conflict <- conflict[, c('year', 'sumconfv412')]
conflict <- ddply(conflict, "year", transform, num_conflicts = sum(sumconfv412))
conflict <- conflict[!duplicated(conflict[, c(1, 2)]), ]
conflict <- conflict[, c("year", "num_conflicts")]

# Combine data sets
cpi.data <- merge(cpi.data, product, all.x = TRUE)
cpi.data <- merge(cpi.data, oil, all.x = TRUE)
cpi.data <- merge(cpi.data, conflict, all.x = TRUE)

write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv", sep = ",")

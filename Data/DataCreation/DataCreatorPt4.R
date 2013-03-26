##############
# Final Clean Up
# Christopher Gandrud
# Updated 17 January 2013
##############

# Load Data
cpi.data <- read.csv(file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv")

##### Create DV, Other Cleaning ########

# Create presidential party factor variable
cpi.data$pres_party_name <- factor(cpi.data$pres_party, label = c("Rep", "Dem"))

# Create standardized forecast error variable
cpi.data$error.prop.deflator.q0 <-  (cpi.data$GB_CPI_QTR0 - cpi.data$deflator)/cpi.data$deflator
cpi.data$error.prop.deflator.q1 <-  (cpi.data$GB_CPI_QTR1 - cpi.data$deflator)/cpi.data$deflator
cpi.data$error.prop.deflator.q2 <-  (cpi.data$GB_CPI_QTR2 - cpi.data$deflator)/cpi.data$deflator
cpi.data$error.prop.deflator.q3 <-  (cpi.data$GB_CPI_QTR3 - cpi.data$deflator)/cpi.data$deflator
cpi.data$error.prop.deflator.q4 <-  (cpi.data$GB_CPI_QTR4 - cpi.data$deflator)/cpi.data$deflator
cpi.data$error.prop.deflator.q5 <-  (cpi.data$GB_CPI_QTR5 - cpi.data$deflator)/cpi.data$deflator

cpi.data$Quarter <- as.numeric(cpi.data$Quarter)

# Create FRB/Global Model Variable 
cpi.data$GlobalModel[cpi.data$Quarter > 1995.4] <- "After 1996"
cpi.data$GlobalModel[cpi.data$Quarter < 1996.1] <- "Before 1996"

# Create Fed Chair variable
cpi.data$Chair[cpi.data$Quarter > 1987.3] <-  "Greenspan"
cpi.data$Chair[cpi.data$Quarter > 2005.4] <-  "Bernanke"
cpi.data$Chair[cpi.data$Quarter <= 1987.3] <- "Volcker"
cpi.data$Chair[cpi.data$Quarter <= 1979.3] <- "Miller"
cpi.data$Chair[cpi.data$Quarter <= 1978.1] <- "Burns"
cpi.data$Chair[cpi.data$Quarter <= 1970.1] <- "Martin"
cpi.data$Chair <- factor(cpi.data$Chair)

## Remove 2 quarters from Johnson presidency
cpi.data <- subset(cpi.data, president != "Johnson")

cpi.data$presTerm <- as.factor(cpi.data$presTerm)

## Electio Period 4 quarters ###
cpi.data$ElectionPeriod4[cpi.data$time_to_election >= 4] <- 0
cpi.data$ElectionPeriod4[cpi.data$time_to_election < 4] <- 1

### Write Data ####
write.table(x = cpi.data, file = "/git_repositories/GreenBook/Data/GB_FRED_cpi_2007.csv", sep = ",")
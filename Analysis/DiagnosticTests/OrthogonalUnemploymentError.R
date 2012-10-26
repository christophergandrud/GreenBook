###############
# Run Models with an Orthoganal Variable (Unemployment Errors)
# Christopher Gandrud 
# 26 October 2012
###############

# Load libraries
library(Zelig)
library(MatchIt)

# Load data from GitHub
url <- "https://raw.github.com/christophergandrud/GreenBook/master/Data/GB_FRED_cpi_2006.csv"
cpi.data <- getURL(url)
cpi.data <- read.csv(textConnection(cpi.data))

##### Create DV, Other Cleaning ########

# Create presidential party factor variable
cpi.data$pres_party_name <- factor(cpi.data$pres_party, label = c("Rep", "Dem"))

# Create standardized forecast error variable
cpi.data$error.unemploy.q0 <-  (cpi.data$GB_Unemp0 - cpi.data$UNRATE)/cpi.data$UNRATE
cpi.data$error.unemploy.q1 <-  (cpi.data$GB_Unemp1 - cpi.data$UNRATE)/cpi.data$UNRATE
cpi.data$error.unemploy.q2 <-  (cpi.data$GB_Unemp2 - cpi.data$UNRATE)/cpi.data$UNRATE
cpi.data$error.unemploy.q3 <-  (cpi.data$GB_Unemp3 - cpi.data$UNRATE)/cpi.data$UNRATE
cpi.data$error.unemploy.q4 <-  (cpi.data$GB_Unemp4 - cpi.data$UNRATE)/cpi.data$UNRATE
cpi.data$error.unemploy.q5 <-  (cpi.data$GB_Unemp5 - cpi.data$UNRATE)/cpi.data$UNRATE

# Create FRB/Global Model Variable 
cpi.data$GlobalModel[cpi.data$Quarter > 1995.4] <- "1"
cpi.data$GlobalModel[cpi.data$Quarter < 1996.1] <- "0"
cpi.data$GlobalModel <- factor(cpi.data$GlobalModel, labels = c("Before 1996", "After 1996"))  

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

cpi.data$president <- as.factor(cpi.data$presTerm)

#### Create Partisan Error Graph ####
# Partisan colours
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

errors.employ.time <- ggplot(cpi.data, aes(x = Quarter, y = error.unemploy.q2)) +
                      geom_hline(yintercept = 0, size = 1, alpha = I(0.5)) +
                      geom_point(aes(color = pres_party_name)) +
  stat_smooth(method = "lm", aes(group = presTerm, color = pres_party_name, fill = pres_party_name)) +
  scale_color_manual(values = partisan.colors, name = "") +
  scale_fill_manual(values = partisan.colors, name = "") +
  xlab("") + ylab("Standardized Unemployment\n Forecast Error\n") + 
  scale_x_continuous(limits = c(1968, 2007),
                     breaks = c(1970, 1980, 1990, 2000, 2007), 
                     labels = c(1970, 1980, 1990, 2000, 2007)) +
  scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2, 0.3), labels = c(-0.1, 0, 0.1, 0.2, 0.3)) +
  theme_bw(base_size = 12)

print(errors.employ.time) 

#### Match Data and Run Comparison Analysis ####

# Subset for complete (nonmissing) values
# matchit requires data sets to have no missing values
vars <- c("Quarter", "ElectionPeriod", "pres_party", "error.unemploy.q2", 
          "time_to_election", "recession", "senate_dem_rep", 
          "house_dem_rep", "DebtGDP", "ExpenditureGDP",
          "PotentialGDP", "GlobalModel", "FedFunds", "FedFunds2qChange", 
          "DiscountRate", "DiscountRate2qChange", "Chair"
          )  
cpi.complete <- cpi.data[complete.cases(cpi.data[vars]),]
cpi.complete <- cpi.complete[vars]

# Party, Only pres*ElectionPeriod Interaction
cpi.matched.party <- matchit(pres_party ~ recession + time_to_election + ElectionPeriod + senate_dem_rep + house_dem_rep + ExpenditureGDP + PotentialGDP + GlobalModel + DiscountRate2qChange + pres_party*ElectionPeriod, data = cpi.complete, method = "genetic", pop.size = 161)

# summary(cpi.matched.party, interactions = TRUE)
# plot(cpi.matched.party, type = "QQ", interactive = FALSE)
# plot(cpi.matched.party, type = "jitter", interactive = FALSE)

# Turn matched data into data.frame for analysis
cpi.Mdf.party <- match.data(cpi.matched.party)

################### Parametric Models ################
# Only partisanship
PLU1 <- zelig(error.unemploy.q2 ~ pres_party, model = "ls", data = cpi.Mdf.party, cite = FALSE)

# Only relevant variables (i.e. excluding the inflation relevant variables DiscountRate2qChange and GlobalModel)
PLU2 <- zelig(error.unemploy.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP, model = "ls", data = cpi.Mdf.party, cite = FALSE)

# All variables in model PL7
PLU3 <- zelig(error.prop.deflator.q2 ~ pres_party + time_to_election + recession + senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + PotentialGDP + DiscountRate2qChange + GlobalModel, model = "ls", data = cpi.Mdf.party, cite = FALSE)
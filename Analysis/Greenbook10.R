###############
# Run Models with an Orthoganal Variable (Unemployment Errors)
# Christopher Gandrud 
# 5 May 2014
###############

# Load libraries
library(Zelig)
library(MatchIt)
library(ggplot2)

# Load data
cpi.dataU <- read.csv("Data/GB_FRED_cpi_2007.csv", stringsAsFactors = FALSE)

##### Create DV, Other Cleaning ########

# Create presidential party factor variable
cpi.dataU$pres_party_name <- factor(cpi.dataU$pres_party, 
                                    label = c("Rep", "Dem"))

# Create standardized forecast error variable
cpi.dataU$error.unemploy.q0 <-  (cpi.dataU$GB_Unemp0 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE
cpi.dataU$error.unemploy.q1 <-  (cpi.dataU$GB_Unemp1 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE
cpi.dataU$error.unemploy.q2 <-  (cpi.dataU$GB_Unemp2 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE
cpi.dataU$error.unemploy.q3 <-  (cpi.dataU$GB_Unemp3 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE
cpi.dataU$error.unemploy.q4 <-  (cpi.dataU$GB_Unemp4 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE
cpi.dataU$error.unemploy.q5 <-  (cpi.dataU$GB_Unemp5 - cpi.dataU$UNRATE)/cpi.dataU$UNRATE

cpi.dataU$error.prop.deflator.q2 <- (cpi.dataU$GB_CPI_QTR2 - cpi.dataU$deflator)/cpi.dataU$deflator


# Create FRB/Global Model Variable 
cpi.dataU$GlobalModel[cpi.dataU$Quarter > 1995.4] <- "After 1996"
cpi.dataU$GlobalModel[cpi.dataU$Quarter < 1996.1] <- "Before 1996"

# Create Fed Chair variable
cpi.dataU$Chair[cpi.dataU$Quarter > 1987.3] <-  "Greenspan"
cpi.dataU$Chair[cpi.dataU$Quarter > 2005.4] <-  "Bernanke"
cpi.dataU$Chair[cpi.dataU$Quarter <= 1987.3] <- "Volcker"
cpi.dataU$Chair[cpi.dataU$Quarter <= 1979.3] <- "Miller"
cpi.dataU$Chair[cpi.dataU$Quarter <= 1978.1] <- "Burns"
cpi.dataU$Chair[cpi.dataU$Quarter <= 1970.1] <- "Martin"
cpi.dataU$Chair <- factor(cpi.dataU$Chair)

## Remove 2 quarters from Johnson presidency
cpi.dataU <- subset(cpi.dataU, president != "Johnson")

cpi.dataU$president <- as.factor(cpi.dataU$presTerm)

#### Create Partisan Error Graph ####
# Partisan colours
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

errors.employ.time <- ggplot(cpi.dataU, aes(x = Quarter, y = 
                            error.unemploy.q2)) +
                      geom_hline(yintercept = 0, size = 1, alpha = I(0.5)) +
                      geom_point(aes(color = pres_party_name)) +
  stat_smooth(method = "lm", aes(group = presTerm, color = pres_party_name, 
              fill = pres_party_name)) +
  scale_color_manual(values = partisan.colors, name = "") +
  scale_fill_manual(values = partisan.colors, name = "") +
  xlab("") + ylab("Unemployment Forecast Error\n") + 
  ggtitle("Errors in Employment Forecasts \n Made 2 Qtr. Beforehand\n") +
  scale_x_continuous(limits = c(1968, 2008),
                     breaks = c(1970, 1980, 1990, 2000), 
                     labels = c(1970, 1980, 1990, 2000)) +
  scale_y_continuous(breaks = c(-0.1, 0, 0.1, 0.2, 0.3), 
                    labels = c(-0.1, 0, 0.1, 0.2, 0.3)) +
  guides(colour = guide_legend(reverse = TRUE), 
         fill = guide_legend(reverse = TRUE)) +
  theme_bw(base_size = 12)

#### Correlation Between Forecast Error and Unemployment Error ####

Cor <- cor.test(cpi.dataU$error.unemploy.q2, cpi.dataU$error.prop.deflator.q2)

Estat <- as.vector(Cor$estimate)

Pstat <- as.vector(Cor$p.value)

# Graph the relationship
ErrorOrthogScatter <- ggplot(data = cpi.dataU, aes(error.unemploy.q2, 
                            error.prop.deflator.q2)) + 
                              geom_smooth() +
                              geom_point() + 
                              xlab("\n Unemployment Forecast Errors") +
                              ylab("Inflation Forecast Errors") +
                              ggtitle("Scatterplot of Unemployment and\n Inflation Forecast Errors\n") +
                              theme_bw(base_size = 12)

################### Parametric Models (UnMatched) ################
# Only partisanship
PLU1 <- zelig(error.unemploy.q2 ~ pres_party, model = "ls", data = cpi.dataU, 
            cite = FALSE)

# Only relevant variables (i.e. excluding the inflation relevant variables 
# DiscountRate2qChange)
PLU2 <- zelig(error.unemploy.q2 ~ pres_party + time_to_election + recession + 
                senate_dem_rep + house_dem_rep + DebtGDP + ExpenditureGDP + 
                PotentialGDP + GlobalModel, model = "ls", data = cpi.dataU, 
                cite = FALSE)                       
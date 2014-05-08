####################
# Greenbook Actual Inflation vs. Forecasts Graph
# Christopher Gandrud
# 8 May 2014
####################

library(reshape2)
library(ggplot2)

setwd('/git_repositories/Greenbook') # Change as appropriate. 

# Load data
cpi.data <- read.csv("Data/GB_FRED_cpi_2007.csv", stringsAsFactors = FALSE)

##### Create Plot #######
### Melt data, i.e. reshape
cpi.abs <- melt(cpi.data, id = "Quarter", measure = c("GB_CPI_QTR2",
                "deflator"))

### Rename variables
cpi.abs$variable <- gsub("GB_CPI_QTR2", "Forecast", cpi.abs$variable)
cpi.abs$variable <- gsub("deflator", "Actual", cpi.abs$variable)

### Colours
absolute.colors <- c("Forecast" = "#B35B40", "Actual" = "#000000")

### Create line graph
absInflation <- qplot(Quarter, value, geom = "line", data = cpi.abs,
                        color = variable, linetype = variable) +
  xlab("") + ylab("Inflation\n") +
  scale_color_manual(values = absolute.colors, name = "") +
  scale_linetype(name = "") +
  scale_x_continuous(limits = c(1968, 2008),
                     breaks = c(1970, 1980, 1990, 2000, 2008),
                     labels = c(1970, 1980, 1990, 2000, 2008)) +
  geom_vline(aes(xintercept = 1996), linetype = "dotted", colour = "grey50") +
  annotate("text", x = 2001, y = 8.5, label = "FRB/Global", colour = "grey50") +
  theme_bw(base_size = 12)

print(absInflation)

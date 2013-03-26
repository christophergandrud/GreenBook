####################
# Greenbook Actual Inflation vs. Forecasts Graph
# Christopher Gandrud
# 26 March 2013
####################

library(RCurl)
library(reshape2)
library(ggplot2)

# Load data from GitHub
url <- "https://raw.github.com/christophergandrud/GreenBook/master/Data/GB_FRED_cpi_2007.csv"
cpi.data <- getURL(url)
cpi.data <- read.csv(textConnection(cpi.data))

## Other data loading options
#cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi.csv") # Load data locally from Christopher's computer
# cpi.data <- read.dta("http://dl.dropbox.com/u/12581470/code/Replicability_code/GreenBook/GB_FRED_cpi.dta") # Load data from Dropbox 

##### Create Plot #######
### Melt data, i.e. reshape
cpi.abs <- melt(cpi.data, id = "Quarter", measure = c("GB_CPI_QTR2", "deflator"))

### Rename variables
cpi.abs$variable <- gsub("GB_CPI_QTR2", "Forecast", cpi.abs$variable)
cpi.abs$variable <- gsub("deflator", "Actual", cpi.abs$variable)

### Colours
absolute.colors <- c("Forecast" = "#B35B40", "Actual" = "#000000")

### Create line graph
absInflation <- qplot(Quarter, value, geom = "line", data = cpi.abs, color = variable, linetype = variable) +
  xlab("") + ylab("Inflation\n") +
  scale_color_manual(values = absolute.colors, name = "") +
  scale_linetype(name = "") + 
  scale_x_continuous(limits = c(1968, 2007),
                     breaks = c(1970, 1980, 1990, 2000, 2007), 
                     labels = c(1970, 1980, 1990, 2000, 2007)) +
  geom_vline(aes(xintercept = 1996), linetype = "dotted", colour = "grey50") +
  annotate("text", x = 2001, y = 8.5, label = "FRB/Global", colour = "grey50") +
  theme_bw(base_size = 12)

print(absInflation)
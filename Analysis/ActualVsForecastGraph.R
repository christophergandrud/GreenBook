####################
# Greenbook Actual Inflation vs. Forecasts Graph
# Christopher Gandrud
# 3 October 2012
####################

library(RCurl)
library(reshape2)
library(ggplot2)

### Load data

# Load data from GitHub
url <- "https://raw.github.com/christophergandrud/GreenBook/master/Data/GB_FRED_cpi.csv"
cpi.data <- getURL(url)
cpi.data <- read.csv(textConnection(cpi.data))

## Other data loading options
#cpi.data <- read.csv("/git_repositories/GreenBook/Data/GB_FRED_cpi.csv") # Load data locally from Christopher's computer
# cpi.data <- read.dta("http://dl.dropbox.com/u/12581470/code/Replicability_code/GreenBook/GB_FRED_cpi.dta") # Load data from Dropbox 

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
  geom_vline(aes(xintercept = 1996), linetype = "dotted", colour = "grey50") +
  annotate("text", x = 2001, y = 8.5, label = "FRB/Global", colour = "grey50") +
  geom_vline(aes(xintercept = 1975), linetype = "dotted", colour = "grey50") +
  annotate("text", x = 1978, y = 1.5, label = "SEM", colour = "grey50") +
  theme_bw(base_size = 12)

print(absInflation)
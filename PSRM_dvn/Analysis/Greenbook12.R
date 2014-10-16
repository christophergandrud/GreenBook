###############
# Propensity score distribution plots (Supplemental Materials Figure 2 & 3)
# Christopher Gandrud
# 14 October 2014
###############

# Load package
library(MatchIt)

#### Create plots ####
# Matched by election period
plot(cpi.matched.election, type = "jitter", interactive = FALSE)

# Matched by presidential party ID
plot(cpi.matched.party, type = "jitter", interactive = FALSE)

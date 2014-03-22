####################
# Greenbook Inflation Forecast Error by President Party Over Time
# Christopher Gandrud
# 26 March 2013
####################

# To run as a stand alone file. First, run the following file from the paper:
## source('Analysis/Greenbook1.R') 

## Create graph

# Partisan colours
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

errors.time <- ggplot2::ggplot(cpi.data, aes(x = Quarter, y = error.prop.deflator.q2)) +
                                geom_hline(yintercept = 0, size = 1,
                                           alpha = I(0.5)) +
                                geom_point(aes(color = pres_party_name)) +
                                stat_smooth(method = "lm", aes(group = presTerm, 
                                                               color = pres_party_name, 
                                                               fill = pres_party_name)) +
                                scale_color_manual(values = partisan.colors, name = "") +
                                scale_fill_manual(values = partisan.colors, name = "") +
                                xlab("") + ylab("Standardized Forecast Error\n") + 
                                scale_x_continuous(limits = c(1968, 2008),
                                                   breaks = c(1970, 1980, 1990, 2000, 2008), 
                                                   labels = c(1970, 1980, 1990, 2000, 2008)) +  
                                scale_y_continuous(breaks = c(-0.5, 0, 0.5), labels = c(-0.5, 0, 0.5)) +
                                guides(colour = guide_legend(reverse = TRUE), 
                                       fill = guide_legend(reverse = TRUE)) +
                                theme_bw(base_size = 12)

print(errors.time) 
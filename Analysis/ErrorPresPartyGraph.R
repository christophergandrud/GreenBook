####################
# Greenbook Inflation Forecast Error by President Party Over Time
# Christopher Gandrud
# 19 October 2012
####################


# library(ggplot2)

# To run as a stand alone file. First, run the following file from the paper:
## source_url("http://bit.ly/NXdCpk") 

## Create graph
# Error region +/- 10 percent
rect.time <- data.frame(xmin = 1968, xmax = 2006, ymin = -0.1, ymax = 0.1)

# Partisan colours
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

errors.time <- ggplot(cpi.data, aes(x = Quarter, y = error.prop.deflator.q2)) +
                      geom_point(aes(color = pres_party_name)) +
                      stat_smooth(method = "lm", aes(group = presTerm, color = pres_party_name, fill = pres_party_name)) +
                      geom_rect(data = rect.time, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "grey20", alpha = 0.5, linetype = 0, inherit.aes = FALSE) +
                      scale_color_manual(values = partisan.colors, name = "") +
                      scale_fill_manual(values = partisan.colors, name = "") +
                      xlab("") + ylab("Standardized Forecast Error\n") + 
                      scale_x_continuous(limits=c(1968, 2006)) +
                      theme_bw(base_size = 12)

print(errors.time) 
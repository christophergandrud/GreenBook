####################
# Greenbook Inflation Forecast Error by President Party Over Time
# Christopher Gandrud
# 1 August 2012
####################

# Clean up data and create forecast error variable
cpi.data$pres_party_name <- factor(cpi.data$pres_party, label = c("Rep", "Dem"))
cpi.data$error.prop.deflator.q2 <-  (cpi.data$GB_CPI_QTR2 - cpi.data$deflator)/cpi.data$deflator

# Create FRB/Global Model Variable 
cpi.data$GlobalModel[cpi.data$Quarter > 1995.4] <- "1"
cpi.data$GlobalModel[cpi.data$Quarter < 1996.1] <- "0"
cpi.data$GlobalModel <- factor(cpi.data$GlobalModel, labels = c("Before 1996", "After 1996"))  

# Create Fed Chair variable
cpi.data$Chair[cpi.data$Quarter > 1987.3] <-  "Greenspan"
cpi.data$Chair[cpi.data$Quarter <= 1987.3] <- "Volcker"
cpi.data$Chair[cpi.data$Quarter <= 1979.3] <- "Miller"
cpi.data$Chair[cpi.data$Quarter <= 1978.1] <- "Burns"
cpi.data$Chair[cpi.data$Quarter <= 1970.1] <- "Martin"
cpi.data$Chair <- factor(cpi.data$Chair)

## Remove 2 quarters from Johnson presidency
cpi.data <- subset(cpi.data, president != "Johnson")

cpi.data$president <- as.factor(cpi.data$presTerm)

## Create graph
# Error region +/- 10 percent
rect.time <- data.frame(xmin = 1968, xmax = 2006, ymin = -0.1, ymax = 0.1)

# Partisan colours
partisan.colors = c("Rep" = "#C42B00", "Dem" = "#2259B3")

errors.time <- ggplot(cpi.data, aes(x = Quarter, y = error.prop.deflator.q2)) +
  geom_point(aes(color = pres_party_name)) +
  stat_smooth(method = "lm", aes(group = presTerm, color = pres_party_name, fill = pres_party_name)) +
  geom_rect(data = rect.time, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "grey20", alpha = 0.5, inherit.aes = FALSE) +
  scale_color_manual(values = partisan.colors, name = "") +
  scale_fill_manual(values = partisan.colors, name = "") +
  xlab("") + ylab("Standardized Forecast Error") + 
  scale_x_continuous(limits=c(1968, 2006)) +
  theme_bw() +
  opts(axis.title.x = theme_text(size = 12, vjust = 0)) + 
  opts(axis.title.y = theme_text(angle = 90, size = 12, vjust = 0.3))
print(errors.time) 